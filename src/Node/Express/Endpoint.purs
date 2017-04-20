module Node.Express.Endpoint (
  makeApp, listen, hostEndpoint, hostFile, hostFileUploadEndpoint, Input(), Handler(), App(), EXPRESS(),
  hostStatic, compression, Middleware
) where

import Control.Apply ((*>))
import Control.Bind ((>=>))
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, message, throw, throwException)
import Control.Monad.Error.Class (throwError, class MonadError)
import DOM.File.Types (Blob)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Endpoint (Endpoint(Endpoint), FileUploadEndpoint(FileUploadEndpoint))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.StrMap (StrMap)
import Data.String (take)
import Global (decodeURI)
import Node.Buffer (Buffer)
import Prelude (Unit, bind, pure, show, unit, ($), (/=), (<<<), (<>), (>>=), (>>>), discard)
import Unsafe.Coerce (unsafeCoerce)

foreign import data App :: Type
foreign import data EXPRESS :: Effect
foreign import data Request :: Type
foreign import data Response :: Type
foreign import data Middleware :: Type

foreign import makeApp :: forall eff. Array Middleware -> Eff (express :: EXPRESS | eff) App
foreign import listen :: forall eff. App -> Int -> Eff (express :: EXPRESS | eff) Unit
foreign import get :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import delete :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import post :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import put :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import sendStr  :: forall eff. Response -> String -> Eff (express :: EXPRESS | eff) Unit
foreign import setStatus  :: forall eff. Response -> Int -> Eff (express :: EXPRESS | eff) Unit
foreign import sendBuffer :: forall eff. Response -> Buffer -> Eff (express :: EXPRESS | eff) Unit
foreign import hostStatic :: forall eff. App -> String -> Eff (express :: EXPRESS | eff) Unit
foreign import jsonParserMW :: Middleware
foreign import bufferParserMW :: Middleware
foreign import noParserMW :: Middleware
foreign import rawParserMW :: Middleware
foreign import compression :: Middleware

type Handler eff a b c = a -> Input b -> Aff eff c

type Input a = { url :: String
               , body :: a
               , params :: StrMap String
               , path :: String
               , query :: StrMap String
               , headers :: StrMap String
               }

mapInput :: forall a b c. (b -> c) -> { body :: b | a} -> { body :: c | a}
mapInput f i@{body} = i {body = f body}

hostEndpoint :: forall qp body ret eff. (DecodeJson qp) => (DecodeJson body) => (EncodeJson ret) =>
                  App
                  -> Endpoint qp body ret
                  -> Handler (express :: EXPRESS, console :: CONSOLE, exception :: EXCEPTION | eff) qp body ret
                  -> Eff (express :: EXPRESS, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
hostEndpoint app (Endpoint {method, url}) h = do
  checks
  case method of
       GET -> get app url noParserMW handler
       POST -> post app url rawParserMW handler
       PUT -> put app url rawParserMW handler
       DELETE -> delete app url noParserMW handler
       _ -> pure unit
  where
    checks = if take 1 url /= "/" then throw "Url must start with /" else pure unit
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint on " <> url <> " " <> message err
                                 setStatus res 500
                                 sendStr res $ message err)
                             (\a -> sendStr res $ show $ encodeJson a)
                             (do let i = convert req
                                 qp <- parseQueryParams i
                                 body <- parseBody i
                                 h qp body) *> pure unit

hostFileUploadEndpoint :: forall eff qp body. (DecodeJson qp) => (EncodeJson body) =>
                      App
                      -> FileUploadEndpoint qp body
                      -> Handler (express :: EXPRESS, console :: CONSOLE | eff) qp Buffer body
                      -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFileUploadEndpoint app (FileUploadEndpoint {url}) h = post app url bufferParserMW handler
  where handler req res = runAff (\err -> do
                                     log $ "Failed hostFileUploadEndpoint on " <> url <> message err
                                     setStatus res 500
                                     sendStr res $ message err)
                                 (\a -> sendStr res $ show $ encodeJson a)
                                 (let i = convertBlob req
                                   in parseQueryParams i >>= \qp -> h qp (mapInput blobToBuffer i)) *> pure unit

blobToBuffer :: Blob -> Buffer
blobToBuffer = unsafeCoerce

parseBody :: forall a m. (DecodeJson a) => (MonadError Error m) => Input String -> m (Input a)
parseBody a = either (\err -> throwError $ error err)
                                (\p -> pure $ a { body = p})
                                (jsonParser a.body >>= decodeJson)

foreign import getParamsImpl :: forall a. (a -> Maybe a) -> Maybe a -> String -> Maybe String
getParams :: String -> Maybe String
getParams = getParamsImpl Just Nothing

parseQueryParams :: forall a b m. (DecodeJson b) => (MonadError Error m) => Input a -> m b
parseQueryParams {url} = maybe (throwError $ error $ "No params found")
                                       (\p -> either (throwError <<< error) pure $ (decodeURI >>> jsonParser >=> decodeJson) p)
                                       (getParams url)

hostFile :: forall eff.
              App
              -> String
              -> (Input Unit -> Aff (express :: EXPRESS, console :: CONSOLE | eff) Buffer)
              -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFile app url f = get app url noParserMW handler
  where
    handler req res = runAff (\err -> do
                               log $ "Failed hostFile " <> message err
                               setStatus res 500
                               sendStr res $ message err)
                             (\a -> sendBuffer res a)
                             (parseBody (convert req) >>= f) *> pure unit

convert :: Request -> Input String
convert = mkConvert (\a b c d e f -> {url: a, body: b, params: c, path: d, query: e, headers: f})
                    (show $ encodeJson unit)

convertBlob :: Request -> Input Blob
convertBlob = mkBufferConvert (\a b c d e f -> {url: a, body: b, params: c, path: d, query: e, headers: f})

foreign import mkConvert ::
  forall a. (String -> a -> StrMap String -> String -> StrMap String -> StrMap String -> Input a)
            -> String -> Request -> Input a
foreign import mkBufferConvert ::
  (String -> Blob -> StrMap String -> String -> StrMap String -> StrMap String -> Input Blob)
            -> Request -> Input Blob
