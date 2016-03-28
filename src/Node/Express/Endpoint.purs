module Node.Express.Endpoint (
  makeApp, listen, hostEndpoint, hostFile, hostFileUploadEndpoint, Input(), Handler(), App(), EXPRESS(),
  hostStatic
) where

import Prelude 

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Aff (runAff, Aff())
import Control.Monad.Eff.Exception (message, error, Error)
import Control.Monad.Error.Class (throwError, class MonadError)

import Data.StrMap (StrMap)
import Data.Either (Either(Left), either)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser as P
import Data.Serializable (class Serializable, deserialize)
import Data.Maybe (Maybe(Nothing, Just), maybe)

import DOM.File.Types (Blob())
import Unsafe.Coerce (unsafeCoerce)

import Data.HTTP.Method (Method(..))

import Node.Buffer (Buffer())

import Data.Endpoint (Endpoint(Endpoint), FileUploadEndpoint(FileUploadEndpoint))

foreign import data App :: *
foreign import data EXPRESS :: !
foreign import data Request :: *
foreign import data Response :: *
foreign import data Middleware :: *

foreign import makeApp :: forall eff. Eff (express :: EXPRESS | eff) App
foreign import listen :: forall eff. App -> Int -> Eff (express :: EXPRESS | eff) Unit
foreign import get :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import delete :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import post :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import put :: forall eff. App -> String -> Middleware -> (Request -> Response -> (Eff (express :: EXPRESS | eff) Unit)) -> Eff (express :: EXPRESS | eff) Unit
foreign import sendStr  :: forall eff. Response -> String -> Eff (express :: EXPRESS | eff) Unit
foreign import sendBuffer :: forall eff. Response -> Buffer -> Eff (express :: EXPRESS | eff) Unit
foreign import hostStatic :: forall eff. App -> String -> Eff (express :: EXPRESS | eff) Unit
foreign import jsonParser :: Middleware
foreign import bufferParser :: Middleware
foreign import noParser :: Middleware
foreign import rawParser :: Middleware

type Handler eff a b c = a -> Input b -> Aff eff c

type Input a = { url :: String
               , body :: a
               , params :: StrMap String
               , path :: String 
               , query :: StrMap String
               , headers :: StrMap String
               }

mapBody :: forall a b. (a -> b) -> Input a -> Input b
mapBody f (i@{body}) = i {body = f body}

hostEndpoint :: forall a b c eff. (Serializable a, DecodeJson b, EncodeJson c) =>
                  App -> Endpoint a b c -> Handler (express :: EXPRESS, console :: CONSOLE | eff) a b c
                  -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostEndpoint app (Endpoint {method, url}) h = 
  case method of
       GET -> register get noParser
       POST -> register post rawParser
       PUT -> register put rawParser
       DELETE -> register delete noParser
  where
    register f parser = f app url parser handler 
    handler req res = runAff (\err -> do
                                 log $ "Failed hostEndpoint on " <> url <> message err
                                 sendStr res $ message err)
                             (\a -> sendStr res $ show $ encodeJson a) 
                             (do let i = convert req
                                 qp <- parseQueryParams i
                                 body <- parseBody i
                                 h qp body)

hostFileUploadEndpoint :: forall eff a b. (Serializable a, EncodeJson b) =>
                      App 
                      -> FileUploadEndpoint a b 
                      -> Handler (express :: EXPRESS, console :: CONSOLE | eff) a Buffer b
                      -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFileUploadEndpoint app (FileUploadEndpoint {url}) h = post app url bufferParser handler
  where handler req res = runAff (\err -> do
                                     log $ "Failed hostFileUploadEndpoint on " <> url <> message err
                                     sendStr res $ message err)
                                 (\a -> sendStr res $ show $ encodeJson a) 
                                 (let i = convertBlob req
                                   in parseQueryParams i >>= \qp -> h qp (mapBody blobToBuffer i))

blobToBuffer :: Blob -> Buffer
blobToBuffer = unsafeCoerce

parseBody :: forall a m. (DecodeJson a, MonadError Error m) => Input String -> m (Input a)
parseBody a = either (\err -> throwError $ error err)
                            (\p -> return $ a { body = p})
                            (P.jsonParser a.body >>= decodeJson)

foreign import getParamsImpl :: forall a. (a -> Maybe a) -> Maybe a -> String -> Maybe String
getParams :: String -> Maybe String
getParams = getParamsImpl Just Nothing

parseQueryParams :: forall a b m. (Serializable b, MonadError Error m) => Input a -> m b
parseQueryParams {url} = either throwError
                                return
                                (maybe (Left $ error $ "No params found") 
                                       (\s -> deserialize s)
                                       (getParams url))

hostFile :: forall eff. 
              App 
              -> String 
              -> (Input Unit -> Aff (express :: EXPRESS, console :: CONSOLE | eff) Buffer) 
              -> Eff (express :: EXPRESS, console :: CONSOLE | eff) Unit
hostFile app url f = get app url noParser handler
  where 
    handler req res = runAff (\err -> do
                               log $ "Failed hostFile " <> message err
                               sendStr res $ message err)
                             (\a -> sendBuffer res a)
                             (parseBody (convert req) >>= f)

convert :: Request -> Input String
convert = mkConvert {url: _, body: _, params: _, path: _, query: _, headers: _}
                    (show $ encodeJson unit)

convertBlob :: Request -> Input Blob
convertBlob = mkBufferConvert {url: _, body: _, params: _, path: _, query: _, headers: _}

foreign import mkConvert :: 
  forall a. (String -> a -> StrMap String -> String -> StrMap String -> StrMap String -> Input a) 
            -> String -> Request -> Input a
foreign import mkBufferConvert :: 
  (String -> Blob -> StrMap String -> String -> StrMap String -> StrMap String -> Input Blob) 
            -> Request -> Input Blob