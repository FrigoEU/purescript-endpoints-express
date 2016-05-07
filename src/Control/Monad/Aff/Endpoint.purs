module Control.Monad.Aff.Endpoint where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM.File.Types (File, Blob)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(Left), either)
import Data.Endpoint (Endpoint(Endpoint), FileUploadEndpoint(FileUploadEndpoint))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Global (encodeURI)
import Network.HTTP.Affjax (AJAX, affjax)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Prelude (return, (<>), ($), (>>>), (>>=), show, (<<<))
import Unsafe.Coerce (unsafeCoerce)

execEndpoint_ :: forall eff a b c. (EncodeJson a, EncodeJson b, DecodeJson c) =>
                  String -> Endpoint a b c -> a -> b -> Aff (ajax :: AJAX | eff) c
execEndpoint_ s (Endpoint {method: method, url: u}) a b = 
  affjax opts >>= _.response >>> parseOrThrow
    where opts = { method: Left method
                 , url: s <> u <> "?params=" <> (encodeURI <<< printJson <<< encodeJson) a
                 , headers: [ContentType applicationJSON]
                 , content: (Just $ show $ encodeJson b) :: Maybe String
                 , username: Nothing
                 , password: Nothing
                 , withCredentials: false}

-- relative path
execEndpoint :: forall eff a b c. (EncodeJson a, EncodeJson b, DecodeJson c) =>
                 Endpoint a b c -> a -> b -> Aff (ajax :: AJAX | eff) c
execEndpoint = execEndpoint_ ""

execFileUploadEndpoint :: forall eff a b . (EncodeJson a, DecodeJson b) =>
                             FileUploadEndpoint a b -> File -> a -> Aff (ajax :: AJAX | eff) b
execFileUploadEndpoint (FileUploadEndpoint {url: u}) file a = affjax opts >>= _.response >>> parseOrThrow
  where opts = { method: Left POST
               , url: u <> "?params=" <> (encodeURI <<< printJson <<< encodeJson) a
               , headers: []
               , content: Just $ fileToBlob file
               , username: Nothing
               , password: Nothing
               , withCredentials: false}

-- Atm affjax does nat define File as Requestable. I could also make a PR for Affjax, but this is easier...
fileToBlob :: File -> Blob
fileToBlob = unsafeCoerce

parseOrThrow :: forall eff a. (DecodeJson a) => String -> Aff eff a
parseOrThrow s = 
  either (\e -> throwError $ error $ "Failed to parse: " <> s) return (jsonParser s >>= decodeJson)
