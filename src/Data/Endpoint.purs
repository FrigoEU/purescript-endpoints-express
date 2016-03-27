module Data.Endpoint where

import Data.HTTP.Method (Method())

data Endpoint a b c = Endpoint {
  method :: Method,
  url :: String
}

data FileUploadEndpoint a b = FileUploadEndpoint {
  url :: String
}
