module Data.Endpoint where

import Data.HTTP.Method (Method())

data Endpoint qp body ret = Endpoint {
  method :: Method,
  url :: String
}

data FileUploadEndpoint qp body = FileUploadEndpoint {
  url :: String
}
