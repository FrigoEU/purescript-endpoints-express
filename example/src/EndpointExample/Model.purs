module EndpointExample.Model where

import Prelude (class Show, Unit)

import Data.Argonaut.Encode (class EncodeJson, gEncodeJson)
import Data.Argonaut.Decode (class DecodeJson, gDecodeJson)
import Data.Generic (class Generic, gShow)
import Data.Endpoint (Endpoint(Endpoint))
import Data.HTTP.Method (Method(GET))

----------------------------

data Order = Order { productId :: Int
                   , quantity :: Int }

derive instance genericOrder :: Generic Order

instance encodeJsonOrder :: EncodeJson Order where
  encodeJson = gEncodeJson
instance decodeJsonOrder :: DecodeJson Order where
  decodeJson = gDecodeJson
instance showOrder :: Show Order where
  show = gShow

getOrdersEndpoint :: Endpoint Int Unit (Array Order)
getOrdersEndpoint = Endpoint {url: "/getorders", method: GET}
