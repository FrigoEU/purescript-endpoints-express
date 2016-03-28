module EndpointExample.Client where

import Data.Argonaut.Encode 
import Data.Argonaut.Decode
import Data.Generic

data Order = Order { productId :: Int
                   , quantity :: Int }

derive instance genericOrder :: Generic Order

instance encodeJsonOrder :: EncodeJson Order where
  encodeJson = gEncodeJson
