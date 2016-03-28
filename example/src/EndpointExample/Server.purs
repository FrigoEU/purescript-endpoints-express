module EndpointExample.Server where

import Prelude (Unit, (==), bind, return, ($))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Network.HTTP.Affjax (AJAX)

import Data.Array (filter)

import EndpointExample.Model (Order(Order), getOrdersEndpoint)

import Node.Express.Endpoint (EXPRESS, listen, hostStatic, hostEndpoint, makeApp)

----------------------------

myOrders :: Array Order
myOrders = [Order {productId: 1, quantity: 50}, Order {productId: 2, quantity: 6}]

main :: forall eff. Eff ( console :: CONSOLE, express :: EXPRESS, ajax :: AJAX | eff ) Unit
main = do
  app <- makeApp
  hostEndpoint app getOrdersEndpoint (\productId _ -> return $ filterOrders productId)
  hostStatic app "static"
  listen app 8080
  log "Listening on port 8080"

filterOrders :: Int -> Array Order
filterOrders id = filter (\(Order {productId}) -> productId == id) myOrders
