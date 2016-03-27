module Data.Serializable where

import Prelude (Unit, (<$>), (<>), ($), unit, (==), return, bind, id, (<<<))

import Control.Monad.Eff.Exception (Error, error)

import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Either (Either(Right, Left))
import Data.Date (Date, fromStringStrict, toISOString)
import Data.Tuple (Tuple(Tuple))
import Data.String (split, joinWith)
import Data.Traversable (traverse)

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> Either Error a

foreign import parseFloatImpl :: forall a b. (a -> Maybe a) -> Maybe a -> b -> Maybe Number
parseFloat :: forall a. a -> Maybe Number
parseFloat = parseFloatImpl Just Nothing
foreign import parseIntImpl :: forall a b. (a -> Maybe a) -> Maybe a -> b -> Maybe Int
parseInt :: forall a. a -> Maybe Int
parseInt = parseIntImpl Just Nothing
foreign import parseBoolImpl :: forall a b. (a -> Maybe a) -> Maybe a -> b -> Maybe Boolean
parseBool :: forall a. a -> Maybe Boolean
parseBool = parseBoolImpl Just Nothing

foreign import toString :: forall a. a -> String

instance serializableInt :: Serializable Int where
  serialize = toString 
  deserialize a = maybe (Left $ error $ "Unable to deserialize " <> a <> " to Int") 
                        Right 
                        (parseInt a)

instance serializableNumber :: Serializable Number where
  serialize = toString 
  deserialize a = maybe (Left $ error $ "Unable to deserialize " <> a <> " to Number")
                        Right 
                        (parseFloat a)

instance serializableString :: Serializable String where
  serialize = id
  deserialize = Right <<< id

instance serializableBoolean :: Serializable Boolean where
  serialize = toString
  deserialize a = maybe (Left $ error $ "Unable to deserialize " <> a <> " to Boolean") 
                        Right 
                        (parseBool a)

instance serializableDate :: Serializable Date where
  serialize = toISOString
  deserialize a = maybe (Left $ error $ "Unable to deserialize " <> a <> " to Date") 
                        Right 
                        (fromStringStrict a)

foreign import tupleRegexImpl :: forall a b c. (b -> c -> Tuple b c) -> 
                                               (a -> Maybe a) -> 
                                               Maybe a -> 
                                               String -> Maybe (Tuple String String)
instance serializableTuple :: (Serializable a, Serializable b) => Serializable (Tuple a b) where 
  serialize (Tuple a b) = "(" <> serialize a <> "," <> serialize b <> ")"
  deserialize s = do
    (Tuple a b) <- maybe (Left $ error $ "Unable to deserialize " <> s <> " to Tuple")
                         Right
                         (tupleRegexImpl Tuple Just Nothing s)
    justa <- deserialize a
    justb <- deserialize b
    return $ Tuple justa justb

instance serializableUnit :: Serializable Unit where
  serialize s = "unit"
  deserialize s = if s == "unit" then Right unit
                                 else Left $ error $ "Unable to deserialize " <> s <> " to Unit"

instance serializableArray :: (Serializable a) => Serializable (Array a) where
  serialize arr = joinWith ";" (serialize <$> arr)
  deserialize s = traverse deserialize (split ";" s)

