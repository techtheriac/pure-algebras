module Data.Adts
  ( Color(..)
  , Genre(..)
  , divide
  , f
  , f'
  , fumber
  , getColorFromGenre
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- implementing function with the following definition
-- f(x) = (83 ÷ x) × 10

divide :: ∀ a. Eq a => EuclideanRing a => a -> a -> Maybe a
divide x y 
  | y == zero = Nothing
  | otherwise = Just $ x / y
f :: Number -> Maybe Number
f x = case divide 83.0 x of
  Nothing -> Nothing
  Just result -> Just $ result * 10.0

divide' :: ∀ a. Eq a => EuclideanRing a => a -> a -> Either String a
divide' x y 
  | y == zero = Left "Divide by zero"
  | otherwise = Right $ x / y

f' :: Number -> Either String Number 
f' x = case divide' 83.0 x of 
  Left reason -> Left reason
  Right result -> Right $ result * 10.0

fumber :: Number -> Number
fumber x = 10.0 * case divide' 83.0 x of
  Left _ -> 0.0
  Right result -> result

newtype Color = Color String
derive newtype instance showColor :: Show Color
data Genre = Musing | Poetry | Engineering 
getColorFromGenre :: Genre -> Color
getColorFromGenre x = case x of
  Musing -> Color "#FF9200"
  Poetry -> Color "#87A19E"
  Engineering -> Color "#87A190"

-- to implement an instance of a typeclass, it folows the structure:
-- instance arbitrary_name  :: typeclass_name concrete_type_for_which_typeclass_is_being_implemented where 
-- function = implementation


-- Conjuring an either, but there's a challange
-- functors only have one type parameter i.e
-- class Functor a where 
-- map :: f -> (a -> b) -> (f a -> f b)
-- but we can use partial application to overcome this hurdle
data Either' a b = Left' a | Right' b 

instance functorEither' :: Functor (Either' a) where
  map _ (Left' x) = Left' x
  map g (Right' y) = Right' $ g y

class Bifunctor' f where
  bimap' :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

instance bifunctorEither' :: Bifunctor' Either' where
  bimap' g _ (Left' x) = Left' $ g x
  bimap' _ h (Right' y) = Right' $ h y

-- creating a bifunctor instace for a product type
-- we define a data type `Tuple a b`` (having a constructor with two type params a b respectively)
data Tuple' a b = Tuple' a b
instance bifucntorTuple' :: Bifunctor' Tuple' where
  bimap' g h (Tuple' a b) = Tuple' (g a) (h b)