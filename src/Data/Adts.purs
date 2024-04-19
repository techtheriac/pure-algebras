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