module Data.Order where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal', traversed)
import Data.Show.Generic (genericShow)
import Type.Prelude (Proxy(..))

type Order = { drinks :: Array Highball }
type Highball = { liquor :: Liquor, mixer :: Mixer, ounces :: Int }
data Liquor = Scotch | Gin
data Mixer = Soda | Tonic


derive instance Generic Liquor _
instance Show Liquor where
  show = genericShow

derive instance Generic Mixer _
instance Show Mixer where
  show = genericShow

-- A lens deconstructs product types such as Records and Tuples
-- A lens must always focus on the value
drinksLens :: Lens' Order (Array Highball)
drinksLens = prop (Proxy :: _ "drinks")

liquorLens :: Lens' Highball Liquor
liquorLens = prop (Proxy :: _ "liquor")

-- composing lenses
orderedLiquor :: Traversal' Order Liquor
orderedLiquor = drinksLens <<< traversed <<< liquorLens

orderedOunces :: Traversal' Order Int
orderedOunces = drinksLens <<< traversed <<< prop (Proxy :: _ "ounces")

myOrder :: Order 
myOrder = { drinks : [
  { liquor: Scotch, mixer: Soda, ounces: 22 },
  { liquor: Gin, mixer: Soda, ounces: 22 }
  ] }

increaseOunce :: Order -> Order
increaseOunce x = over orderedOunces (_ + 1) x
