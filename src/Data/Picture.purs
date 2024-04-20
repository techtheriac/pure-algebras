module Data.Picture where

import Prelude

import Control.Apply (lift2, lift3)
import Data.Maybe (Maybe(..))

gcd ∷ Int → Int → Int
gcd n 0 = n
gcd 0 m = m 
gcd n m = if n > m
            then gcd (n - m) m
            else gcd n (m - n)

fromString :: String -> Boolean
fromString "true" = true
fromString _ = false

toString :: Boolean -> String
toString true = "true"
toString false = "false"

factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 1 
factorial x = x * factorial (x - 1)

isEmpty :: ∀ a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

showPerson :: { firstName :: String, lastName :: String } -> String
showPerson { firstName: x, lastName: y } = y <> " " <> x

-- using record puns
showPersonV2 :: { firstName :: String, lastName :: String } -> String
showPersonV2 { firstName, lastName } = firstName <> " " <> lastName

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address : { city : "Los Angeles "} } = true
livesInLA _ = false

-- named Patterns 
sortPair :: Array Int -> Array Int
sortPair arr@[x, y] 
  | x <= y  = arr 
  | otherwise = [y, x]
sortPair arr = arr

sameCity :: Person -> Person -> Boolean 
sameCity { address : { city : x} } { address : { city : y } } = x == y

sameCity' ∷ ∀ (t35 ∷ Row Type) (t38 ∷ Row Type) (t41 ∷ Row Type) (t44 ∷ Row Type) (a46 ∷ Type). Eq a46 ⇒ { address ∷ { city :: a46 | t38 } | t35 } → { address ∷ { city :: a46 | t44 } | t41 } → Boolean
sameCity' { address : { city : x} } { address : { city : y } } = x == y

fromSingleton :: ∀ a. Array a -> Maybe a
fromSingleton [x] = Just x
fromSingleton _ = Nothing 


data Shape
  = Circle Point Number 
  | Rectangle Point Number Number
  | Line Point Point 
  | Text Point String

type Point = 
  { x :: Number
  , y :: Number 
  }

exampleLine :: Shape 
exampleLine = Line p1 p2
  where 
    p1 :: Point 
    p1 = { x: 0.0, y: 0.0}

    p2 :: Point 
    p2 = { x: 100.0, y: 50.0 }

showShape :: Shape -> String
showShape (Circle c r) = 
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"

showPoint :: Point -> String 
showPoint {x, y} = "(" <> show x <> ", " <> show y <> ")"

circleAtOrigin :: Number -> Shape
circleAtOrigin radius = Circle point radius
  where 
    point :: Point
    point = {x : 0.0, y: 0.0}

doubleScalarAtCenter :: Shape -> Shape
doubleScalarAtCenter shape = 
    case shape of
    (Circle c r) -> Circle (center c) (2.0 * r)
    (Rectangle c w h) -> Rectangle (center c) (w * 2.0) (h * 2.0)
    (Line point1 point2) -> Line (center point1) (center point2)
    (Text loc text) -> Text (center loc) text
  where 
    center :: Point -> Point 
    center _ = { x : 0.0, y : 0.0 }

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

newtype Point' 
  = Point' 
  { x :: Number 
  , y :: Number
  }

-- delegates show implementation to compiler
derive newtype instance showPoint' :: Show Point'

-- 💡 Ideally, a value object should implement the Eq type class

newtype Complex
  = Complex 
  { real :: Number 
  , imaginary :: Number 
  }

-- (i) => an arbitrary instace name
-- (ii) => type class whose function's behaviour should be overrided
-- (iii) => user defined type 
--       (i)            (ii) (iii)
instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = show real <> "+" <> show imaginary <> "i"


type Address' = { street :: String, city :: String, state :: String }
address :: String -> String -> String -> Address'
address street city state = { street, city, state}

-- class Functor f where
--   map :: forall a b. (a -> b) -> f a -> f b

-- class Functor f <= Apply f where
--   apply :: forall a b. f (a -> b) -> f a -> f b

-- Apply is a subclass of Functor which implements an additional 
-- function `apply` 
-- A superclass is usually a less specific specification of behaviours
-- For instance - a monoid is a subclass of a semigroup becuase a monoid 
-- Is first a `Semigroup` with but with an additional Identity element

-- see - https://github.com/fantasyland/fantasy-land

-- Relationship bewteen map and apply
-- Given that a function with multiple arguments is supplied to map
-- where map is defined as map :: f -> a -> fa
-- A function with two arguments must be curried such that
-- the following function f(x, y) -> z is x -> (y -> z) 
-- when this function is applied by map to a functor
-- we have a new data structure of functions waiting to be evaluated 
-- to the simplest irreducible form. 
-- the function apply from typeclass `Apply` can traverse this functor
-- and squeeze out an irreducible expression or value as the case may be.
-- Hence, the expression 
-- apply (map g x) y === g <$> x <*> y where <$> is map & <*> is apply

-- map -> wrap into context
-- apply -> operate on primitives within context
-- pure -> back into context

add' :: ∀ a. Semiring a => a -> a -> a
add' x y = x + y

add3 :: ∀ a. Semiring a => a -> a -> a -> a
add3 x y z = x + y + z

addMaybe ∷ ∀ a . Semiring a => Maybe a -> Maybe a -> Maybe a  
addMaybe = lift2 add' 

addMaybe' ∷ ∀ a . Semiring a => Maybe a -> Maybe a -> Maybe a -> Maybe a  
addMaybe' = lift3 add3