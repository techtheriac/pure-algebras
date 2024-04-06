module Data.Domain.Person where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)

data Coordinates = Coordinates Int Int
instance showCoordinates :: Show Coordinates where
  show (Coordinates x y) = "Coordinates " <> show x <> " " <> show y
incrementX :: Coordinates -> Coordinates
incrementX (Coordinates x y) = Coordinates (x + 1) y
incrementY :: Coordinates -> Coordinates
incrementY (Coordinates x y) = Coordinates x (y + 1)


data DayOfWeek = WeekDay | WeekEnd
instance showDayOfWeek :: Show DayOfWeek where
  show WeekDay = "WeekDay"
  show _ = "WeekEnd"
whatToDo :: DayOfWeek -> String
whatToDo = case _ of 
  WeekDay -> "work"
  WeekEnd -> "sleep"

newtype Id = ValidatedId String
derive instance newTypeId :: Newtype Id _
derive newtype instance showId :: Show Id -- uses the show instance defined on `String`
makeId :: String -> Maybe Id
makeId "" = Nothing
makeId s = Just $ wrap s

holed :: Int -> String 
holed x = show x

someValue2 :: String
someValue2 =
  """
  The "?name" syntax above is a named typed wildcard.
  It will produce a compiler error whose message will include
  the compiler's guess as to which type goes there.
  """