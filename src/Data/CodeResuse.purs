module Data.CodeResuse where

import Prelude

import Data.List (List(..), (:))


-- functions in this module goes from concrete implementations to more generic ones
-- demonstrating the power of polymorphism

-- Note on Recursive data structures
-- An operation defined to execute on the first element of a list
-- can be called recursively on the rest of the list
-- Think of it as defining a base case that runs n times
-- n being the tail of the list or recursive data structrure

sayHello :: List String -> List String
sayHello Nil = Nil
sayHello (name : names) = 
-- base case            recurse
  ("Hello, " <> name) : sayHello names


prependAll :: String -> List String -> List String
prependAll _ Nil = Nil
prependAll prefix (name : names) = 
  (prefix <> name) : prependAll prefix names


sayHello' :: List String -> List String
sayHello' = prependAll "Hello, "

sayGoodbye :: List String -> List String
sayGoodbye = prependAll "Goodbye, "

-- transform :: (String -> String) -> List String -> List String
-- transform _ Nil = Nil
-- transform editFunc Nil = Nil
-- trasform editFunc (name : names) =
--   (editFunc name) : transform editFunc names