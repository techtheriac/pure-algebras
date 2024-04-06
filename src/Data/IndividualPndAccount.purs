module Data.IndividualPndAccount
  ( IndividualPndAccount
  , PndReason(..)
  )
  where

data PndReason a = BvnOutstanding a |  ReactivationViaInflow | AccountHolderDeceased
type IndividualPndAccount = 
  { cif :: String
  , accountNumber :: String
  , accountName :: String
  , schemeCode :: String
  , schemecodeDescription :: String
  , balanceOnFreezeData :: Number
  , pndReason :: PndReason Int
  }


-- SetNextNotificationState :: IndividualPndAccount -> IndividualPndAccount

type Address = 
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

data Person = Person 
  { name :: String
  , age :: Int
  , address :: Address 
  }

data Company = Company 
  { name :: String 
  , address :: Address
  }

data Residence = Home Address | Facility Address

data Directions = North | East

