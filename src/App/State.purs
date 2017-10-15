module App.State where

import Prelude

import App.Config (config)
import App.Routes (Route, match)
--import Data.Newtype (class Newtype)

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.DateTime (DateTime(..))
import Data.Date (canonicalDate,  Month(..))
import Data.Time (Time(..))

import Partial.Unsafe (unsafePartial)
import Data.Enum (toEnum)




data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a

data GenericLoadingError
  = Err String

newtype User = User
  { name :: String
  }

newtype Shop = Shop
  { name :: String
  }

newtype Item = Item
  { name :: String
  , addedAt :: DateTime
  , dueAt :: Maybe DateTime
  , addedBy :: User
  , buyAt :: List Shop
  }

newtype ShoppingList = ShoppingList (List Item)

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , currentUser :: Maybe User
  , lists :: RemoteData GenericLoadingError (List ShoppingList)
  }

--derive instance newtypeState :: Newtype State _

testList :: ShoppingList
testList = 
  ShoppingList (
    Item 
    { name: "Zeug"
    , addedAt: DateTime someDay someTime
    , dueAt: Nothing
    , addedBy: User { name: "test" }
    , buyAt: Nil
    } : Nil
  )
  where
    someDay = unsafePartial fromJust $ canonicalDate <$> toEnum 2017 <*> (Just September) <*> toEnum 4
    someTime = unsafePartial $ fromJust $ Time <$> toEnum 17 <*> toEnum 2 <*> toEnum 16 <*> toEnum 362

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , currentUser: Nothing
  , lists: Success (testList : Nil)
  }

