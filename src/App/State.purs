module App.State where

import Prelude

import Data.Argonaut
import Data.Traversable

import App.Config (config)
import App.Routes (Route, match)
import App.Types (ShoppingList(..), User, RemoteData(..), GenericLoadingError(..))

import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.DateTime (DateTime(..))
import Data.Date (canonicalDate,  Month(..))
import Data.Time (Time(..))
import Data.Either (Either(..), either)

import Partial.Unsafe (unsafePartial)
import Data.Enum (toEnum)

newtype State = State
  { title :: String
  , route :: Route
  , currentUser :: Maybe User
  , lists :: RemoteData GenericLoadingError (List ShoppingList)
  , selectedList :: Maybe ShoppingList
  , newListName :: String
  , newItemName :: String
  }


init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , currentUser: Nothing
  --, lists: Success (testList : Nil)
  , lists: NotAsked
  , selectedList: Nothing
  , newListName: ""
  , newItemName: ""
  }

