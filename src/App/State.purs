module App.State where

import Prelude

import Data.Argonaut
import Data.Traversable

import App.Config (config)
import App.Routes (Route, match)
import App.Types (ShoppingList(..), User, RemoteData(..), GenericLoadingError(..), ShoppingListId, remoteDataToMaybe)

import Data.List (List(..), (:), fromFoldable, head, filter)
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
  , selectedListId :: Maybe ShoppingListId
  , newListName :: String
  , newItemName :: String
  }


getSelectedList :: State -> Maybe ShoppingList
getSelectedList (State st) = do
  slId <- st.selectedListId
  lists <- (remoteDataToMaybe st.lists)
  selectedList <- filterLists lists slId
  pure selectedList
  where
    filterLists :: List ShoppingList -> ShoppingListId -> Maybe ShoppingList
    filterLists lists listId = head $ filter (\(ShoppingList sl) -> sl.id == listId) lists


init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , currentUser: Nothing
  , lists: NotAsked
  , selectedListId: Nothing
  , newListName: ""
  , newItemName: ""
  }

