module App.State where

import Prelude

import App.Config (config)
import App.Routes
import App.Types

import Data.List (List(..), (:), fromFoldable, head, filter)
import Data.Maybe (Maybe(..))
import App.ListsComponent as ListsComponent


newtype State = State
  { title :: String
  , route :: MainRoute
  , lists :: RemoteData GenericLoadingError (List ShoppingList)
  , newListName :: String
  , newItemName :: String
  , currentUser :: Maybe User
  , lists' :: RemoteData GenericLoadingError ListsComponent.State
  , error :: String
  }


getSelectedList :: State -> Maybe ShoppingList
getSelectedList (State st) = do
  slId <- case st.route of
    LoggedIn (Detail slId) -> Just slId
    _ -> Nothing
  lists <- (remoteDataToMaybe st.lists)
  selectedList <- filterLists lists slId
  pure selectedList
  where
    filterLists :: List ShoppingList -> ShoppingListId -> Maybe ShoppingList
    filterLists lists listId = head $ filter (\(ShoppingList sl) -> sl.id == listId) lists


init :: String -> State
init url = State
  { title: config.title
  , route: Home
  , lists: NotAsked
  , newListName: ""
  , newItemName: ""
  , currentUser: Nothing
  , lists': NotAsked
  , error: ""
  }
