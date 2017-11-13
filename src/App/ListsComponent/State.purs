module App.ListsComponent.State where

import App.Types
import Data.List
newtype State = State
  { lists :: List ShoppingList
  , newListName :: String
  }


init :: List ShoppingList -> State
init lists = State
  { lists: lists
  , newListName: ""
  }
