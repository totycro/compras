module App.ListsComponent.Overview where

import Prelude hiding (div)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML hiding (map)
import Text.Smolder.HTML.Attributes as Attributes
import Text.Smolder.Markup ((#!), (!), text)
import Data.Foldable (for_)
import Data.List (length)

import App.ListsComponent.Events (Event(..))
import App.ListsComponent.State (State(..))

import App.Types (ShoppingList(..))
import App.View.Utils


view :: State -> HTML Event
view (State st) = div do
  ul ! Attributes.className "list-unstyled p-2" $ do
    for_ st.lists showListInOverview
  showAddNewList st.newListName


showAddNewList :: String -> HTML Event
showAddNewList newListName = div $ do
  h5 ! Attributes.className "mt-4" $ text "Añadir nueva list"
  form ! Attributes.className "form-inline" $ do
    div ! Attributes.className "form-group" $ do
      input
        ! Attributes.className "form-control"
        ! Attributes.value newListName
        #! onInput (\ev -> ChangeNewListName $ targetValue ev)
      (disableIfStringEmpty newListName button)
        ! Attributes.className "btn btn-primary ml-1"
        #! onClick (const AddNewList)
        $ text "añadir"


showListInOverview :: ShoppingList -> HTML Event
showListInOverview (ShoppingList shoppingList) =
  li ! Attributes.className "my-2" $do
    button
      ! Attributes.className "btn btn-info"
      -- #! onClick (const $ PageView $ LoggedIn $ Detail shoppingList.id) -- TODO
      $ do
        span
          $ text ("lista: " <> shoppingList.name)
        span
          ! Attributes.className "badge badge-light ml-1"
          $ text (show (length shoppingList.items))
