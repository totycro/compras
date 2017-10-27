module App.View.LoggedIn where

import Prelude hiding (div)

import App.Events (Event(..))
import App.State
import App.Types
import Control.Bind (discard)
import Data.Function (($))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.List (List(..), (:), foldl)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Text.Smolder.HTML hiding (style)
import Text.Smolder.HTML.Attributes hiding (span)
import Text.Smolder.Markup ((#!), (!), text, class Attributable)
import Data.Foldable (for_)


getUserName :: Maybe User -> String
getUserName maybeUser = case maybeUser of
  Just (User user) -> user.name
  Nothing -> ""

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text ( "Bienvenid@ " <> (getUserName st.currentUser))
    div do
      button ! className "btn btn-primary" #! onClick (const RequestShoppingLists) $ text "load"
    centralView st.lists st.selectedList st.newItemName
    showAddNewList st.newListName

centralView :: RemoteData GenericLoadingError (List ShoppingList) -> Maybe ShoppingList -> String -> HTML Event
centralView shoppingLists (Just sl) newItemName = div do
   button
     ! className "btn btn-secondary mb-3"
     #! onClick (ShoppingListSelected Nothing)
     $ text "back"
   showSelectedList sl newItemName
centralView shoppingLists Nothing _ = showLists shoppingLists

-- TODO: datetime serialization
-- TODO: button for actually deleting?
-- TODO:
-- In a future version, have all lists be "selected", i.e. and in edit mode. then we'd need a component for this to be reusable with a state each
-- Alternatively, the overview in the main page could just be an overview, whereas on detail selection the actual items are loaded. this has the disadvantage of increased loading time

showAddNewList :: String -> HTML Event
showAddNewList newListName =
  div do
    h5 ! style "margin-top: 32px" $ text "Add new list"
    input ! value newListName #! onInput (\ev -> ChangeNewListName $ targetValue ev)
    (disableIfStringEmpty newListName button) #! onClick (const AddNewList) $ text "add"

disableIfStringEmpty :: forall ta. Attributable ta => String -> ta -> ta
disableIfStringEmpty "" x = x ! disabled "disabled"
disableIfStringEmpty _ x = x


createCheckbox :: Boolean -> HTML Event
createCheckbox value = input ! type' "checkbox" ! checked (if value then "checked" else "" )

showItem :: Item -> HTML Event
showItem (Item item) = li do
  text item.name
  createCheckbox item.bought #! onClick (const $ RequestToggleBoughtState item.id (not item.bought))

showLists :: RemoteData GenericLoadingError (List ShoppingList) -> HTML Event
showLists (Success listList) =
  div do
    span $ text "loaded"
    div $ for_ listList showListInOverview
showLists NotAsked = span $ text "Lists not loading yet"
showLists Loading = span $ text "Loading"
showLists (Failure e) = span $ text $ "Error while loading:" <> show e

showListInOverview :: ShoppingList -> HTML Event
showListInOverview (ShoppingList shoppingList) =
  div do
    a
      -- ! style "cursor: pointer"
      ! href "#"
      #! onClick (ShoppingListSelected $ Just $ ShoppingList shoppingList)
      $ text $ "list: " <> shoppingList.name


showSelectedList :: ShoppingList -> String -> HTML Event
showSelectedList (ShoppingList sl) newItemName = div do
  showList (ShoppingList sl)
  h5 $ text "Add new item to list"
  input ! value newItemName #! onChange (\ev -> ChangeNewItemName $ targetValue ev)
  (disableIfStringEmpty newItemName button ) #! onClick (const $ AddNewItem sl.id) $ text "add"

showList :: ShoppingList -> HTML Event
showList (ShoppingList shoppingList) =
  div ! className "card mb-3" $ do
     div ! className "card-body" $ do
       h3 ! className "card-title" $ text shoppingList.name
       ul $ for_ shoppingList.items showItem
