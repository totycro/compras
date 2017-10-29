module App.View.LoggedIn where

import App.Routes
import App.State
import App.Types
import Text.Smolder.HTML

import App.Events (Event(..))
import CSS (offset)
import Data.Foldable (for_)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Prelude hiding (div)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML.Attributes as Attributes
import Text.Smolder.Markup ((#!), (!), text, class Attributable)


getUserName :: Maybe User -> String
getUserName maybeUser = case maybeUser of
  Just (User user) -> user.name
  Nothing -> ""

overviewView :: State -> HTML Event
overviewView (State st) =
  div do
    h1 $ text ( "Bienvenid@ " <> (getUserName st.currentUser))
    showLists st.lists
    showAddNewList st.newListName

detailView :: State -> HTML Event
detailView (State st) = div $ do
  button
    ! Attributes.className "btn btn-secondary mb-3"
    #! onClick (const $ PageView (LoggedIn Overview))
    $ text "back"
  case (getSelectedList (State st)) of
    Just sl -> showSelectedList sl st.newItemName
    Nothing -> span $ text "Error loading shopping list"


-- TODO: datetime serialization
-- TODO: button for actually deleting?
-- TODO:
-- In a future version, have all lists be "selected", i.e. and in edit mode. then we'd need a component for this to be reusable with a state each
-- Alternatively, the overview in the main page could just be an overview, whereas on detail selection the actual items are loaded. this has the disadvantage of increased loading time

showAddNewList :: String -> HTML Event
showAddNewList newListName = div $ do
  h5 ! Attributes.className "mt-4" $ text "Add new list"
  form ! Attributes.className "form-inline" $ do
    div ! Attributes.className "form-group" $ do
      input
        ! Attributes.className "form-control"
        ! Attributes.value newListName
        #! onInput (\ev -> ChangeNewListName $ targetValue ev)
      (disableIfStringEmpty newListName button)
        ! Attributes.className "btn btn-primary ml-1"
        #! onClick (const AddNewList) $ text "add"

disableIfStringEmpty :: forall ta. Attributable ta => String -> ta -> ta
disableIfStringEmpty "" x = x ! Attributes.disabled "disabled"
disableIfStringEmpty _ x = x


createCheckbox :: Boolean -> HTML Event
createCheckbox value =
  input
  ! Attributes.type' "checkbox"
  ! Attributes.checked (if value then "checked" else "" )


showLists :: RemoteData GenericLoadingError (List ShoppingList) -> HTML Event
showLists (Success listList) =
  for_ listList showListInOverview
showLists NotAsked = span $ text "Lists not loading yet"  -- now due to autoloading, this shouldn't occur
showLists Loading = span $ text "Loading ..."
showLists (Failure e) = do
  div $ text $ "Error while loading:" <> show e
  button
    ! Attributes.className "btn btn-primary"
    #! onClick (const RequestShoppingLists)
    $ text "reload"



showListInOverview :: ShoppingList -> HTML Event
showListInOverview (ShoppingList shoppingList) =
  div do
    a
      -- ! style "cursor: pointer"
      ! Attributes.href "javascript:void(0);"
      #! onClick (const $ PageView $ LoggedIn $ Detail shoppingList.id)
      $ text $ "list: " <> shoppingList.name


showSelectedList :: ShoppingList -> String -> HTML Event
showSelectedList (ShoppingList sl) newItemName = div do
  showList (ShoppingList sl) ( div do
    h5 $ text "Add new item to list"
    form ! Attributes.className "form-inline" $ do
      div ! Attributes.className "form-group" $ do
        input
          ! Attributes.className "form-control"
          ! Attributes.value newItemName
          #! onChange (\ev -> ChangeNewItemName $ targetValue ev)
        (disableIfStringEmpty newItemName button )
          ! Attributes.className "btn btn-primary ml-1"
          #! onClick (const $ AddNewItem sl.id) $ text "add"
  )


showList :: ShoppingList -> HTML Event -> HTML Event
showList (ShoppingList shoppingList) transclusion =
  div ! Attributes.className "card mb-3" $ do
     div ! Attributes.className "card-body" $ do
       h3 ! Attributes.className "card-title" $ text shoppingList.name
       ul ! Attributes.className "list-unstyled" $ for_ shoppingList.items showItem
       transclusion


showItem :: Item -> HTML Event
showItem (Item item) = li do
  -- using labels for the checkbox would be the most standard and accessible way,
  -- however it screws up bootstrap styling. so we just make everything clickable here
  div ! Attributes.className "input-group" #! onClick (const $ RequestToggleBoughtState item.id (not item.bought)) $ do
    span ! Attributes.className "input-group-addon" $
      createCheckbox item.bought
    span
      ! Attributes.className "form-control form-control-plaintext span-with-border"
      $ text item.name
