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
import Pux.DOM.Events (onClick, onChange)
import Text.Smolder.HTML hiding (style)
import Text.Smolder.HTML.Attributes hiding (span)
import Text.Smolder.Markup ((#!), (!), text)
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
    centralView st.lists st.selectedList

centralView :: RemoteData GenericLoadingError (List ShoppingList) -> Maybe ShoppingList -> HTML Event
centralView shoppingLists (Just sl) = showSelectedList sl
centralView shoppingLists Nothing = showLists shoppingLists

-- TODO: datetime serialization
-- TODO: button for actually deleting?
-- TODO: In a future version, have all lists be "selected", i.e. and in edit mode. then we'd need a component for this to be reusable with a state each

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
      #! onClick (ShoppingListSelected $ ShoppingList shoppingList)
      $ text $ "list: " <> shoppingList.name


showSelectedList :: ShoppingList -> HTML Event
showSelectedList sl = showList sl

showList :: ShoppingList -> HTML Event
showList (ShoppingList shoppingList) =
  div do
     h3 $ text $ "list: " <> shoppingList.name
     ul $ for_ shoppingList.items showItem
     div do
        input -- ! value st.newItem #! onChange (const ChangeNewItem)
        button $ text "add" -- #! onClick (const AddNewItem
