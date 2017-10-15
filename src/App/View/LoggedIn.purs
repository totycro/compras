module App.View.LoggedIn where

import Prelude hiding (div)

import App.Events (Event(..))
import App.State
import Control.Bind (discard)
import Data.Function (($))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.List (List(..), (:), foldl)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML
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
    h1 $ text ( "Hoy " <> (getUserName st.currentUser))
    div do
      button ! className "btn btn-primary" #! onClick (const RequestShoppingLists) $ text "load"
    showLists st.lists

-- TODO: list should have a name
-- TODO: items should have bought flag
-- TODO: datetime serialization

showItem :: Item -> HTML Event
showItem (Item item) = div  $ text item.name

showList :: ShoppingList -> HTML Event
showList (ShoppingList items) =
  div do
     h3 $ text "list"
     div $ for_ items showItem

showLists :: RemoteData GenericLoadingError (List ShoppingList) -> HTML Event
showLists (Success listList) =
  div do
    span $ text "loaded"

    div $ for_ listList showList


showLists NotAsked = span $ text "Lists not loading yet"
showLists Loading = span $ text "Loading"
showLists (Failure e) = span $ text $ "Error while loading:" <> show e

