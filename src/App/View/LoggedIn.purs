module App.View.LoggedIn where

import Prelude (const, (<>))

import App.Events (Event(..))
import App.State (State(..), User(..), GenericLoadingError, ShoppingList, RemoteData)
import Control.Bind (discard)
import Data.Function (($))
import Data.Maybe (fromMaybe, Maybe(..))
import Data.List (List(..), (:), foldl)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (a, div, h1, button)
import Text.Smolder.HTML.Attributes (className, style)
import Text.Smolder.Markup ((#!), (!), text)


getUserName :: Maybe User -> String
getUserName maybeUser = case maybeUser of
  Just (User user) -> user.name
  Nothing -> ""

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text ( "Hoy " <> (getUserName st.currentUser))
    showLists st.lists


showLists :: RemoteData GenericLoadingError (List ShoppingList) -> HTML Event
showLists rd = h1 $ text "Here"

