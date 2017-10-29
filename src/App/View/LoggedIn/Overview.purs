module App.View.LoggedIn.Overview where

import App.Routes
import App.State (State(..))
import App.Types
import Text.Smolder.HTML

import App.Events (Event(..))
import Data.Foldable (for_)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Prelude hiding (div)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML.Attributes as Attributes
import Text.Smolder.Markup ((#!), (!), text, class Attributable)
import App.View.LoggedIn (disableIfStringEmpty)


getUserName :: Maybe User -> String
getUserName maybeUser = case maybeUser of
  Just (User user) -> user.name
  Nothing -> ""

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text ( "Bienvenid@ " <> (getUserName st.currentUser))
    showLists st.lists
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
        #! onClick (const AddNewList) $ text "añadir"


showLists :: RemoteData GenericLoadingError (List ShoppingList) -> HTML Event
showLists (Success listList) =
  for_ listList showListInOverview
showLists NotAsked = span $ text "Listas todavía no están cargando"  -- now due to autoloading, this shouldn't occur
showLists Loading = span $ text "Cargando ..."
showLists (Failure e) = do
  div $ text $ "Error mientras cargando:" <> show e
  button
    ! Attributes.className "btn btn-primary"
    #! onClick (const RequestShoppingLists)
    $ text "cargar de nuevo"


showListInOverview :: ShoppingList -> HTML Event
showListInOverview (ShoppingList shoppingList) =
  div do
    a
      ! Attributes.href "javascript:void(0);"
      #! onClick (const $ PageView $ LoggedIn $ Detail shoppingList.id)
      $ text $ "lista: " <> shoppingList.name
