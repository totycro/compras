module App.View.LoggedIn.Overview where

import App.Routes
import App.Types
import App.Events
import App.State (State(..))
import App.View.Utils (disableIfStringEmpty)
import App.ListsComponent as ListsComponent

import Data.Foldable (for_)
import Data.List (List, length)
import Data.Maybe (Maybe(..))
import Prelude hiding (div)
import Pux.DOM.Events (onClick, onChange, onInput, targetValue)
import Pux.DOM.HTML (HTML, mapEvent)
import Text.Smolder.HTML hiding (map)
import Text.Smolder.HTML.Attributes as Attributes
import Text.Smolder.Markup ((#!), (!), text)



getUserName :: Maybe User -> String
getUserName maybeUser = case maybeUser of
  Just (User user) -> user.name
  Nothing -> ""

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text ( "Bienvenid@ " <> (getUserName st.currentUser))
    showLists st.lists'

showLists :: RemoteData GenericLoadingError (ListsComponent.State) -> HTML Event
showLists (Success listComponentState) = mapEvent (\ev -> ListsComponentEvent ev) listsComponentHtml
  where
    listsComponentHtml = ListsComponent.view listComponentState
showLists NotAsked = span $ text "Listas todavía no están cargando"  -- now due to autoloading, this shouldn't occur
showLists Loading = span $ text "Cargando ..."
showLists (Failure e) = do
  div $ text $ "Error mientras cargando:" <> show e
  button
    ! Attributes.className "btn btn-primary"
    #! onClick (const RequestShoppingLists)
    $ text "cargar de nuevo"
