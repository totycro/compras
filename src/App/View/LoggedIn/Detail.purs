module App.View.LoggedIn.Detail where

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
import App.View.LoggedIn (disableIfStringEmpty, createCheckbox)

view :: State -> HTML Event
view (State st) = div $ do
  button
    ! Attributes.className "btn btn-secondary mb-3"
    #! onClick (const $ PageView (LoggedIn Overview))
    $ text "regresar"
  case (getSelectedList (State st)) of
    Just sl -> showSelectedList sl st.newItemName
    Nothing -> span $ text "Error mientras cargar lista de compras"


-- TODO: datetime serialization
-- TODO: button for actually deleting?
-- TODO:
-- In a future version, have all lists be "selected", i.e. and in edit mode. then we'd need a component for this to be reusable with a state each
-- Alternatively, the overview in the main page could just be an overview, whereas on detail selection the actual items are loaded. this has the disadvantage of increased loading time


showSelectedList :: ShoppingList -> String -> HTML Event
showSelectedList (ShoppingList sl) newItemName = div do
  showList (ShoppingList sl) ( div do
    h5 $ text "Añadir artículo a la lista"
    form ! Attributes.className "form-inline" $ do
      div ! Attributes.className "form-group" $ do
        input
          ! Attributes.className "form-control"
          ! Attributes.value newItemName
          #! onChange (\ev -> ChangeNewItemName $ targetValue ev)
        (disableIfStringEmpty newItemName button )
          ! Attributes.className "btn btn-primary ml-1"
          #! onClick (const $ AddNewItem sl.id) $ text "añadir"
  )


showList :: ShoppingList -> HTML Event -> HTML Event
showList (ShoppingList shoppingList) transclusion =
  div ! Attributes.className "card" $ do
     div ! Attributes.className "card-body" $ do
       h2 ! Attributes.className "card-title text-info" $ text shoppingList.name
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
