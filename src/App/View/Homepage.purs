module App.View.Homepage where

import Prelude (const)

import App.Events (Event(..))
import App.State (State(..))
import App.Types (User(..))
import Control.Bind (discard)
import Data.Function (($))
import Data.Foldable
import Data.List (List(..), (:), foldl)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (a, div, h1, button)
import Text.Smolder.HTML.Attributes (className, style)
import Text.Smolder.Markup ((#!), (!), text)
-- import Text.Smolder.Markup (MarkupM(..), Markup)
-- import Control.Monad.Free (liftF)


renderUserSelectionButton :: User -> HTML Event
renderUserSelectionButton (User u) = 
     button ! className "btn btn-primary" ! style "margin: 4px" #! onClick (const $ UserSelected (User u)) $ text u.name

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text "Quien es?"
    div $ for_ users renderUserSelectionButton 
  where 
        users = User {name: "moni"} : User {name: "berni"} : Nil
