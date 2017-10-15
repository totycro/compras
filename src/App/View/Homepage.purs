module App.View.Homepage where

import Prelude (const)

import App.Events (Event(..))
import App.State (State(..), User(..))
import Control.Bind (discard)
import Data.Function (($))
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
    div do 
       renderUserSelectionButton $ User {name: "moni"}
       renderUserSelectionButton $ User {name: "berni"}

    {-
    --div do (renderUserSelectionButton $ User {name: "a"} ) `bind` (const (renderUserSelectionButton $ User {name: "b"}))
    --div $ foldl (\a b -> bind a b) $ userSelectionButtons
    div do
       foldl foldFun foldInit userSelectionButtons

  where 
        users = User {name: "moni"} : User {name: "berni"} : Nil
        userSelectionButtons :: List (HTML Event)
        userSelectionButtons = map renderUserSelectionButton users
        foldFun :: forall e. Markup e -> HTML Event -> Markup e
        foldFun a b = a `bind` (\x -> b)
        foldInit :: forall e. Markup e
        foldInit = (liftF (Empty unit))
        -}
