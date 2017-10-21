module App.View.Homepage where

import Prelude (show, const)

import App.Events (Event(..))
import App.State (State(..))
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (a, div, h1, span, button)
import Text.Smolder.HTML.Attributes (href, className, style)
import Text.Smolder.Markup ((#!), (!), text)

view :: State -> HTML Event
view (State st) =
 div do
    h1 $ text "Hola"
    a ! style "display: none" ! className "guide" ! href "https://www.purescript-pux.org/" $ text $ show "Guide"
    a ! style "display: none" ! className "guide" ! href "https://www.purescript-pux.org/" $ text $ show "Guide"
    div do span $ text (show st.myCounter)
    button #! onClick (const $ TheClickEvent 3) $ text "p 3"
    button #! onClick (const $ TheClickEvent 5) $ text "p 5"

viewX :: State -> HTML Event
viewX (State st) =
  div do
    span $ text (show st.myCounter)

view2 :: State -> HTML Event
view2 (State st) =
  div do
    h1 $ text "Pux"
    a ! className "guide" ! href "https://www.purescript-pux.org/" $ text "Guide"
    --button $ text "foo"
    a ! className "github" ! href "https://github.com/alexmingoia/purescript-pux/" $ text "GitHub"
    --div ! (style "margin-top: 30px") [
    --div $  do span $ text "foo"
    a $ text "Guide"

