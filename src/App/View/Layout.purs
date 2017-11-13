module App.View.Layout where

import CSS hiding (div, (!))

import App.Events (Event)
import App.Routes
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.LoggedIn.Overview as LoggedInOverview
import App.View.LoggedIn.Detail as LoggedInDetail
import App.View.NotFound as NotFound
import CSS.Border (solid)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), text)

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css

    case st.error of
      "" -> text ""
      msg -> div $ text msg

    case st.route of
      (Home) -> Homepage.view (State st)
      (LoggedIn Overview) -> LoggedInOverview.view (State st)
      (LoggedIn (Detail _)) -> LoggedInDetail.view (State st)
      (NotFound url) -> NotFound.view (State st)

css :: CSS
css = do

  let black = rgb 0 0 0
      bootstrapDarkGray = rgb 206 212 218

  fromString ".span-with-border" ? do
    color black
    borderColor bootstrapDarkGray
    key (fromString "border-style") solid
    key (fromString "border-width") (1.0 #px)
