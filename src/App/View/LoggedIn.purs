module App.View.LoggedIn where

import App.Events (Event)

import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (input)
import Text.Smolder.HTML.Attributes as Attributes
import Text.Smolder.Markup (class Attributable, (!))


disableIfStringEmpty :: forall ta. Attributable ta => String -> ta -> ta
disableIfStringEmpty "" x = x ! Attributes.disabled "disabled"
disableIfStringEmpty _ x = x

createCheckbox :: Boolean -> HTML Event
createCheckbox value =
  input
  ! Attributes.type' "checkbox"
  ! Attributes.checked (if value then "checked" else "" )
