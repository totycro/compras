module App.ListsComponent.Events where

import App.ListsComponent.State (State(..))
import Pux (EffModel, noEffects)
import Network.HTTP.Affjax (AJAX)

import Prelude

type AppEffects fx = (ajax :: AJAX | fx)

data Event
  = ChangeNewListName String
  | AddNewList


foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp ev (State st) = noEffects $ State st
