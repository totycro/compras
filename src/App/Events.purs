module App.Events where

import Prelude ((+))

import App.Routes (Route)
import App.State (State(..))
import Data.Function (($))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event = PageView Route | TheClickEvent Int

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
	noEffects $ State st { route = route, loaded = true }
foldp (TheClickEvent num) (State st) =
	noEffects $ State st { myCounter = st.myCounter + num }
