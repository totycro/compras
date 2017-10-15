module App.Events where

import App.Routes (Route(..))
import App.State (State(..), User)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)
import Control.Applicative (pure)

data Event 
  = PageView Route 
  | UserSelected User

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
	noEffects $ State st { route = route, loaded = true }
foldp (UserSelected u) (State st) =
  { state: State (st { currentUser = Just u })
  , effects: [
      do
         pure (Just (PageView LoggedIn))
     ]
  }
