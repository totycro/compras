module App.Events where

import Prelude hiding (div)

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Control.Monad.Aff (attempt)
import App.Routes (Route(..))
import App.State (State(..), User, ShoppingList, RemoteData(..), GenericLoadingError(..))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.Either (Either(..), either)
import Network.HTTP.Affjax (AJAX, get)
import Pux (EffModel, noEffects)
import Control.Applicative (pure)

data Event 
  = PageView Route 
  | UserSelected User
  | RequestShoppingLists
  | ReceiveShoppingLists (Either String (List ShoppingList))

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) =
	noEffects $ State st { route = route, loaded = true }
foldp (UserSelected u) (State st) =
  { state: State st { currentUser = Just u }
  , effects: [
      do
         pure (Just (PageView LoggedIn))
     ]
  }
foldp (RequestShoppingLists ) (State st) =
  { state: State st { lists = Loading }
  , effects: [
    do
       res <- attempt $ get "/api/list"
       let decode r = decodeJson r.response :: Either String (List ShoppingList)
       let listsResult = either (Left <<< show) decode res :: Either String (List ShoppingList)
       pure $ Just $ ReceiveShoppingLists listsResult
  ]
  }
foldp (ReceiveShoppingLists (Left err)) (State st) =
  noEffects $ State st { lists = Failure $ Err err }
foldp (ReceiveShoppingLists (Right result)) (State st) = noEffects $ State st
