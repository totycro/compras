module App.ListsComponent.Events where

import Data.Tuple

import Control.Monad.Aff (attempt, delay)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Argonaut (class DecodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Either (Either(..), either)
import Data.Foreign (toForeign)
import Data.List (List(..), (:), head, snoc)
import Data.Maybe (Maybe(..), fromMaybe)
import Network.HTTP.Affjax (AJAX, get, patch, post)
import Prelude hiding (div)
import Pux (EffModel, noEffects)
import Data.Time.Duration (Milliseconds(..))
import Data.Int (toNumber)

import App.ListsComponent.State (State(..))
import App.Types

type AppEffects fx = (ajax :: AJAX | fx)

data Event
  = ChangeNewListName String
  | AddNewList
  | ReceiveNewShoppingList (Either String ShoppingList)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (ChangeNewListName newListName) (State st) =
  noEffects $ State st { newListName = newListName }

foldp (AddNewList) (State st) =
  { state: State st { newListName = "" }
  , effects: [ do
    let requestJson = ("name" := st.newListName) ~> jsonEmptyObject
    res <- attempt $ post "/api/list" requestJson
    let decode r = decodeJson r.response :: Either String (ShoppingList)
    let listResult = either (Left <<< show) decode res :: Either String (ShoppingList)
    pure $ Just $ ReceiveNewShoppingList listResult
  ]
  }

foldp (ReceiveNewShoppingList (Left err)) (State st) =
  noEffects $ State st  -- TODO: error handling
  -- TODO: add check in backend if list with name already exists and handle error here
foldp (ReceiveNewShoppingList (Right newList)) (State st) =
  noEffects $ State st { lists = snoc st.lists newList }
