module App.Events where

import Prelude hiding (div)

import Data.Argonaut (class DecodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Control.Monad.Aff (attempt)
import App.Routes (Route(..))
import App.State
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.Either (Either(..), either)
import Network.HTTP.Affjax (AJAX, get, patch)
import Pux (EffModel, noEffects)
import Control.Applicative (pure)


data Event 
  = PageView Route 
  | UserSelected User
  | RequestShoppingLists
  | ReceiveShoppingLists (Either String (List ShoppingList))
  | RequestToggleBoughtState ItemId Boolean
  | ReceiveToggleBoughtState (Either String { id :: ItemId, bought :: Boolean })

type AppEffects fx = (ajax :: AJAX | fx)

-- TODO: get rid of this?
newtype ToggleBoughtStateResponse = ToggleBoughtStateResponse { bought :: Boolean }

instance toggleBoughtStateResponseDecodeJson :: DecodeJson (ToggleBoughtStateResponse) where
  decodeJson json = do
     obj <- decodeJson json
     bought <- obj .? "bought"
     pure $ ToggleBoughtStateResponse { bought: bought }


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
foldp (ReceiveShoppingLists (Right result)) (State st) =
  noEffects $ State st { lists = Success result }

foldp (RequestToggleBoughtState (ItemId id) newBoughtState) (State st) =
  { state : State st
  , effects: [
  do
     let requestJson = ("bought" := newBoughtState) ~> jsonEmptyObject
     res <- attempt $ patch ("/api/item/" <> (show id) <> "/") requestJson
     let decode r = decodeJson r.response :: Either String ToggleBoughtStateResponse
     let result = either (Left <<< show) decode res :: Either String ToggleBoughtStateResponse
     let resultParsed = map (\(ToggleBoughtStateResponse response) -> { id: ItemId id, bought: response.bought } ) result
     pure $ Just $ ReceiveToggleBoughtState resultParsed
   ]
   }

foldp (ReceiveToggleBoughtState (Left err)) (State st) =
  noEffects $ State st  -- TODO: error handling?
foldp (ReceiveToggleBoughtState (Right result)) (State st @ { lists: Success loadedLists }) =
  noEffects $ State st { lists = Success newList }
  where
        newList = updateBoughtState loadedLists result.id result.bought
foldp (ReceiveToggleBoughtState (Right result)) (State st) =
  noEffects $ State st


updateBoughtState :: List ShoppingList -> ItemId -> Boolean -> List ShoppingList
updateBoughtState shoppingLists itemId newBoughtState =
  map updateShoppingList shoppingLists
  where
        updateShoppingList :: ShoppingList -> ShoppingList
        updateShoppingList (ShoppingList sl) = ShoppingList sl { items = updateItem <$> sl.items }

        updateItem :: Item ->  Item
        updateItem (Item item@{ id: id }) | id == itemId = Item (item { bought = newBoughtState } )
        updateItem item = item
