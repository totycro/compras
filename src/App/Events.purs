module App.Events where

import App.State
import App.Types
import Data.Tuple

import App.Routes (LoggedInSubRoute(..), MainRoute(..), toURL)
import Control.Monad.Aff (attempt)
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


data Event
  = PageView MainRoute
  | UserLoggedIn User
  | RequestShoppingLists
  | ReceiveShoppingLists (Either String (List ShoppingList))
  | RequestToggleBoughtState ItemId Boolean
  | ReceiveToggleBoughtState (Either String { id :: ItemId, bought :: Boolean })
  | ChangeNewListName String
  | AddNewList
  | ReceiveNewShoppingList (Either String ShoppingList)
  | ChangeNewItemName String
  | AddNewItem ShoppingListId
  | ReceiveNewItem (Either String (Tuple ShoppingListId Item))


type AppEffects fx = (ajax :: AJAX, dom :: DOM, history :: HISTORY | fx)

-- TODO: get rid of this?
newtype ToggleBoughtStateResponse = ToggleBoughtStateResponse { bought :: Boolean }

instance toggleBoughtStateResponseDecodeJson :: DecodeJson (ToggleBoughtStateResponse) where
  decodeJson json = do
     obj <- decodeJson json
     bought <- obj .? "bought"
     pure $ ToggleBoughtStateResponse { bought: bought }


setBrowserUrl r = do
   liftEff do
      h <- history =<< window
      pushState (toForeign {}) (DocumentTitle "") (URL $ toURL r) h
   pure Nothing


foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView (NotFound url)) (State st) =
	noEffects $ State st { route = NotFound url}
foldp (PageView Home) (State st) =
	{ state: State st { route = Home}
  , effects: [ setBrowserUrl Home ]
  }
foldp (PageView (LoggedIn subRoute)) (State st) =
  { state: State st { route = LoggedIn subRoute }
  , effects:
    [ setBrowserUrl (LoggedIn subRoute)
    , case st.lists of
        NotAsked -> pure $ Just RequestShoppingLists
        _ -> pure Nothing
    ]
  }

foldp (UserLoggedIn user) (State st) =
  { state: State st { currentUser = Just user }
  , effects: [ pure $ Just $ PageView $ LoggedIn Overview ]
  }

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
-- TOOD: refactor such that new lists by design only can be created in parts of the program where we actually have loaded lists alread
foldp (ReceiveNewShoppingList (Left err)) (State st) =
  noEffects $ State st  -- TODO: error handling
  -- TODO: add check in backend if list with name already exists and handle error here
foldp (ReceiveNewShoppingList (Right newList)) (State st) =
  noEffects $ State st { lists = (\lists -> snoc lists newList) <$> st.lists}

foldp (ChangeNewItemName newItemName) (State st) =
  noEffects $ State st { newItemName = newItemName }

foldp (AddNewItem slId) (State st) =
  { state: State st { newItemName = "" }
  , effects: [ do
    let requestJson = ("name" := st.newItemName) ~> jsonEmptyObject
    res <- attempt $ post ("/api/list/" <> show slId <> "/") requestJson
    let decode r = decodeJson r.response :: Either String (Item)
    let newItem = either (Left <<< show) decode res :: Either String (Item)
    pure $ Just $ ReceiveNewItem $ map (\item -> Tuple slId item) newItem
  ]
  }
foldp (ReceiveNewItem (Left err)) (State st) =
  noEffects $ State st  -- TODO: error handling
  -- TODO: add check in backend if item with name already exists and handle error here
foldp (ReceiveNewItem (Right tup)) (State st @ { lists: Success lists}) =
  noEffects $ State st { lists = Success $ updateLists <$> lists}
  where
        updateLists (ShoppingList sl) | sl.id == (fst tup) = ShoppingList $ sl { items = snoc sl.items (snd tup)}
        updateLists s = s
foldp (ReceiveNewItem (Right tup)) (State st) = noEffects $ State st -- TODO: get rid of this state


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

foldp (RequestToggleBoughtState itemId newBoughtState) (State st) =
  { state : State st
  , effects: [
  do
     let requestJson = ("bought" := newBoughtState) ~> jsonEmptyObject
     res <- attempt $ patch ("/api/item/" <> (show itemId) <> "/") requestJson
     let decode r = decodeJson r.response :: Either String ToggleBoughtStateResponse
     let result = either (Left <<< show) decode res :: Either String ToggleBoughtStateResponse
     let resultParsed = map (\(ToggleBoughtStateResponse response) -> { id: itemId, bought: response.bought } ) result
     pure $ Just $ ReceiveToggleBoughtState resultParsed
   ]
   }

foldp (ReceiveToggleBoughtState (Left err)) (State st) =
  noEffects $ State st  -- TODO: error handling
foldp (ReceiveToggleBoughtState (Right result)) (State st @ { lists: Success loadedLists }) =
  noEffects $ State st {
    lists = Success newLists
  }
  where
        newLists = updateBoughtState loadedLists result.id result.bought
foldp (ReceiveToggleBoughtState (Right result)) (State st) =
  noEffects $ State st


updateBoughtState :: List ShoppingList -> ItemId -> Boolean -> List ShoppingList
updateBoughtState shoppingLists itemId newBoughtState =
  map updateShoppingList shoppingLists
  where
        updateShoppingList :: ShoppingList -> ShoppingList
        updateShoppingList (ShoppingList sl) = ShoppingList sl { items = updateItem <$> sl.items }

        updateItem :: Item -> Item
        updateItem (Item item@{ id: id }) | id == itemId = Item (item { bought = newBoughtState } )
        updateItem item = item
