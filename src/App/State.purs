module App.State where

import Prelude

--import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Argonaut
import Data.Traversable

import App.Config (config)
import App.Routes (Route, match)
--import Data.Newtype (class Newtype)

import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.DateTime (DateTime(..))
import Data.Date (canonicalDate,  Month(..))
import Data.Time (Time(..))
import Data.Either (Either(..), either)

import Partial.Unsafe (unsafePartial)
import Data.Enum (toEnum)


data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a

data GenericLoadingError
  = Err String

instance showGenericLoadingError :: Show (GenericLoadingError) where
  show (Err e) = "Error: " <> e

newtype User = User
  { name :: String
  }

newtype Shop = Shop
  { name :: String
  }

newtype ItemId = ItemId Int

instance itemEq :: Eq (ItemId) where
  eq (ItemId a) (ItemId b) = eq a b

newtype Item = Item
  { id :: ItemId
  , name :: String
  , addedAt :: DateTime
  , dueAt :: Maybe DateTime
  , addedBy :: User
  , buyAt :: List Shop
  , bought :: Boolean
  }

newtype ShoppingList = ShoppingList
  { name :: String
  , items :: List Item
  }

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , currentUser :: Maybe User
  , lists :: RemoteData GenericLoadingError (List ShoppingList)
  }


toEither :: forall a. String -> Maybe a -> Either String a
toEither _ (Just x) = Right x
toEither s (Nothing) = Left s


instance decodeJsonUser :: DecodeJson (User) where
  decodeJson json = do
     obj <- decodeJson json
     name <- obj .? "name"
     pure $ User { name: name } 


instance decodeJsonShop :: DecodeJson (Shop) where
  decodeJson json = do
     obj <- decodeJson json
     name <- obj .? "name"
     pure $ Shop { name: name } 


instance decodeJsonItem :: DecodeJson (Item) where
  decodeJson json = do
     obj <- decodeJson json
     id <- obj .? "id"
     name <- obj .? "name"
     addedBy <- obj .? "addedBy"
     --dueAt <- obj .? "dueAt"
     --addedAt <- obj .? "addedAt"
     buyAt <- obj .? "buyAt"
     bought <- obj .? "bought"
     pure $ Item
      { id: ItemId id
      , name: name
      , addedAt: someDateTime
      , addedBy: addedBy
      , dueAt: Nothing
      , buyAt: buyAt
      , bought: bought
      }


instance decodeJsonShoppingList :: DecodeJson (ShoppingList) where
  decodeJson json = do
    obj <- decodeJson json
    itemsJson <- obj .? "items"
    items <- foldJsonArray (Left $ "Not array " <> show json) arrayToItems itemsJson
    name <- obj .? "name"
    pure $ ShoppingList { items: items , name: name }
    where 
        arrayToItems :: Array Json -> Either String (List Item)
        arrayToItems o = fromFoldable <$> traverse decodeJson o


testItem :: Item
testItem = Item 
  { id: ItemId 5
  , name: "Zeug"
  , addedAt: DateTime someDay someTime
  , dueAt: Nothing
  , addedBy: User { name: "test" }
  , buyAt: Nil
  , bought: false
  } 

someDay = unsafePartial fromJust $ canonicalDate <$> toEnum 2017 <*> (Just September) <*> toEnum 4
someTime = unsafePartial $ fromJust $ Time <$> toEnum 17 <*> toEnum 2 <*> toEnum 16 <*> toEnum 362
someDateTime = DateTime someDay someTime



testList :: ShoppingList
testList = ShoppingList { name: "TestList", items: (testItem:Nil) }

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , currentUser: Nothing
  --, lists: Success (testList : Nil)
  , lists: NotAsked
  }

