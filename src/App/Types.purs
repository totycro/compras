module App.Types where

import Prelude

--import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Argonaut
import Data.Traversable

import App.Config (config)
--import Data.Newtype (class Newtype)

import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.DateTime (DateTime(..))
import Data.Date (canonicalDate,  Month(..))
import Data.Time (Time(..))
import Data.Either (Either(..), either)
import Data.Functor

import Partial.Unsafe (unsafePartial)
import Data.Enum (toEnum)


data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a


instance remoteDataFunctor :: Functor (RemoteData e) where
  map f (Success e) = Success $ f e
  map f NotAsked = NotAsked
  map f (Failure e) = Failure e
  map f Loading = Loading

remoteDataToMaybe :: forall e a. RemoteData e a -> Maybe a
remoteDataToMaybe (Success e) = Just e
remoteDataToMaybe NotAsked = Nothing
remoteDataToMaybe (Failure e) = Nothing
remoteDataToMaybe Loading = Nothing


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

type ItemId = Int

newtype Item = Item
  { id :: ItemId
  , name :: String
  , addedAt :: DateTime
  , dueAt :: Maybe DateTime
  , addedBy :: User
  , buyAt :: List Shop
  , bought :: Boolean
  }

instance itemShow :: Show (Item) where
  show (Item item) = "Item" <> " " <> item.name <> " (bought: " <> show item.bought <>")"


type ShoppingListId = Int

newtype ShoppingList = ShoppingList
  { id :: ShoppingListId
  , name :: String
  , items :: List Item
  }


instance shoppingListShow :: Show (ShoppingList) where
  show (ShoppingList sl) = "List " <> show sl.id <> ": " <> sl.name <> ": " <> show sl.items


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
      { id: id
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
    id <- obj .? "id"
    pure $ ShoppingList { id: id, items: items , name: name }
    where
        arrayToItems :: Array Json -> Either String (List Item)
        arrayToItems o = fromFoldable <$> traverse decodeJson o


testItem :: Item
testItem = Item
  { id: 5
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
testList = ShoppingList { id: 4, name: "TestList", items: (testItem:Nil) }
