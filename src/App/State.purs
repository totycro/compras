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

newtype Item = Item
  { name :: String
  , addedAt :: DateTime
  , dueAt :: Maybe DateTime
  , addedBy :: User
  , buyAt :: List Shop
  }

newtype ShoppingList = ShoppingList (List Item)

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , currentUser :: Maybe User
  , lists :: RemoteData GenericLoadingError (List ShoppingList)
  }


toEither :: forall a. Maybe a -> Either String a
toEither (Just x) = Right x
toEither (Nothing) = Left "err"


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
     name <- obj .? "name"
     addedBy <- obj .? "addedBy"
     --dueAt <- obj .? "dueAt"
     --addedAt <- obj .? "addedAt"
     buyAt <- obj .? "buyAt"
     pure $ Item
      { name: name
      , addedAt: someDateTime
      , addedBy: addedBy
      , dueAt: Nothing
      , buyAt: buyAt
      }


{-
f :: Array Json -> Either String (List Item)
f x = h $ decodeJson <$> (fromFoldable x)


h :: forall a. List (Either String a) -> Either String (List a)
h Nil = pure Nil
h (x:xs) = Cons <$> x <*> (h xs)
-}


instance decodeJsonShoppingList :: DecodeJson (ShoppingList) where
  decodeJson json = do
    foldJsonArray (Left "err") arrayToShoopingList json
    where 
        --arrayToShoopingList o = ShoppingList <$> f o
        arrayToShoopingList :: Array Json -> Either String ShoppingList
        arrayToShoopingList o = ShoppingList <$> fromFoldable <$> traverse decodeJson o


{-
instance decodeJsonShoppingList :: DecodeJson (ShoppingList) where
  decodeJson json = do
     fromFoldable $ foldJsonArray (Left "err")  \o ->
       fromFoldable $ f o
       -}
{-
instance decodeJsonShoppingList :: DecodeJson (ShoppingList) where
  decodeJson json = do
    -- obj <- decodeJson json
    listJson <- (toEither $ toArray json) :: Either String JArray
    listItems <- (decodeJson <$> listJson) --:: Either String (Array Item)
    (either (Left <<< show) (\x -> ShoppingList x) listItems ) :: Either String ShoppingList
    -- id <- obj .? "id"
    -- title <- obj .? "title"
    -- pure $ Todo { id: id, title: title }
-}

--derive instance newtypeState :: Newtype State _

testItem :: Item
testItem = Item 
  { name: "Zeug"
  , addedAt: DateTime someDay someTime
  , dueAt: Nothing
  , addedBy: User { name: "test" }
  , buyAt: Nil
  } 

someDay = unsafePartial fromJust $ canonicalDate <$> toEnum 2017 <*> (Just September) <*> toEnum 4
someTime = unsafePartial $ fromJust $ Time <$> toEnum 17 <*> toEnum 2 <*> toEnum 16 <*> toEnum 362
someDateTime = DateTime someDay someTime



testList :: ShoppingList
testList = ShoppingList (testItem:Nil)

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , currentUser: Nothing
  --, lists: Success (testList : Nil)
  , lists: NotAsked
  }

