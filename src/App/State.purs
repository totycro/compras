module App.State where

import App.Config (config)
import App.Routes (Route, match)
--import Data.Newtype (class Newtype)


data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a


newtype User = User
  { name :: String
  }

newtype Item = Item
  { name :: String,



newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  }

--derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  }
