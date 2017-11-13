module App.Routes where

import Prelude

import App.Types (ShoppingList(..), ShoppingListId, User)
import Control.Alternative ((<|>), (<*))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit, int)
import App.ListsComponent.Routes as ListsComponentRoutes


data MainRoute
  = Home
  | LoggedIn ListsComponentRoutes.Route
  | NotFound String

instance mainRouteShow :: Show (MainRoute) where
  show Home = "Home"
  show (LoggedIn subRoute) = "LoggedIn: " <> show subRoute
  show (NotFound url) = "NotFound: " <> url

instance mainRouteEq :: Eq (MainRoute) where
  eq Home Home = true
  eq (LoggedIn subRouteA) (LoggedIn subRouteB) = (subRouteA == subRouteB)
  eq (NotFound a) (NotFound b) = (a == b)
  eq _ _ = false

-- TODO: consider alternative routing: https://github.com/dariooddenino/pux-blog/blob/master/src/Layout.purs

match :: String -> MainRoute
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  LoggedIn <$> ListsComponentRoutes.match


toURL :: MainRoute -> String
toURL _ = "not implemented yet"
-- toURL (NotFound url) = url
-- toURL (LoggedIn Overview) = "/#/app"
-- toURL (LoggedIn (Detail slId)) = "/#/app/lists/" <> show slId
-- toURL (Home) = "/"
