module App.Routes where

import Prelude

import App.Types (ShoppingList(..), ShoppingListId, User)
import Control.Alternative ((<|>), (<*))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit, int)


data MainRoute
  = Home
  | LoggedIn LoggedInSubRoute
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


data LoggedInSubRoute
  = Overview
  | Detail ShoppingListId

instance loggedInSubRouteEq :: Eq (LoggedInSubRoute) where
  eq Overview Overview = true
  eq (Detail slIdA) (Detail slIdB) = (slIdA == slIdB)
  eq _ _ = false

instance loggedInSubRouteShow :: Show (LoggedInSubRoute) where
  show Overview = "Overview"
  show (Detail slId) = "Detail " <> show slId


-- TODO: consider alternative routing: https://github.com/dariooddenino/pux-blog/blob/master/src/Layout.purs

match :: String -> MainRoute
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  LoggedIn Overview <$ (lit "#" *> lit "app") <* end
  <|>
  LoggedIn <$> (Detail <$> (lit "#" *> lit "app" *> lit "lists" *> int)) <* end

toURL :: MainRoute -> String
toURL (NotFound url) = url
toURL (LoggedIn Overview) = "/#/app"
toURL (LoggedIn (Detail slId)) = "/#/app/lists/" <> show slId
toURL (Home) = "/"
