module App.ListsComponent.Routes where

import Prelude
import Control.Alternative ((<|>), (<*))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit, int, Match)


import App.Types

data Route
  = Overview
  | Detail ShoppingListId


instance routeEq :: Eq (Route) where
  eq Overview Overview = true
  eq (Detail slIdA) (Detail slIdB) = (slIdA == slIdB)
  eq _ _ = false


instance routeShow :: Show (Route) where
  show Overview = "Overview"
  show (Detail slId) = "Detail " <> show slId


match :: Match Route
match =
  Overview <$ (lit "#" *> lit "app") <* end
  <|>
  Detail <$> (lit "#" *> lit "app" *> lit "lists" *> int) <* end
