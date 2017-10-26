module App.Routes where

import Prelude

import App.Types (ShoppingList(..))
import Data.Function (($))
import Data.Functor ((<$))
import Control.Alternative ((<|>), (<*))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit)

data Route
  = Home
  | LoggedIn
  | ListDetail ShoppingList
  | NotFound String


-- TODO: nicer routes, cf. e.g. https://github.com/dariooddenino/pux-blog/blob/master/src/Layout.purs

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  LoggedIn <$ (lit "app") <* end

-- TODO: also fix current urls, /app/list/123 doesn't work yet

toURL :: Route -> String
toURL (NotFound url) = url
toURL (LoggedIn ) = "/#/app"
toURL (ListDetail (ShoppingList sl)) = "/app/list/" <> show sl.id
toURL (Home) = "/"
