module App.Routes where

import Data.Function (($))
import Data.Functor ((<$))
import Control.Alternative ((<|>), (<*))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit)

data Route
  = Home
  | LoggedIn
  | NotFound String

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  LoggedIn <$ (lit "app") <* end

toURL :: Route -> String
toURL (NotFound url) = url
toURL (LoggedIn ) = "/app"
toURL (Home) = "/"
