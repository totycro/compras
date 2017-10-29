module Test.Main where

import Prelude

import App.Routes (LoggedInSubRoute(..), MainRoute(..), match, toURL)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (for_)
import Test.Assert (ASSERT, assert)

type Tests = Eff (console :: CONSOLE, assert :: ASSERT) Unit

main :: Tests
main = do
  log "Test route/url generating and parsing"
  for_ routes testRoute
  where
    testRoute :: MainRoute -> Tests
    testRoute r = do
      log $ "Testing " <> show r
      assert $ r == (match $ toURL r)

    routes :: Array MainRoute
    routes = [Home, LoggedIn Overview, LoggedIn (Detail 4)]
