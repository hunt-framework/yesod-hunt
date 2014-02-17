module Yesod.Hunt.Routes where

import           Hunt.Interpreter.Interpreter
import           Hunt.Query.Ranking

import Yesod

-- | wrapper type for index environment
data Hunt = Hunt { getHunt :: DefaultEnv }

-- | helper for easy initiation
initHunt :: IO Hunt
initHunt = do
  env <- initEnv emptyIndexer defaultRankConfig contextTypes
  return $ Hunt env

-- | class that has to be implemented for yesod master application
class Yesod master => YesodHunt master where

-- | TemplateHaskell magic: create Types for routes with
--   that small QQ-Dsl then generate Yesod Dispatch
mkYesodSubData "Hunt" [parseRoutes|
/search/#String            HSearch         GET
/search/#String/#Int/#Int  HPagedSearch    GET
/completion/#String        HCompletion     GET
|]

