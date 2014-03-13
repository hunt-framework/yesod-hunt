module Yesod.Hunt.Routes where

import           Control.Applicative
import           Hunt.Interpreter.Interpreter
import           Yesod

-- | wrapper type for index environment
data HuntS = HuntS { getHunt :: DefHuntEnv }

-- | helper for easy initiation
initHuntS :: IO HuntS
initHuntS = HuntS <$> initHunt

-- | class that has to be implemented for yesod master application
class Yesod master => YesodHunt master where

-- | TemplateHaskell magic: create Types for routes with
--   that small QQ-Dsl then generate Yesod Dispatch
mkYesodSubData "HuntS" [parseRoutes|
/search/#String            HSearch         GET
/search/#String/#Int/#Int  HPagedSearch    GET
/completion/#String        HCompletion     GET
|]

