{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Hunt.SubSite where

import           Data.Text                        (Text)
import           Data.Aeson

import           Control.Applicative              ((<$>))

import           Hunt.Common

import qualified Hunt.Interpreter.Interpreter as Hol
import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Parser

import           Yesod
import           Yesod.Hunt.Routes

-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure Int [Text]

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure n msg) = object
    [ "code"  .= n
    , "msg"   .= msg
    ]

-- | A subsite needs to be an instance of YesodSubDispatch, which states how to
-- dispatch. By using constraints, we can make requirements of our master site.
instance YesodHunt master => YesodSubDispatch HuntS (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesHuntS)


type HuntHandler a = forall master. YesodHunt master
                  => HandlerT HuntS (HandlerT master IO) a

-- | helper that runs command within holumbus index interpreter
runHunt :: Command -> HuntHandler (Either CmdError CmdResult)
runHunt cmd = do
  env <- getHunt <$> getYesod
  liftIO $ Hol.runCmd env cmd

-- | helper to run simple commands without results
--runCmd :: Command -> HuntHandler Value
--runCmd cmd = do
--  res <- runHunt cmd
--  return . toJSON $ case res of
--    Right (ResOK)            -> JsonSuccess ("Ok"::Text)
--    Left (ResError code msg) -> JsonFailure (code [msg] :: JsonResponse Text)
--    _                        -> JsonFailure 700 ["invalid operation"] :: JsonResponse Text)


-- | search for all documents
getHSearch :: String -> HuntHandler Value
getHSearch query = getHPagedSearch query 1 10000

-- | search for a subset of documents by page
getHPagedSearch :: String -> Int -> Int -> HuntHandler Value
getHPagedSearch query p pp = do
  case parseQuery query of
    Left  err -> return $ toJSON $ (JsonFailure 700 [err] :: JsonResponse Text)
    Right qry -> do
      res <- runHunt $ Search qry p pp
      case res of
        Right (ResSearch docs)   -> return . toJSON
                                  $ JsonSuccess docs
        Left (ResError code msg) -> return . toJSON
                                  $ (JsonFailure code [msg] :: JsonResponse [Document])
        _                        -> return . toJSON
                                  $ (JsonFailure 700 ["invalid operation"] :: JsonResponse [Document])


-- | search for auto-completion terms
getHCompletion :: String -> HuntHandler Value
getHCompletion query = do
  case parseQuery query of
    Left  err -> return . toJSON $ (JsonFailure 700 [err] :: JsonResponse Text)
    Right qry -> do
      res <- runHunt $ Completion qry 20
      case res of
        Right (ResCompletion ws) -> return . toJSON
                                  $ JsonSuccess ws
        Left (ResError code msg) -> return . toJSON
                                  $ (JsonFailure code [msg] :: JsonResponse [Text])
        _                        -> return . toJSON
                                  $ (JsonFailure 700 ["invalid operation"] :: JsonResponse [Text])

