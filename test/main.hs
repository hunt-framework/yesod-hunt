{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Applicative ((<$>))
import           Yesod
import           Yesod.Hunt

-- main application state
data App = App
    { index :: HuntS
    }

-- main application routes
mkYesod "App" [parseRoutes|
/ HomeR GET
/hunt/ HuntSR HuntS index
|]

-- typeclass the main application implements
instance Yesod App
instance YesodHunt App

-- application start
main :: IO ()
main = do
    app <- App <$> initHuntS
    warp 3000 app

-- kind of JS client library for holumbus
getHomeR :: Handler Html
getHomeR = defaultLayout 
           [whamlet|Welcome to Yesod with Hunt integration|]
