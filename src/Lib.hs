{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startServer
  ) where

import Servant
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Proxy as Proxy
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as WaiCors

import BQL (parseBQL)
import Elastic (queryES)

{-
   Describing a REST API using a type. Get on this api returns a story.
 -}
type SearchAPI =
  "search" :> QueryParam "query" String :> Get '[JSON] String


{-
   The part where we describe how we actually serve the api.
 -}
serverSearch :: Maybe String -> ExceptT ServantErr IO String
serverSearch Nothing = return "No Query provided"
serverSearch (Just queryStr) =
  case parseBQL queryStr of
    Left err -> return . show $ err
    Right bqlStr -> liftIO $ queryES bqlStr

{-
   No Idea WTF is this
 -}
storyAPI :: Proxy.Proxy SearchAPI
storyAPI =
  Proxy.Proxy

{-
   'serve' comes from servant and hands you a WAI Application,
   which you can think of as an "abstract" web application,
   not yet a webserver.
 -}
app1 :: Wai.Application
app1 =
  serve storyAPI serverSearch

{-
   The main entry point for our application. It starts our web application on
   port 8081 (and uses warp behind the scene to do so.)
 -}
startServer :: IO ()
startServer =
  Warp.run 8081 (WaiCors.simpleCors app1)

