{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( startServer
  ) where

import Control.Monad.Trans (liftIO)
import Data.Text (Text, pack)
import Data.Map (Map)

import Data.Aeson (Value(..), object, (.=))
import Database.Bloodhound (BH, IndexName(..), Reply)
import Web.Scotty

import Network.HTTP.Types.Status (status500)
import Network.Wai.Middleware.RequestLogger

import BQL (parseBQL)
import Elastic (queryES)
import Utils (readEnv)
import Types


indexRoute :: ActionM ()
indexRoute =
  html "<h1>Elastic Search Bank Query Application</h1>"


app :: Map Text [Text] -> Map Text [Text] -> (BH IO Reply -> IO Reply) -> IndexName -> ScottyM ()
app kMap' fMap' withBH bankIndex = do
  middleware logStdoutDev

  defaultHandler $ \e -> do
    liftAndCatchIO $ print e
    status status500
    json $ object ["error" .= String "Something went wrong"]

  get "/"
    indexRoute

  get "/search" $ do
    query <- param "query" `rescue` const next
    case parseBQL kMap' fMap' query of
      Left err -> json $ object ["error" .= String (pack $ show err)]
      Right bqlStr -> do
        queryResult <- liftIO $ queryES withBH bankIndex bqlStr
        json $ object ["results" .= queryResult]

  notFound . json $ object ["error" .= String "Not Found"]


startServer :: IO ()
startServer = do
  config <- readEnv
  scotty (port config) (app (kMap config) (fMap config) (withBH' config) (index config))
