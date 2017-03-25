{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( startServer
  ) where

import Web.Scotty

import Data.Aeson (Value(..), object, (.=))
import Data.Text (pack)
import Network.HTTP.Types.Status (status500)
import Network.Wai.Middleware.RequestLogger

import BQL (parseBQL)
import Elastic (queryES)


app :: ScottyM ()
app = do
  middleware logStdoutDev

  defaultHandler $ \e -> do
    liftAndCatchIO $ print e
    status status500
    json $ object ["error" .= String "Something went wrong"]

  get "/" $
    html "<h1>Elastic Search Bank Query Application</h1>"

  get "/search" $ do
    query <- param "query" `rescue` const next
    case parseBQL query of
      Left err -> json $ object ["error" .= String (pack $ show err)]
      Right bqlStr -> do
        queryResult <- liftAndCatchIO $ queryES bqlStr
        json $ object ["results" .= queryResult]

  notFound . json $ object ["error" .= String "Not Found"]


startServer :: IO ()
startServer = scotty 8081 app
