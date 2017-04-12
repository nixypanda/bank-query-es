{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( readEnv
  ) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Environment (lookupEnv)

import Database.Bloodhound (BH, IndexName(..), Reply, Server(..), withBH)
import Network.HTTP.Client (defaultManagerSettings)

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.IO as TIO

import Types (Config(..))


textToMap :: T.Text -> M.Map T.Text [T.Text]
textToMap = M.fromList . map ((\(x:xs) -> (x, xs)) . T.words) . T.lines


fileToMap :: FilePath -> IO (M.Map T.Text [T.Text])
fileToMap path = do
  contents <- TIO.readFile path
  return $ textToMap contents


constructUrl :: Maybe String -> Maybe String -> String -> String -> T.Text
constructUrl user pass url port =
  let
    basicAuth :: String
    basicAuth = maybe "" concat . sequence $ [user, pure ":", pass, pure "@"]
 in
    T.pack $ "http://" ++ basicAuth ++ url ++ ":" ++ port


readEnv :: IO (Config Reply)
readEnv = do
  appPort <- fmap (fromMaybe 8081 . (>>= readMaybe)) (lookupEnv "SCOTTY_PORT")
  username <- lookupEnv "ES_USERNAME"
  password <- lookupEnv "ES_PASSWORD"
  url <- fmap (fromMaybe "localhost") (lookupEnv "ES_URL")
  port <- fmap (fromMaybe "9200") (lookupEnv "ES_PORT")
  index' <- fmap (T.pack . fromMaybe "bank") (lookupEnv "ES_INDEX")
  kMapPath <- lookupEnv "KEY_MAP"
  fMapPath <- lookupEnv "FILTER_MAP"
  kMap' <- maybe (return M.empty) fileToMap kMapPath
  fMap' <- maybe (return M.empty) fileToMap fMapPath

  let esurl = constructUrl username password url port
  let withBH'' = withBH defaultManagerSettings (Server esurl)
  let config = Config withBH'' (IndexName index') appPort kMap' fMap'

  return config
