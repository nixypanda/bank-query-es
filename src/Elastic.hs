{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Elastic
  ( queryES
  ) where

import Database.Bloodhound
import Network.HTTP.Client

import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (pack)

import BQL
  ( BQL(..)
  , Operator(..)
  )

getRangeFilter :: Operator -> Double -> RangeValue
getRangeFilter QLessThan value = RangeDoubleLt (LessThan value)
getRangeFilter QGreaterThan value = RangeDoubleGt (GreaterThan value)
getRangeFilter QLessThanEqual value = RangeDoubleLte (LessThanEq value)
getRangeFilter QGreaterThanEqual value = RangeDoubleGte (GreaterThanEq value)
getRangeFilter QEqual _ = error "Range with equal should not be possible"


bqlToElastic :: BQL -> (Maybe Query, Maybe Filter)
bqlToElastic (QBalance op balance) = (Nothing, Just $ RangeFilter (FieldName "balance") (getRangeFilter op balance) RangeExecutionIndex False)
bqlToElastic (QGender gender) = (Just $ QueryMatchQuery $ mkMatchQuery (FieldName "gender") (QueryString (pack . show $ gender)), Nothing)


esServer :: Server
esServer =
  Server "http://localhost:9200"

bankIndex :: IndexName
bankIndex =
  IndexName "bank"

withBH' :: BH IO a -> IO a
withBH' =
  withBH defaultManagerSettings esServer

queryES :: BQL -> IO String
queryES bankQuery = do
  let (searchQ, filterQ) = bqlToElastic bankQuery
  let query = mkSearch searchQ filterQ
  reply <- withBH' $ searchByIndex bankIndex query
  return . unpack $ responseBody reply

