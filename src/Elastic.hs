{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-
   This module contains the conversion function from Bank Query Language to Bloodhound ES Query DSL.
 -}

module Elastic
  ( queryES
  , bqlToElastic
  ) where

import Database.Bloodhound
import Network.HTTP.Client

import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text (pack)

import BQL
  ( BQL(..)
  , Operator(..)
  )

--------------------------------
-- Basic Config for ES Server --
--------------------------------

esServer :: Server
esServer =
  Server "http://localhost:9200"

bankIndex :: IndexName
bankIndex =
  IndexName "bank"

withBH' :: BH IO a -> IO a
withBH' =
  withBH defaultManagerSettings esServer

----------------
-- Conversion --
----------------

getRangeValue :: Operator -> Double -> RangeValue
getRangeValue QLessThan value = RangeDoubleLt (LessThan value)
getRangeValue QGreaterThan value = RangeDoubleGt (GreaterThan value)
getRangeValue QLessThanEqual value = RangeDoubleLte (LessThanEq value)
getRangeValue QGreaterThanEqual value = RangeDoubleGte (GreaterThanEq value)
getRangeValue QEqual _ = error "Range with equal should not be possible"


bqlToElastic :: BQL -> Query
bqlToElastic (QAge op age) =
  QueryRangeQuery $ mkRangeQuery (FieldName "age") (getRangeValue op (fromInteger age))
bqlToElastic (QBalance op balance) =
  QueryRangeQuery $ mkRangeQuery (FieldName "balance") (getRangeValue op balance)
bqlToElastic (QGender gender) =
  QueryMatchQuery $ mkMatchQuery (FieldName "gender") (QueryString (pack . show $ gender))

---------------
-- Executing --
---------------

queryES :: BQL -> IO String
queryES bankQuery = do
  let searchQ = bqlToElastic bankQuery
  let query = mkSearch (Just searchQ) Nothing
  reply <- withBH' $ searchByIndex bankIndex query
  return . unpack $ responseBody reply

