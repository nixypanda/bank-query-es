{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-
   This module contains the conversion function from Bank Query Language to Bloodhound ES Query DSL.
 -}

module Elastic
  ( queryES
  , bqlToElastic
  ) where

import Data.Aeson (Value, eitherDecode)
import Data.Either.Combinators (fromRight')
import Data.Maybe (fromMaybe, fromJust)
import Data.Time.Clock (UTCTime)
import Data.ByteString.Lazy.Char8 (ByteString)
import Database.Bloodhound
import Network.HTTP.Client

import BQL
  ( BQL(..)
  , Operator(..)
  )

---------------------------------------------------------------------------------------------------
-- Basic Config for ES Server ---------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

esServer :: Server
esServer =
  Server "http://localhost:9200"

bankIndex :: IndexName
bankIndex =
  IndexName "bank"

withBH' :: BH IO a -> IO a
withBH' =
  withBH defaultManagerSettings esServer

----------------------------------------------------------------------------------------------------
-- Conversion (IQL -> ELASTIC) ---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

getRangeValue :: Operator -> Double -> RangeValue
getRangeValue B_LessThan value = RangeDoubleLt (LessThan value)
getRangeValue B_GreaterThan value = RangeDoubleGt (GreaterThan value)
getRangeValue B_LessThanEqual value = RangeDoubleLte (LessThanEq value)
getRangeValue B_GreaterThanEqual value = RangeDoubleGte (GreaterThanEq value)
getRangeValue B_Equal value = RangeDoubleGteLte (GreaterThanEq value) (LessThanEq value)

getDateRange :: Operator -> UTCTime -> RangeValue
getDateRange B_LessThan value = RangeDateLt (LessThanD value)
getDateRange B_GreaterThan value = RangeDateGt (GreaterThanD value)
getDateRange B_LessThanEqual value = RangeDateLte (LessThanEqD value)
getDateRange B_GreaterThanEqual value = RangeDateGte (GreaterThanEqD value)
getDateRange B_Equal value = RangeDateGteLte (GreaterThanEqD value) (LessThanEqD value)

bqlToElastic :: BQL -> Query
bqlToElastic (B_String field value) =
  QueryMatchQuery $ mkMatchQuery (FieldName field) (QueryString value)
bqlToElastic (B_MultiString fields' value) =
  QueryMultiMatchQuery $ mkMultiMatchQuery (map FieldName fields') (QueryString value)
bqlToElastic (B_Date field op value) =
  QueryRangeQuery $ mkRangeQuery (FieldName field) (getDateRange op value)
bqlToElastic (B_Num field op value)
  = QueryRangeQuery $ mkRangeQuery (FieldName field) (getRangeValue op value)
bqlToElastic (B_And qs) =
  QueryBoolQuery $ mkBoolQuery (map bqlToElastic qs) [] []
bqlToElastic (B_Or qs) =
  QueryBoolQuery $ mkBoolQuery [] [] (map bqlToElastic qs)
bqlToElastic (B_Not q) =
  QueryBoolQuery $ mkBoolQuery [] [bqlToElastic q] []
bqlToElastic (B_In _) =
  error "I do not handle \"in\" queries"

---------------------------------------------------------------------------------------------------
-- Executing --------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

queryES' :: BQL -> IO ByteString
queryES' bankQuery =
  let
    searchQ = bqlToElastic bankQuery
    filterQ = Nothing
    query = mkSearch (Just searchQ) filterQ
  in
    fmap responseBody (withBH' $ searchByIndex bankIndex query)


queryES :: BQL -> IO [Value]
queryES =
  fmap (fromRight' . fmap (fromJust . mapM hitSource . hits . searchHits) . eitherDecode) . queryES'

