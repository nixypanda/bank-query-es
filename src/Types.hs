
module Types
  ( Config(..)
  ) where

import Data.Map (Map)
import Data.Text (Text)

import Database.Bloodhound (BH, IndexName)

data Config a =
  Config
    { withBH' :: BH IO a -> IO a
    , index :: IndexName
    , port :: Int
    , kMap :: Map Text [Text]
    , fMap :: Map Text [Text]
    }


