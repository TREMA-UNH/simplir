{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SimplIR.Types.Relevance
    ( -- * Graded relevance
      GradedRelevance(..)
      -- * Binary relevance
    , IsRelevant(..)
    ) where

import Data.Hashable
import Control.DeepSeq
import GHC.Generics
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson

-- | Graded relevance judgement
newtype GradedRelevance = GradedRelevance {unGradedRelevance:: Int}
                        deriving stock (Eq, Ord, Show)
                        deriving newtype (Hashable)



instance Aeson.ToJSON GradedRelevance where
    toJSON (GradedRelevance r) = Aeson.toJSON (r::Int)

instance Aeson.FromJSON GradedRelevance where
    parseJSON x = do
      result <- Aeson.parseJSON x
                :: Aeson.Parser Int
      return $ GradedRelevance result


-- | Binary relevance judgement
data IsRelevant = NotRelevant | Relevant
                        deriving stock (Eq, Ord, Show, Generic)
                        deriving anyclass (Hashable, NFData)

instance Aeson.ToJSON IsRelevant where
    toJSON Relevant = Aeson.toJSON (1::Int)
    toJSON NotRelevant = Aeson.toJSON (0::Int)

instance Aeson.FromJSON IsRelevant where
    parseJSON x = do
      result <- Aeson.parseJSON x
                :: Aeson.Parser Int
      case result of
        1 -> return $ Relevant
        0 -> return $ NotRelevant
        x -> fail $ "Cannot parse IsRelevant from "<> show x<> " only supports 1 (Relevant) or 0 (NotRelevant)."          

    
