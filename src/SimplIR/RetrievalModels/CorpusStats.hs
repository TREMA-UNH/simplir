{-# LANGUAGE ScopedTypeVariables #-}

module SimplIR.RetrievalModels.CorpusStats
    ( -- * Background statistics
      CorpusStats(..)
    , addCorpusStats
    , CorpusDocCount
    , CorpusTokenCount
    , TermStats(..)
    , TermFreq
    , documentTermStats
    ) where

import Data.Hashable
import Data.Semigroup
import Data.Profunctor
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Control.Foldl as Foldl

type CorpusDocCount = Int
type CorpusTokenCount = Int
type TermFreq = Int

data TermStats = TermStats { documentFrequency :: !Int
                           , termFrequency     :: !Int
                           }
               deriving (Show)

instance Semigroup TermStats where
    TermStats a b <> TermStats x y = TermStats (a+x) (b+y)
instance Monoid TermStats where
    mempty = TermStats 0 0
    mappend = (<>)

data CorpusStats term = CorpusStats { corpusTerms      :: !(HM.HashMap term TermStats)
                                      -- ^ per-term usage statistics
                                    , corpusDocCount   :: !CorpusDocCount
                                      -- ^ total document count
                                    , corpusTokenCount :: !CorpusTokenCount
                                      -- ^ total token count
                                    }

addCorpusStats :: (Hashable term, Eq term)
               => CorpusStats term -> CorpusStats term -> CorpusStats term
addCorpusStats a b =
    CorpusStats { corpusTerms = HM.unionWith mappend (corpusTerms a) (corpusTerms b)
                , corpusDocCount = corpusDocCount a + corpusDocCount b
                , corpusTokenCount = corpusTokenCount a + corpusTokenCount b
                }

-- | A 'Foldl.Fold' over documents (bags of words) accumulating TermStats
documentTermStats :: forall term. (Hashable term, Eq term)
                  => Maybe (HS.HashSet term) -> Foldl.Fold [term] (CorpusStats term)
documentTermStats interestingTerms =
    CorpusStats <$> termStats <*> Foldl.length <*> lmap Prelude.length Foldl.sum
  where
    accumTermStats = Foldl.Fold (HM.unionWith mappend) mempty id
    termStats = lmap toTermStats $ Foldl.handles traverse accumTermStats
    toTermStats :: [term] -> [HM.HashMap term TermStats]
    toTermStats = maybe unfilteredToTermStats filteredToTermStats interestingTerms

    filteredToTermStats :: HS.HashSet term -> [term] -> [HM.HashMap term TermStats]
    filteredToTermStats filterTerms terms =
        [ HM.singleton term (TermStats 1 n)
        | (term, n) <- HM.toList terms'
        , term `HS.member` filterTerms
        ]
      where
        terms' = HM.fromListWith (+) $ zip terms (repeat 1)

    unfilteredToTermStats :: [term] -> [HM.HashMap term TermStats]
    unfilteredToTermStats terms =
        [ HM.singleton term (TermStats 1 n)
        | (term, n) <- HM.toList terms'
        ]
      where
        terms' = HM.fromListWith (+) $ zip terms (repeat 1)
{-# INLINEABLE documentTermStats #-}
