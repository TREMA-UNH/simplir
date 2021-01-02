{-# LANGUAGE ScopedTypeVariables #-}

module SimplIR.RetrievalModels.TfIdf
    ( -- * Scoring
      Score
    , tfIdf
    , tfIdf'
    , tfIdfAccum
    ) where

import Data.Maybe
import Data.Hashable
import Numeric.Log hiding (sum)
import qualified Data.HashMap.Strict as HM
import SimplIR.RetrievalModels.CorpusStats
import qualified Data.HashSet as HS

type Score = Log Double

tfIdf :: (Eq term, Hashable term)
      => CorpusStats term -> term -> TermFreq -> Score
tfIdf stats term tf = tfIdf' (corpusDocCount stats) termStats tf
  where termStats = fromMaybe mempty $ HM.lookup term (corpusTerms stats)

tfIdf' :: CorpusDocCount -> TermStats -> TermFreq -> Score
tfIdf' docCount termStats tf =
    realToFrac tf * log (realToFrac docCount / (1 + realToFrac (documentFrequency termStats)))


-- | Accumulate Tf-IDF scores across a document (for each query term)
tfIdfAccum :: (Eq term, Hashable term)
           => CorpusStats term -> HS.HashSet term -> HM.HashMap term TermFreq -> Score
tfIdfAccum stats queryTerms terms =
  let filteredTerms = HM.filterWithKey (\k _ -> k `HS.member` queryTerms) terms    
  in sum 
     $ fmap (\(term,count) -> tfIdf stats term count) 
     $ HM.toList filteredTerms