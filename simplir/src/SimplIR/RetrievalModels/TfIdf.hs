{-# LANGUAGE ScopedTypeVariables #-}

module SimplIR.RetrievalModels.TfIdf
    ( -- * Scoring
      Score
    , tfIdf
    , tfIdf'
    , tfIdfAccum
    
    -- * TF-IDF variants:
    , tfIdf_log_tf_smoothed_idf
    -- ^ log tf / smoothed idf
    , tfIdf_raw_tf_smoothed_idf
    -- ^ raw tf / smoothed idf
    ) where

import Data.Maybe
import Data.Hashable
import Numeric.Log hiding (sum)
import qualified Data.HashMap.Strict as HM
import SimplIR.RetrievalModels.CorpusStats
    ( CorpusStats(corpusTerms, corpusDocCount),
      TermStats(documentFrequency),
      TermFreq,
      CorpusDocCount )
import qualified Data.HashSet as HS

type Score = Log Double

type TfIdfVariant = CorpusDocCount -> TermStats -> TermFreq -> Score

tfIdf :: (Eq term, Hashable term)
      => TfIdfVariant -> CorpusStats term -> term -> TermFreq -> Score
tfIdf tfIdfVariant stats term tf =tfIdfVariant (corpusDocCount stats) termStats tf
  where termStats = fromMaybe mempty $ HM.lookup term (corpusTerms stats)



tfIdf' = tfIdf_log_tf_smoothed_idf

-- | Log-Variant of Tf-IDF that uses:
--   * log tf: log(1+tf)
--   * smothed idf:  log (N / (nt +1)) +1
tfIdf_log_tf_smoothed_idf :: CorpusDocCount -> TermStats -> TermFreq -> Score
tfIdf_log_tf_smoothed_idf  docCount termStats tf =
    log (realToFrac tf + 1.0)
    * log (realToFrac docCount / (1 + realToFrac (documentFrequency termStats)))



-- | Raw-Variant of Tf-IDF that uses:
--  * raw count tf: tf
--  * inverse document frequency smooth: log (N / (nt +1)) +1
tfIdf_raw_tf_smoothed_idf :: CorpusDocCount -> TermStats -> TermFreq -> Score
tfIdf_raw_tf_smoothed_idf docCount termStats tf =
    realToFrac tf 
    * log (realToFrac docCount / (1 + realToFrac (documentFrequency termStats)))


-- | Accumulate Tf-IDF scores across a document (for each query term)
tfIdfAccum :: (Eq term, Hashable term)
           => TfIdfVariant -> CorpusStats term -> HS.HashSet term -> HM.HashMap term TermFreq -> Score
tfIdfAccum tfIdfVariant stats queryTerms terms =
  let filteredTerms = HM.filterWithKey (\k _ -> k `HS.member` queryTerms) terms    
  in sum 
     $ fmap (\(term,count) -> tfIdf tfIdfVariant stats term count) 
     $ HM.toList filteredTerms