{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module SimplIR.StopWords.Read where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..), unsafeTExpCoerce)
import System.FilePath

import qualified Data.Text as T
import qualified Data.HashSet as HS

stopWordDir :: FilePath
stopWordDir = "data"

readStopWords :: FilePath -> Code Q (HS.HashSet T.Text)
readStopWords fname = liftCode $ do
    stopwords <- runIO $ readFile $ stopWordDir </> fname
    examineCode [e|| HS.fromList $ T.lines $ T.pack $$(liftTyped $ stopwords) ||]
