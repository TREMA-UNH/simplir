{-# LANGUAGE CPP #-}
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

#if MIN_VERSION_template_haskell(2,17,0)
readStopWords :: FilePath -> Code Q (HS.HashSet T.Text)
readStopWords fname = liftCode $ do
    stopwords <- runIO $ readFile $ stopWordDir </> fname
    examineCode [e|| HS.fromList $ T.lines $ T.pack $$(liftTyped $ stopwords) ||]
#else
readStopWords :: FilePath -> Q (TExp (HS.HashSet T.Text))
readStopWords fname = do
    stopwords <- runIO $ readFile $ stopWordDir </> fname
    [e|| HS.fromList $ T.lines $ T.pack $$(unsafeTExpCoerce $ lift $ stopwords) ||]
#endif
