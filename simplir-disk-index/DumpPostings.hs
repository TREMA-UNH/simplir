{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Monoid
import qualified SimplIR.DiskIndex as DiskIndex
import qualified SimplIR.DiskIndex.Posting2 as PostingIdx
import qualified SimplIR.Term as Term
import           SimplIR.Types
import Options.Applicative

args :: Parser ( DiskIndex.DiskIndexPath Term.Term (DocumentName, DocumentLength) [Position]
               , [String]
               )
args =
    (,)
      <$> option (DiskIndex.DiskIndexPath <$> str) (long "index" <> short 'i' <> help "index directory")
      <*> many (option str (long "term" <> short 't' <> help "term"))

main :: IO ()
main = do
    (index, terms) <- execParser $ info (helper <*> args) mempty
    idx <- DiskIndex.open index

    let toDocName :: DocumentId -> String
        toDocName = maybe "none" show . flip DiskIndex.lookupDoc idx
        showTermPostings :: Term.Term -> [Posting [Position]] -> String
        showTermPostings term postings =
            show term ++ "\n"
            ++ unlines [ "  "++toDocName (postingDocId p) ++ " " ++ show (length $ postingBody p)
                       | p <- postings
                       ]
    if null terms
      then
        forM_ (PostingIdx.toPostingsLists $ DiskIndex.postingIdx idx) $ \(term, postings) ->
          putStrLn $ showTermPostings term postings
      else
        forM_ terms $ \term ->
          let term' = Term.fromString term
          in case DiskIndex.lookupPostings' term' idx of
               Just postings -> putStrLn $ showTermPostings term' postings
               Nothing       -> return ()
    return ()
