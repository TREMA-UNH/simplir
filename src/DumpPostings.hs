{-# LANGUAGE RankNTypes #-}

import Data.Binary
import qualified Data.Map as M
import Control.Monad.Trans.Except

import Pipes
import qualified Pipes.Prelude as PP

import qualified BTree.BinaryList as BList
import BTree.BinaryList (BinaryList)
import Types

main :: IO ()
main = do
    let postings = BList.open "postings" :: BinaryList (Term, [Posting [Position]])
    dumpBList postings

    let docIds = BList.open "docids" :: BinaryList (DocumentId, DocumentName)
    dumpBList docIds

    return ()

dumpBList :: (Binary a, Show a) => BinaryList a -> IO ()
dumpBList blist = do
    Right stream <- runExceptT $ BList.stream blist
    res <- runEffect $ stream >-> PP.mapM_ print
    print res