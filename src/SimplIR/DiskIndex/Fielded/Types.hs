{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module SimplIR.DiskIndex.Fielded.Types
    ( NamedFields(..)
    , OnDiskIndex(..)
    , getFieldIndexPath
    , getDocumentIndexPath
    ) where

import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Kind
import Data.Proxy
import System.FilePath

import qualified SimplIR.DiskIndex.Posting as Postings
import qualified SimplIR.DiskIndex.Document as DocMeta

class NamedFields f where
    fieldName :: Proxy (a :: f) -> String

newtype OnDiskIndex docmeta (fields :: [field]) (postingType :: field -> Type)
    = OnDiskIndex FilePath
    deriving (Show, Eq, Ord)

getFieldIndexPath
    :: forall (f :: field) (fields :: [field]) docmeta (postingType :: field -> Type).
       (NamedFields field)
    => OnDiskIndex docmeta fields postingType -> Proxy f
    -> Postings.OnDiskIndex (postingType f)
getFieldIndexPath (OnDiskIndex root) fld =
    Postings.OnDiskIndex $ root </> fieldName fld

-- | Get the path to the document metadata index
getDocumentIndexPath
    :: OnDiskIndex docmeta fields postingType
    -> DocMeta.OnDiskIndex docmeta
getDocumentIndexPath (OnDiskIndex root) =
    DocMeta.OnDiskIndex $ root </> "documents"
