{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module SimplIR.DiskIndex.Fielded.Open where

import Control.Monad.IO.Class
import Data.Kind
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Singletons.TH
import Control.Monad.Except

import qualified SimplIR.DiskIndex.Posting as Postings
import qualified SimplIR.DiskIndex.Document as DocMeta
import SimplIR.DiskIndex.Fielded.Types

data FieldedIndex docmeta (fields :: [field]) (postingType :: field -> Type)
    = FieldedIndex { docMetaIndex :: DocMeta.DocIndex docmeta
                   , fieldIndexes :: Rec (Compose Postings.DiskIndex postingType) fields
                   }

open :: forall docmeta field (fields :: [field]) (postingType :: field -> Type).
        (NamedFields field, RecApplicative fields)
     => OnDiskIndex docmeta fields postingType
     -> ExceptT String IO (FieldedIndex docmeta fields postingType)
open idx = do
    let openIndex :: forall (f :: field). ()
                  => Proxy f
                  -> ExceptT String IO (Compose Postings.DiskIndex postingType f)
        openIndex = fmap Compose . ExceptT . Postings.open . getFieldIndexPath idx

    idxs <- rtraverse openIndex (rproxies (Proxy @fields))
    docMeta <- liftIO $ DocMeta.open $ getDocumentIndexPath idx
    return (FieldedIndex docMeta idxs)

rproxies :: forall fields. RecApplicative fields
         => Proxy fields -> Rec Proxy (fields :: [field])
rproxies _ = rpure Proxy

testOpen :: ExceptT String IO (FieldedIndex Char '[ 'DefaultField ] (SingleAttr Int))
testOpen = open (OnDiskIndex "test")

