{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types where

import Data.String
import Data.Hashable
import Data.Binary
import GHC.Generics
import Control.DeepSeq
import qualified Data.ByteString.Short as BS.S
import qualified Data.Text as T
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as VU
import Test.QuickCheck

newtype DocumentId = DocId Int
                   deriving (Show, Eq, Ord, Enum, Binary)

instance Arbitrary DocumentId where
    arbitrary = DocId . getPositive <$> arbitrary

derivingUnbox "DocumentId"
  [t| DocumentId -> Int |]
  [| \(DocId n) -> n |]
  [| DocId |]

newtype DocumentName = DocName BS.S.ShortByteString
                     deriving (Show, Eq, Ord, Binary)

-- | An interval within a discrete *.
data Span = Span { begin, end :: !Int }
          deriving (Eq, Ord, Show, Generic)

instance Binary Span

derivingUnbox "Span"
  [t| Span -> (Int, Int) |]
  [| \(Span a b) -> (a, b) |]
  [| \(a, b) -> Span a b |]

-- | A position within a tokenized document
data Position = Position { charOffset :: !Span
                         , tokenN     :: !Int
                         }
              deriving (Eq, Ord, Show, Generic)

instance Binary Position
instance NFData Position where
    rnf (Position {}) = ()

derivingUnbox "Position"
  [t| Position -> (Span, Int) |]
  [| \(Position a b) -> (a, b) |]
  [| \(a, b) -> Position a b |]

newtype Term = Term {getTerm :: T.Text}
             deriving (Eq, Ord, Show, NFData, Hashable, IsString, Binary)

instance Arbitrary Term where
    arbitrary = Term . T.pack <$> vectorOf 3 (choose ('a','e'))

toCaseFold :: Term -> Term
toCaseFold (Term t) = Term $ T.toCaseFold t
{-# INLINE toCaseFold #-}

data Posting a = Posting { postingDocId :: !DocumentId, postingBody :: !a }
               deriving (Show, Functor, Generic)

instance Arbitrary p => Arbitrary (Posting p) where
    arbitrary = Posting <$> arbitrary <*> arbitrary

-- | Comparing first on the 'DocumentId'
deriving instance Ord a => Ord (Posting a)
deriving instance Eq a => Eq (Posting a)

instance Binary a => Binary (Posting a)
instance NFData a => NFData (Posting a) where
    rnf (Posting _ x) = rnf x

derivingUnbox "Posting"
  [t| forall a. VU.Unbox a => Posting a -> (DocumentId, a) |]
  [| \(Posting a b) -> (a, b) |]
  [| \(a, b) -> Posting a b |]

type BoolPosting = Posting ()
type TermFreqPosting = Posting Int
type PositionalPosting = Posting (VU.Vector Position)

-- | The length of a document in tokens.
newtype DocumentLength = DocLength Int
                       deriving (Eq, Ord, Show, Binary)
