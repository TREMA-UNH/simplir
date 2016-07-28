{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Query
    ( -- * Sets of queries
      Queries(..)
    , QueryId(..)
      -- * Misc
    , TokenOrPhrase(..)
    , RecordedValueName(..)
    , QueryNodeName(..)
    , FieldName(..)
    , RetrievalModel(..)
    , WikiId(..)
      -- * Query tree
    , QueryNode(..)
    , collectFieldTerms
      -- * Misc
    , kbaTokenise
    ) where

import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import Control.Applicative
import Data.Foldable (fold, toList)
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Aeson.Types as Aeson
import Data.Type.Equality
import Data.Text (Text)

import Numeric.Log
import SimplIR.Types (TokenOrPhrase(..), Position)
import SimplIR.Term as Term
import SimplIR.Tokenise
import SimplIR.RetrievalModels.QueryLikelihood as QL
import qualified SimplIR.TrecStreaming.FacAnnotations as Fac
import Parametric

newtype QueryId = QueryId Text
                deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype WikiId = WikiId Text
               deriving (Show, Eq, Ord, ToJSON, FromJSON)

deriving instance ToJSON Score
deriving instance FromJSON Score

newtype Queries = Queries { getQueries :: M.Map QueryId (QueryNode FieldName) }

instance FromJSON Queries where
    parseJSON = withArray "queries" $ fmap (Queries . fold) . traverse query
      where
        query = withObject "query" $ \o ->
          M.singleton <$> o .: "name" <*> o .: "child"

instance ToJSON Queries where
    toJSON (Queries qs) = toJSON
        [ object [ "name" .= name, "child" .= q ]
        | (name, q) <- M.toList qs
        ]

newtype RecordedValueName = RecordedValueName Text
                          deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype QueryNodeName = QueryNodeName Text
                      deriving (Show, Eq, Ord, ToJSON, FromJSON)

data FieldName a where
    FieldFreebaseIds :: FieldName Fac.EntityId
    FieldText        :: FieldName (TokenOrPhrase Term)

eqFieldName :: FieldName a -> FieldName b -> Maybe (a :~: b)
eqFieldName FieldFreebaseIds FieldFreebaseIds = Just Refl
eqFieldName FieldText FieldText = Just Refl
eqFieldName _ _ = Nothing

deriving instance Show a => Show (FieldName a)
deriving instance Eq (FieldName a)

data RetrievalModel term
    = QueryLikelihood (Parametric (QL.Distribution term -> QL.Smoothing term))

data QueryNode field
    = ConstNode { value :: Parametric Double }
    | SumNode { name         :: Maybe QueryNodeName
              , children     :: [QueryNode field]
              , recordOutput :: Maybe RecordedValueName
              }
    | ProductNode { name         :: Maybe QueryNodeName
                  , children     :: [QueryNode field]
                  , recordOutput :: Maybe RecordedValueName
                  }
    | ScaleNode { name         :: Maybe QueryNodeName
                , scalar       :: Parametric Double
                , child        :: QueryNode field
                , recordOutput :: Maybe RecordedValueName
                }
    | forall term. (Ord term) =>
      RetrievalNode { name           :: Maybe QueryNodeName
                    , retrievalModel :: RetrievalModel term
                    , field          :: field term
                    , terms          :: V.Vector (term, Double)
                    , recordOutput   :: Maybe RecordedValueName
                    }

-- | A helper for writing 'ToJSON' and 'FromJSON' instances for 'QueryNode'.
data QueryTerms field where
    QueryTerms :: (Ord term) => field term -> V.Vector (term, Double) -> QueryTerms field

-- | Since the @field@ and @terms@ JSON fields are part of the same object, just
-- plain 'ToJSON' isn't possible. Hence this hack.
class FieldType field where
    parseQueryTerms :: Text -> Value -> Aeson.Parser (QueryTerms field)
    queryTermsToJSON :: QueryTerms field -> (Text, Value)

instance FieldType FieldName where
    parseQueryTerms field terms =
        case field of
          "freebase_id" -> QueryTerms FieldFreebaseIds <$> parseTermList terms
          "text"        -> QueryTerms FieldText <$> parseTermList terms
          _             -> fail "invalid field"
    queryTermsToJSON (QueryTerms field terms) =
        case field of
          FieldFreebaseIds -> ("freebase_ids", toJSON terms)
          FieldText        -> ("text", toJSON terms) -- FIXME: encode terms correctly

parseTermList :: FromJSON term => Value -> Aeson.Parser (V.Vector (term, Double))
parseTermList = withArray "term list" $ mapM term
  where
    term val = weighted val <|> unweighted val
    unweighted val = ((\x -> (x, 1)) <$> parseJSON val)
                  <|> Aeson.typeMismatch "term" val
    weighted = withObject "weighted term" $ \t ->
      (,) <$> t .: "term" <*> t .: "weight"

instance (FieldType field) => FromJSON (QueryNode field) where
    parseJSON = withObject "query node" $ \o ->
      let nodeName = fmap QueryNodeName <$> o .:? "name"
          record :: Aeson.Parser (Maybe RecordedValueName)
          record = do
              v <- o .:? "record" .!= Bool False
              case v of
                Bool True   -> Just <$> o .: "name"
                Bool False  -> return Nothing
                String name -> return $ Just $ RecordedValueName name
                _           -> fail $ "unknown value for 'record': "++show v

          constNode = ConstNode <$> o .: "value"

          aggregatorNode = do
              op <- o .: "op"
              case op :: String of
                "product" -> ProductNode <$> nodeName <*> o .: "children" <*> record
                "sum"     -> SumNode <$> nodeName <*> o .: "children" <*> record
                _         -> fail "Unknown aggregator node type"

          scaleNode = ScaleNode
              <$> nodeName
              <*> o .: "scalar"
              <*> o .: "child"
              <*> record

          retrievalNode = do
              fieldName <- o .: "field"
              terms <- o .: "terms"
              QueryTerms field terms <- parseQueryTerms fieldName terms
              model <- o .: "retrieval_model"
              RetrievalNode <$> nodeName
                            <*> pure model
                            <*> pure field
                            <*> pure terms
                            <*> record

      in do ty <- o .: "type"
            case ty :: String of
              "aggregator" -> aggregatorNode
              "constant" -> constNode
              "scale" -> scaleNode
              "scoring_model" -> retrievalNode
              _ -> fail "Unknown node type"

collectFieldTerms :: FieldName term -> QueryNode FieldName -> [term]
collectFieldTerms _ ConstNode {..}     = []
collectFieldTerms f SumNode {..}       = foldMap (collectFieldTerms f) children
collectFieldTerms f ProductNode {..}   = foldMap (collectFieldTerms f) children
collectFieldTerms f ScaleNode {..}     = collectFieldTerms f child
collectFieldTerms f RetrievalNode {..}
  | Just Refl <- field `eqFieldName` f = map fst $ toList terms
  | otherwise                          = []

instance ToJSON (RetrievalModel term) where
    toJSON (QueryLikelihood smoothing) = object
      [ "type" .= str "ql"
      , "smoothing" .= str undefined -- TODO
        -- case smoothing of
        --   Dirichlet mu _ -> object [ "type" .= "dirichlet"
        --                            , "mu" .= mu
        --                            ]
        --   JelinekMercer alpha_f alpha_b -> object [ "type" .= "jm"
        --                                           , "alpha_foreground" .= alpha_f
        --                                           , "alpha_background" .= alpha_b
        --                                           ]
      ]

instance FromJSON (RetrievalModel term) where
    parseJSON = withObject "retrieval model" $ \o -> do
        modelType <- o .: "type"
        case modelType of
          "ql" -> do
              s <- o .: "smoothing"
              smoothingType <-  s .: "type"
              QueryLikelihood <$> case smoothingType :: String of
                  "dirichlet" -> pure . Dirichlet <$> s .: "mu"
                  "jm"        -> do
                      fgP <- s .: "alpha_foreground" :: Aeson.Parser (Parametric Double)
                      bgP <- s .: "alpha_background" :: Aeson.Parser (Parametric Double)
                      let alpha :: Parametric (Log Double)
                          alpha = (\fg bg -> realToFrac $ fg / (fg + bg)) <$> fgP <*> bgP
                      pure (JelinekMercer <$> alpha)
                  _           -> fail $ "Unknown smoothing method "++smoothingType
          _  -> fail $ "Unknown retrieval model "++modelType

instance (FieldType field) => ToJSON (QueryNode field) where
    toJSON (ConstNode {..}) = object
        [ "type"     .= str "constant"
        , "value"    .= value
        ]
    toJSON (SumNode {..}) = object
        $ withName name
        [ "type"     .= str "aggregator"
        , "op"       .= str "sum"
        , "children" .= children
        ]
    toJSON (ProductNode {..}) = object
        $ withName name
        [ "type"     .= str "aggregator"
        , "op"       .= str "product"
        , "name"     .= name
        , "children" .= children
        ]
    toJSON (ScaleNode {..}) = object
        $ withName name
        [ "type"     .= str "weight"
        , "name"     .= name
        , "child"    .= child
        ]
    toJSON (RetrievalNode {..}) = object
        $ case queryTermsToJSON (QueryTerms field terms) of
            (fieldVal, termsVal) ->
              withName name
              [ "type"     .= str "scoring_model"
              , "retrieval_model" .= retrievalModel
              , "field"           .= fieldVal
              , "terms"           .= termsVal
              ]

str :: String -> String
str = id

withName :: Maybe QueryNodeName -> [Aeson.Pair] -> [Aeson.Pair]
withName (Just name) = (("name" .= name) :)
withName _           = id

-- TODO This doesn't really belong here
kbaTokenise :: T.Text -> [(T.Text, Position)]
kbaTokenise =
    tokeniseWithPositions . T.map killPunctuation
  where
    killPunctuation c
      | c `HS.member` chars = ' '
      | otherwise           = c
      where chars = HS.fromList "\t\n\r;\"&/:!#?$%()@^*+-,=><[]{}|`~_`"
