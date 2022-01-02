{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}


module SimplIR.Format.JsonRunQrels where

import Data.Semigroup hiding (All, Any, option)

import Data.Maybe

import qualified SimplIR.Format.TrecRunFile as SimplirRun

import qualified SimplIR.Format.QRel as QRel

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Codec.Compression.GZip as GZip
import Codec.Compression.Zlib.Internal (DecompressError)
import Control.Exception (throwIO, handle)
import System.IO (stderr, hPutStrLn)


newtype SerializedRankingEntry q d = SerializedRankingEntry { unserializeRankingEntry :: SimplirRun.RankingEntry' q d }
    deriving (Show, Generic)

instance (Aeson.FromJSON q, Aeson.FromJSON d) 
        => Aeson.FromJSON (SerializedRankingEntry q d) where
    parseJSON = Aeson.withObject "SerializedRankingEntry" $ \content -> do
        queryId <- content Aeson..: "query"
        documentName <- content Aeson..: "document"
        documentRank <- content Aeson..: "rank"    
        documentScore <- content Aeson..: "score"
        methodName <- fromMaybe "" <$> content Aeson..:? "method"      
        return $ SerializedRankingEntry (SimplirRun.RankingEntry{..})

instance (Aeson.ToJSON q, Aeson.ToJSON d) 
        => Aeson.ToJSON (SerializedRankingEntry q d) where
    toJSON (SerializedRankingEntry (SimplirRun.RankingEntry{..})) =
        Aeson.object $ [ "query" Aeson..= queryId
                       , "document" Aeson..= documentName
                       , "rank" Aeson..= documentRank
                       , "score" Aeson..= documentScore
                       ] <> case methodName of
                                "" -> []
                                name -> ["method" Aeson..= name]
    
parseJsonL :: forall a. Aeson.FromJSON a => BSL.ByteString -> [a]
parseJsonL = zipWith f [1..] . BSL.lines
  where
    f :: Int -> BSL.ByteString -> a
    f !lineNo bs = 
      case Aeson.eitherDecode bs of
        Left err -> error "parseJsonL"
        Right x -> x


readTrecEvalRunFile :: (SimplirRun.QueryId -> q ) -> (SimplirRun.DocumentName -> d)
                  -> FilePath  
                  -> IO([SimplirRun.RankingEntry' q d] )
readTrecEvalRunFile qConv dConv fname =  do
    runData <- SimplirRun.readRunFile fname
    return $ fmap (mapFromRunEntry qConv dConv) runData

parseJsonLRunFile :: forall q d
                 . (Aeson.FromJSON q, Aeson.FromJSON d) 
                 => BSL.ByteString -> [SimplirRun.RankingEntry' q d]
parseJsonLRunFile = map unserializeRankingEntry . parseJsonL

readGzJsonLRunFile ::  forall q d
                 . (Aeson.FromJSON q, Aeson.FromJSON d) 
                 => FilePath -> IO (([SimplirRun.RankingEntry' q d]))
readGzJsonLRunFile fname = do
 putStrLn $ "readGzJsonLRunFile " <> fname   

 handle handleDecompressionError $ do
    bs <- fmap GZip.decompress $ BSL.readFile fname
    let decodeRankingEntry :: BSL.ByteString -> IO (SimplirRun.RankingEntry' q d)
        decodeRankingEntry bs = either fail (return . unserializeRankingEntry ) 
                                $ Aeson.eitherDecode bs
    mapM decodeRankingEntry (BSL.lines bs)
  where
    handleDecompressionError :: DecompressError -> IO [SimplirRun.RankingEntry' q d]
    handleDecompressionError err = do
      hPutStrLn stderr $ (fname ++ ": Decompression error: " ++ show err)
      throwIO err
      

readJsonLRunFile ::  forall q d
                 . (Aeson.FromJSON q, Aeson.FromJSON d) 
                 => FilePath -> IO [SimplirRun.RankingEntry' q d]
readJsonLRunFile fname = do
    bs <- BSL.readFile fname
    return $ parseJsonLRunFile bs

writeJsonLRunFile :: forall q d
                  . (Aeson.ToJSON q, Aeson.ToJSON d)
                  => FilePath -> [SimplirRun.RankingEntry' q d] -> IO()
writeJsonLRunFile fname runEntries = do
    let lines :: [BSL.ByteString]
        lines = fmap (Aeson.encode . SerializedRankingEntry) $ runEntries
    putStrLn  $ "Writing JsonL to "<> fname
    BSL.writeFile fname $ BSL.unlines $ lines


writeGzJsonLRunFile :: forall q d
                  . (Aeson.ToJSON q, Aeson.ToJSON d)
                  => FilePath -> [SimplirRun.RankingEntry' q d] -> IO()
writeGzJsonLRunFile fname runEntries = do
    let lines :: [BSL.ByteString]
        lines = fmap (Aeson.encode . SerializedRankingEntry) $ runEntries
    BSL.writeFile fname 
        $ GZip.compressWith (GZip.defaultCompressParams { GZip.compressLevel = GZip.bestSpeed })  
        $ BSL.unlines $ lines
    putStrLn  $ "Writing JsonL.gz to "<> fname



mapFromRunEntry :: (SimplirRun.QueryId -> q) 
             -> (SimplirRun.DocumentName -> d)
             -> SimplirRun.RankingEntry 
             -> SimplirRun.RankingEntry' q d
mapFromRunEntry qConv dConv SimplirRun.RankingEntry{..} =
    SimplirRun.RankingEntry { queryId = (qConv queryId), documentName = (dConv documentName), .. }

mapToRunEntry :: (q ->  SimplirRun.QueryId) 
             -> (d -> SimplirRun.DocumentName)
             -> SimplirRun.RankingEntry' q d
             -> SimplirRun.RankingEntry 
mapToRunEntry qConv dConv SimplirRun.RankingEntry{..} =
    SimplirRun.RankingEntry { queryId = (qConv queryId), documentName = (dConv documentName), .. }

writeTrecEvalRunFile :: forall q d
                  . (Aeson.ToJSON q, Aeson.ToJSON d)
                  => FilePath 
                  -> [SimplirRun.RankingEntry' q d] 
                  -> (q -> SimplirRun.QueryId ) -> (d -> SimplirRun.DocumentName)
                  -> IO()
writeTrecEvalRunFile fname runEntries qConv dConv =
    SimplirRun.writeRunFile fname $ map (mapToRunEntry qConv dConv) runEntries



--     Qrel 

newtype SerializedQrelEntry q d = SerializedQrelEntry { unserializeQrelEntry :: QRel.Entry q d QRel.IsRelevant}
    deriving (Show, Generic)

instance (Aeson.FromJSON q, Aeson.FromJSON d) 
        => Aeson.FromJSON (SerializedQrelEntry q d) where
    parseJSON = Aeson.withObject "SerializedQrelEntry" $ \content -> do
        queryId <- content Aeson..: "query"
        documentName <- content Aeson..: "document"
        relevance <- content Aeson..: "relevance"    
        return $ SerializedQrelEntry (QRel.Entry{..})

instance (Aeson.ToJSON q, Aeson.ToJSON d) 
        => Aeson.ToJSON (SerializedQrelEntry q d) where
    toJSON (SerializedQrelEntry (QRel.Entry{..})) =
        Aeson.object $ [ "query" Aeson..= queryId
                       , "document" Aeson..= documentName
                       , "relevance" Aeson..= relevance
                       ]   
    

readTrecEvalQrelFile ::  forall q d . (Ord q, Ord d, Show q, Show d)
                    =>  (SimplirRun.QueryId -> q) 
                    -> (SimplirRun.DocumentName -> d) 
                   -> FilePath 
                   -> IO [QRel.Entry q d QRel.IsRelevant]
readTrecEvalQrelFile convQ convD qrelFile = do
    qrelData <- QRel.readQRel qrelFile
                :: IO [QRel.Entry SimplirRun.QueryId SimplirRun.DocumentName QRel.IsRelevant]
    let qrelData' :: [QRel.Entry q d QRel.IsRelevant]
        qrelData' = [ QRel.Entry {queryId = (convQ queryId), documentName = (convD documentName), relevance = relevance} | QRel.Entry {..}  <- qrelData]
    return qrelData'

parseJsonLQrel ::  forall q d 
             . (Aeson.FromJSON q, Aeson.FromJSON d) 
             => BSL.ByteString -> [QRel.Entry q d QRel.IsRelevant]
parseJsonLQrel = map unserializeQrelEntry . parseJsonL

readJsonLQrelFile ::  forall q d 
                 . (Aeson.FromJSON q, Aeson.FromJSON d) 
                 => FilePath -> IO [QRel.Entry q d QRel.IsRelevant]
readJsonLQrelFile fname = do
    bs <- BSL.readFile fname
    return $ parseJsonLQrel bs

readGzJsonLQrelFile ::  forall q d 
                 . (Aeson.FromJSON q, Aeson.FromJSON d) 
                 => FilePath -> IO [QRel.Entry q d QRel.IsRelevant]
readGzJsonLQrelFile fname = do
    bs <- fmap GZip.decompress $ BSL.readFile fname
    return $ parseJsonLQrel bs

writeJsonLQrelFile :: forall q d
                  . (Aeson.ToJSON q, Aeson.ToJSON d)
                  => FilePath -> [QRel.Entry q d QRel.IsRelevant] -> IO()
writeJsonLQrelFile fname qrelEntries = do
    let lines :: [BSL.ByteString]
        lines = fmap (Aeson.encode . SerializedQrelEntry) $ qrelEntries
    BSL.writeFile fname $ BSL.unlines $ lines

writeGzJsonLQrelFile :: forall q d
                  . (Aeson.ToJSON q, Aeson.ToJSON d)
                  => FilePath -> [QRel.Entry q d QRel.IsRelevant] -> IO()
writeGzJsonLQrelFile fname qrelEntries = do
    let lines :: [BSL.ByteString]
        lines = fmap (Aeson.encode . SerializedQrelEntry) $ qrelEntries
    BSL.writeFile fname 
        $ GZip.compressWith (GZip.defaultCompressParams { GZip.compressLevel = GZip.bestSpeed })  
        $ BSL.unlines $ lines


mapQrelEntry :: (q ->  QRel.QueryId) 
             -> (d -> QRel.DocumentName)
             -> QRel.Entry q d QRel.IsRelevant 
             -> QRel.Entry QRel.QueryId QRel.DocumentName QRel.IsRelevant
mapQrelEntry qConv dConv QRel.Entry{..} =
    QRel.Entry { queryId = (qConv queryId), documentName = (dConv documentName), .. }

writeTrecEvalQrelFile :: forall q d
                  . (Aeson.ToJSON q, Aeson.ToJSON d)
                  => FilePath 
                  -> [QRel.Entry q d QRel.IsRelevant] 
                  -> (q -> QRel.QueryId ) -> (d -> QRel.DocumentName)
                  -> IO()
writeTrecEvalQrelFile fname qrelEntries qConv dConv =
    QRel.writeQRel fname $ map (mapQrelEntry qConv dConv) qrelEntries

