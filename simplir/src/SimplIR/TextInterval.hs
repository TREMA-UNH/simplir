{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Codepoint intervals in a text string.
module SimplIR.TextInterval
  ( -- * Intervals
    Interval
  , intervalLength
  , startEndInterval
  , startLenInterval
  , extractInterval

    -- * Annotation
  , annotateText

    -- * Tests
  , tests
  ) where

import Data.Ord
import Data.List
import qualified Data.Text as T

-- | An interval of codepoint offsets. Inclusive of start, exclusive of end.
data Interval = Interval { iStart, iEnd :: !Int }
  deriving (Show, Eq, Ord)

-- | The length of an interval in codepoints.
intervalLength :: Interval -> Int
intervalLength (Interval start end) = end - start

-- | @startEndInterval start end@ is an interval from codepoints @start@ to
-- @end@, exclusive of the end.
--
-- For instance,
--
-- > extractInterval (startEndInterval 2 4) "hello world"
-- "ll"
--
startEndInterval :: Int -> Int -> Interval
startEndInterval = Interval

-- | @startLenInterval start len@ is an interval of length @len@ starting at
-- offset @start@.
--
-- For instance,
--
-- > extractInterval (startLenInterval 2 4) "hello world"
-- "ll o"
--
startLenInterval :: Int -> Int -> Interval
startLenInterval start len = Interval start (start + len)

-- | Extract the given interval from a 'T.Text'.
extractInterval :: Interval -> T.Text -> T.Text
extractInterval int =
    T.take (intervalLength int) . T.drop (iStart int)

-- | Compute annotated spans of text.
annotateText :: Monoid a
             => T.Text -> [(Interval, a)] -> [(T.Text, a)]
annotateText = \text0 intervals ->
    tail $ go 0 [] text0 (sortBy (comparing $ iStart . fst) intervals)
  where
    -- invariants: 
    --
    --   - text == T.drop offset text0
    --
    go :: Monoid a
       => Int              -- the current offset.
       -> [(Interval, a)]  -- "open" annotation intervals
       -> T.Text           -- remaining text
       -> [(Interval, a)]  -- remaining annotation intervals
       -> [(T.Text, a)]
    go !_     openAnns text _
        -- Input text ended
      | T.null text
      = [(T.empty, allAnns openAnns)]

    go offset openAnns text rest
        -- An open interval closes
      | (Interval _ openEnd, _) : _ <- openAnns
      , case rest of
          (Interval start _, _) : _ -> start > openEnd
          []                        -> True
      = let (text0, text1) = T.splitAt (openEnd - offset) text
            openAnns' = dropWhile (\(Interval _ end, _) -> end == openEnd) openAnns
        in (text0, allAnns openAnns) : go openEnd openAnns' text1 rest

    go offset openAnns text (intAnn@(Interval start _, _) : rest)
        -- A new interval opens
      = let (text0, text1) = T.splitAt (start - offset) text
        in (text0, allAnns openAnns) : go start (openAnn intAnn openAnns) text1 rest

        -- No more annotations
    go _      openAnns text [] = [(text, allAnns openAnns)]

    allAnns :: Monoid a => [(Interval, a)] -> a
    allAnns = foldMap snd

    openAnn :: (Interval, a) -> [(Interval, a)] -> [(Interval, a)]
    openAnn new openAnns = sortBy (comparing $ iEnd . fst) (new : openAnns)

testIt :: T.Text -> [(Interval, String)] -> [(T.Text, String)] -> IO ()
testIt text intervals expected
  | text /= foldMap fst expected = putStrLn "Invalid test case"
  | actual /= expected = print actual >> print expected
  | otherwise = return ()
  where actual = annotateText text intervals

test1 :: IO ()
test1 = testIt
    (T.pack "hello world")
    [(Interval 0 5, "a"), (Interval 2 9, "b")]
    [("he","a"),("llo","ab"),(" wor","b"),("ld", "")]

test2 :: IO ()
test2 = testIt
    (T.pack "hello world")
    [(Interval 0 3, "a"), (Interval 6 9, "b")]
    [("hel","a"),("lo ",""),("wor","b"),("ld", "")]

test3 :: IO ()
test3 = testIt
    (T.pack "hello world")
    [(Interval 0 5, "a"), (Interval 4 9, "b")]
    [("hell","a"),("o","ab"),(" wor","b"),("ld","")]

tests :: IO ()
tests = sequence [test1, test2, test3]
