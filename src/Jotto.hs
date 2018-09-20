module Jotto
  ( Guess(..)
  , Class
  , ClassMap
  -- * Loading words
  , buildClassMap
  , classMapWordCount
  , GuessState
  , guessState
  -- * Stats
  , guesses
  , possible
  , classesTotal
  , wordsTotal
  , classesLeft
  , wordsLeft
  -- * The Algorithm
  , addGuess
  , removeGuess
  , resetGuesses
  , nextGuesses
  ) where

import           Data.Char
import           Data.List

import           Control.Parallel.Strategies
import qualified Data.Map                    as M
import qualified Data.Set                    as S

data Guess = Guess String Int
  deriving (Show)

-- type Word = String -- Can't do that, because a Word already exists in the prelude
type Class = S.Set Char
type ClassMap = M.Map Class [String]

alphabet :: Class
alphabet = S.fromList ['a'..'z']

{- Loading words -}

toClass :: Maybe Int -> String -> Maybe Class
toClass Nothing  word = Just $ S.fromList word
toClass (Just n) word =
  let set = S.intersection (S.fromList word) alphabet
  in  if length word == n && S.size set == n
        then Just set
        else Nothing

preprocessWords :: [String] -> [String]
preprocessWords = map head . group . sort . map (map toLower)

buildClassMap :: Maybe Int -> [String] -> ClassMap
buildClassMap n = foldr addWord M.empty . preprocessWords
  where
    addWord :: String -> ClassMap -> ClassMap
    addWord word cmap =
      case toClass n word of
        Nothing  -> cmap
        Just set -> M.alter (appendWord word) set cmap
    appendWord :: String -> Maybe [String] -> Maybe [String]
    appendWord word Nothing   = Just [word]
    appendWord word (Just ws) = Just $ word : ws

classMapWordCount :: ClassMap -> Int
classMapWordCount = sum . M.map length

{- Basic guessing operations -}

data GuessState = GuessState
  { guessed  :: [Guess]
  , allWords :: ClassMap
  } deriving (Show)

score :: Class -> Class -> Int
score a b = S.size $ S.intersection a b

{-
maxScore :: Class -> Class -> Int
maxScore a b = min (S.size a) (S.size b)
-}

filterClassMap :: Guess -> ClassMap -> ClassMap
filterClassMap (Guess word n) cmap =
  let c = S.fromList word
  in  M.filterWithKey (\k _ -> score c k == n) cmap

{-
filterClassSet :: Guess -> S.Set Class -> S.Set Class
filterClassSet (Guess word n) cset =
  let c = S.fromList word
  in  S.filter (\e -> score c e == n) cset
-}

possibleWords :: GuessState -> ClassMap
possibleWords g = foldr filterClassMap (allWords g) (guessed g)

guessState :: ClassMap -> GuessState
guessState cmap = GuessState{guessed=[], allWords=cmap}

addGuess :: Guess -> GuessState -> GuessState
addGuess guess g = g{guessed=guess : guessed g}

removeGuess :: String -> GuessState -> GuessState
removeGuess w g =
  let shouldStay (Guess word _) = word /= w
  in  g{guessed=filter shouldStay (guessed g)}

resetGuesses :: GuessState -> GuessState
resetGuesses g = g{guessed=[]}

{- Stats -}

guesses :: GuessState -> [Guess]
guesses = guessed

possible :: GuessState -> [[String]]
possible = M.elems . possibleWords

classesTotal :: GuessState -> Int
classesTotal = M.size . allWords

wordsTotal :: GuessState -> Int
wordsTotal = classMapWordCount . allWords

classesLeft :: GuessState -> Int
classesLeft = M.size . possibleWords

wordsLeft :: GuessState -> Int
wordsLeft = classMapWordCount . possibleWords

{- Behold, THE ALGORITHM -}

-- Based on:
-- A - set of all classes
-- S - set of all possible solution classes
--
-- 1. for each class a in A, calculate the worst-case score
--
-- 1.1 for each possible score, calculate how many WORDS (not classes) from S yield this score
-- 1.2 select the maximum score (i. e. the score that eliminates the least words s)
-- 1.3 subtract the maximum score from the size of S to obtain the worst-case score
--
-- 2. sort A by the following criteria (more important first):
--
--   * highest worst-case score (higher is better)
--   * a is element of S        (if a is in S, it's better)
--   * word count of a          (higher is better)

worstCase :: Class -> M.Map Class Int -> Int
worstCase c cmap =
  let solutions = M.mapKeysWith (+) (score c) cmap
      maxHitCount = maximum solutions
      minEliminated = sum cmap - maxHitCount
  in  minEliminated

nextGuesses :: GuessState -> [(Int, [String])]
nextGuesses g =
  let pwords = possibleWords g
      lpwords = M.map length pwords
      pclasses = M.keysSet pwords
      options1 = M.assocs $ allWords g
      options2 = parMap rdeepseq (\(set, w) -> (set, worstCase set lpwords, w)) options1
      options3 = sortOn (\(_,_,w) -> length w) options2
      options4 = sortOn (\(s,_,_) -> S.member s pclasses) options3
      options5 = sortOn (\(_,c,_) -> c) options4
  in  map (\(_,c,w) -> (c,w)) options5
