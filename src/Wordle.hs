module Wordle where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Lib (WordleWord, getLetters)

type Location = Int -- Int in the range 0 - 4

data LetterInformation
  = LetterNotInWord
  | LetterNotInLocation (S.Set Location) -- set of places letter cannot appear.

data State = MkState (M.Map Char LetterInformation)

-- Set of all letters that are required, according to the state
requiredLetters :: State -> S.Set Char
requiredLetters (MkState mapping) = M.foldlWithKey' go S.empty mapping
  where
    go :: S.Set Char -> Char -> LetterInformation -> S.Set Char
    go alreadyRequired _ LetterNotInWord = alreadyRequired
    go alreadyRequired currentLetter (LetterNotInLocation {}) = currentLetter `S.insert` alreadyRequired

validWord :: State -> WordleWord -> Bool
validWord state@(MkState mapping) word = noLettersInBadLocations word && allRequiredLetters word
  where
    theRequiredLetters = requiredLetters state

    -- Check whether any letter is in a forbiddden location.
    noLettersInBadLocations :: WordleWord -> Bool
    noLettersInBadLocations word = V.ifoldl' go True (getLetters word)
      where
        go :: Bool -> Int -> Char -> Bool
        go False _ _ = False
        go True index letter = case M.lookup letter mapping of
          -- Letter hasn't been seen before
          Nothing -> True
          -- Letter not in word. Reject!
          Just LetterNotInWord -> False
          Just (LetterNotInLocation excluded_locations) -> not (index `S.member` excluded_locations)

    -- All the required letters are indeed in the word
    allRequiredLetters :: WordleWord -> Bool
    allRequiredLetters word = theRequiredLetters `S.isSubsetOf` lettersInWord word

lettersInWord :: WordleWord -> S.Set Char
lettersInWord word = V.foldl' (flip S.insert) S.empty (getLetters word)

filterWords :: State -> V.Vector WordleWord -> V.Vector WordleWord
filterWords state = V.filter (validWord state)

-- Suppose we get XXYXG for a guess of PEACH
peachState :: State
peachState =
  MkState
    ( M.fromList
        [ ('p', LetterNotInWord),
          ('e', LetterNotInWord),
          ('a', LetterNotInLocation (S.fromList [2])),
          ('c', LetterNotInWord),
          ('h', LetterNotInLocation (S.fromList [0, 1, 2, 3]))
        ]
    )

-- Got XXXYY for a guess
unknownState :: State
unknownState =
  MkState
    ( M.fromList
        [ ('l', LetterNotInWord),
          ('i', LetterNotInLocation (S.fromList [3])),
          ('t', LetterNotInLocation (S.fromList [2])),
          ('n', LetterNotInLocation (S.fromList [0, 2, 3, 4])),
          ('u', LetterNotInLocation (S.fromList [1, 2, 3, 4]))
        ]
    )
