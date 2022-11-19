module AdvanceState where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Lib (Vec5, WordleWord, getLetters)
import Response (LetterResponse (..), Response, getResponse)
import Wordle (LetterInformation (..), Location, State (..))

advanceState :: State -> WordleWord -> Response -> State
advanceState state word response = state <> newState
  where
    -- a new state reflecting only information in the response
    newState = MkState newMapping

    guessWithResponse :: Vec5 (Char, LetterResponse)
    guessWithResponse = V.zip (getLetters word) (getResponse response)

    newMapping = V.ifoldl go mempty guessWithResponse
      where
        go :: M.Map Char LetterInformation -> Int -> (Char, LetterResponse) -> M.Map Char LetterInformation
        go oldMapping _ (letter, Grey) = M.insert letter LetterNotInWord oldMapping
        go oldMapping index (letter, Yellow) = M.insert letter (LetterNotInLocations (S.singleton index)) oldMapping
        go oldMapping index (letter, Green) = M.insert letter (LetterNotInLocations (S.delete index (S.fromList allLocations))) oldMapping

allLocations :: [Location]
allLocations = [0 .. 4]
