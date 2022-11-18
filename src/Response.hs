module Response where

import qualified Data.Vector as V
import Lib (Vec5, WordleWord, getLetters)
import Wordle ()

data LetterResponse = Grey | Yellow | Green

instance Show LetterResponse where
  show :: LetterResponse -> String
  show Grey = "X"
  show Yellow = "Y"
  show Green = "G"

newtype Response = MkResponse (Vec5 LetterResponse)

instance Show Response where
  show :: Response -> String
  show (MkResponse responses) = concatMap show (V.toList responses)

getResponse :: Response -> Vec5 LetterResponse
getResponse (MkResponse rs) = rs

-- Take guess, answer and return pattern of grey, yellow and green boxes
respondToGuess :: WordleWord -> WordleWord -> Response
respondToGuess guess answer = MkResponse (V.map convert greenSpots)
  where
    greenSpots :: Vec5 Bool
    greenSpots = V.zipWith (==) (getLetters guess) (getLetters answer)

    convert False = Grey
    convert True = Green
