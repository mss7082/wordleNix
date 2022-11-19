module Main (main) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Response
import Wordle

-- Suppose we get XXYXG for a guess of PEACH
peachState :: State
peachState =
  MkState
    ( M.fromList
        [ ('p', LetterNotInWord),
          ('e', LetterNotInWord),
          ('a', LetterNotInLocations (S.fromList [2])),
          ('c', LetterNotInWord),
          ('h', LetterNotInLocations (S.fromList [0, 1, 2, 3]))
        ]
    )

-- Got XXXYY for a guess
unknownState :: State
unknownState =
  MkState
    ( M.fromList
        [ ('l', LetterNotInWord),
          ('i', LetterNotInLocations (S.fromList [3])),
          ('t', LetterNotInLocations (S.fromList [2])),
          ('n', LetterNotInLocations (S.fromList [0, 2, 3, 4])),
          ('u', LetterNotInLocations (S.fromList [1, 2, 3, 4]))
        ]
    )

main :: IO ()
main = do
  print Grey
