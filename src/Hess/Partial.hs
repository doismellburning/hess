module Hess.Partial where

import Data.Maybe

import Hess.Core


-- Partial functions for convenience

pBoardSquare :: String -> BoardSquare
pBoardSquare = fromJust . fromFEN

pGame :: String -> GameState
pGame = fromJust . fromFEN

startingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

newGame :: GameState
newGame = pGame startingFEN
