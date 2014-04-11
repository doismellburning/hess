module Hess.Core where

--import Data.Array
import Data.Char
import Data.Maybe

data GameState = GameState Board Side CastlingState EnPassant Int Int

data Board = Array BoardSquare (Maybe Piece)

data Side = Black | White deriving (Enum)

data CastlingState = CastlingState -- TODO

data BoardSquare = BoardSquare File Rank deriving (Show, Eq)

newtype File = File Char deriving (Show, Eq)
newtype Rank = Rank Int deriving (Show, Eq)

data Piece = Piece PieceType Side

data PieceType = Pawn deriving (Enum)

data EnPassant = EnPassant (Maybe BoardSquare)

data Move = Move BoardSquare BoardSquare

data MoveError = MoveError -- TODO

class FENable a where
	toFEN :: a -> String
	fromFEN :: String -> Maybe a

instance FENable File where
	toFEN (File f) = [f]
	fromFEN = undefined

instance FENable Rank where
	toFEN (Rank r) = show r
	fromFEN = undefined

instance FENable BoardSquare where
	toFEN (BoardSquare f r) = (toFEN f) ++ (toFEN r)
	fromFEN (f:r:[])
		| 'a' <= f && f <= 'h' && '1' <= r && r <= '8' = Just $ BoardSquare (File f) (Rank $ digitToInt r) -- TODO
		| otherwise = Nothing
	fromFEN _ = Nothing

instance FENable EnPassant where
	toFEN (EnPassant s) = maybe "-" toFEN s
	fromFEN = undefined

instance FENable GameState where
	toFEN (GameState board active castling enPassant halfMove fullMove) =
		(boardToFEN board) ++ " " ++ (toFEN active) ++ " " ++ (toFEN castling) ++ " " ++ (toFEN enPassant) ++ " " ++ (toFEN halfMove) ++ " " ++ (toFEN fullMove)
	fromFEN = undefined

instance FENable CastlingState where
	toFEN = undefined
	fromFEN = undefined

instance FENable Side where
	toFEN = undefined
	fromFEN = undefined

instance FENable Int where
	toFEN i = [intToDigit i]
	fromFEN (s:[]) = Just $ digitToInt s
	fromFEN _ = Nothing

-- Hack :(
boardToFEN :: Board -> String
boardToFEN = undefined
boardFromFEN :: String -> Maybe Board
boardFromFEN = undefined

isPromotionMove :: GameState -> Move -> Bool
isPromotionMove = undefined
{-
isPromotionMove gameState move =
	let piece = gameState `pieceAtSquare` (moveStart move)
	in undefined
-}

validateMove :: GameState -> Move -> Maybe MoveError
validateMove = undefined

pieceAtSquare :: GameState -> BoardSquare -> Maybe Piece
pieceAtSquare = undefined

moveStart (Move s _) = s
moveEnd (Move _ e) = e

startingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

newGame :: GameState
newGame = fromJust $ fromFEN startingFEN


