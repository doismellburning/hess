module Hess.Core where

--import Data.Array
import Data.Maybe

data GameState = GameState Board Side CastlingState EnPassant Int Int

data Board = Array BoardSquare (Maybe Piece)

data Side = Black | White deriving (Enum)

data CastlingState = CastlingState -- TODO

data BoardSquare = BoardSquare File Rank

newtype File = File String
newtype Rank = Rank Int

data Piece = Piece PieceType Side

data PieceType = Pawn deriving (Enum)

data EnPassant = EnPassant (Maybe BoardSquare)

data Move = Move BoardSquare BoardSquare

data MoveError = MoveError -- TODO

class FENable a where
	toFEN :: a -> String
	fromFEN :: String -> Maybe a

instance FENable File where
	toFEN (File f) = f
	fromFEN = undefined

instance FENable Rank where
	toFEN (Rank r) = show r
	fromFEN = undefined

instance FENable BoardSquare where
	toFEN (BoardSquare r f) = (toFEN r) ++ (toFEN f)
	fromFEN = undefined

instance FENable EnPassant where
	toFEN (EnPassant s) = maybe "-" toFEN s
	fromFEN = undefined

instance FENable GameState where
	toFEN = undefined
	fromFEN = undefined

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


