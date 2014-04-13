module Hess.Core where

--import Data.Array
import Data.Char
import Data.Maybe

data GameState = GameState Board Side CastlingState EnPassant Int Int

data Board = Array BoardSquare (Maybe Piece)

data Side = Black | White deriving (Enum, Eq, Show)

data CastlingState = CastlingState Bool Bool Bool Bool

data BoardSquare = BoardSquare File Rank deriving (Show, Eq)

newtype File = File Char deriving (Show, Eq)
newtype Rank = Rank Int deriving (Show, Eq)

data Piece = Piece PieceType Side

data PieceType = Pawn deriving (Enum)

data EnPassant = EnPassant (Maybe BoardSquare) deriving (Show, Eq)

data Move = Move BoardSquare BoardSquare

data MoveError = MoveError -- TODO

class FENable a where
	toFEN :: a -> String
	fromFEN :: String -> Maybe a

instance FENable File where
	toFEN (File f) = [f]
	fromFEN (f:[])
		| 'a' <= f && f <= 'h' = Just $ File f
		| otherwise = Nothing
	fromFEN _ = Nothing

instance FENable Rank where
	toFEN (Rank r) = show r
	fromFEN (r:[])
		| '1' <= r && r <= '8' = Just $ Rank $ digitToInt r
		| otherwise = Nothing
	fromFEN _ = Nothing

instance FENable BoardSquare where
	toFEN (BoardSquare f r) = (toFEN f) ++ (toFEN r)
	fromFEN (f:r:[])
		| isJust (fromFEN [f] :: Maybe File) && isJust (fromFEN [r] :: Maybe Rank) = Just $ BoardSquare (File f) (Rank $ digitToInt r) -- TODO
		| otherwise = Nothing
	fromFEN _ = Nothing

instance FENable EnPassant where
	toFEN (EnPassant s) = maybe "-" toFEN s
	fromFEN "-" = Just $ EnPassant Nothing
	fromFEN s = fmap (EnPassant . Just) (fromFEN s :: Maybe BoardSquare)

instance FENable GameState where
	toFEN (GameState board active castling enPassant halfMove fullMove) =
		(boardToFEN board) ++ " " ++ (toFEN active) ++ " " ++ (toFEN castling) ++ " " ++ (toFEN enPassant) ++ " " ++ (toFEN halfMove) ++ " " ++ (toFEN fullMove)
	fromFEN = undefined

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

instance FENable CastlingState where
    toFEN (CastlingState wk wq bk bq) =
        let
            handleEmpty x = if' (x == "") "-" x
        in handleEmpty $ concat $ [(if' wk "K" ""), (if' wq "Q" ""), (if' bk "k" ""), (if' bq "q" "")]
    fromFEN = undefined

instance FENable Side where
	toFEN Black = "b"
	toFEN White = "w"
	fromFEN "w" = Just White
	fromFEN "b" = Just Black
	fromFEN _ = Nothing

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


