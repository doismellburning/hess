module Hess.Core where

import Data.Array
import Data.Char
import Data.List
import Data.Maybe

data GameState = GameState Board Side CastlingState EnPassant Int Int

data Board = Board (Array BoardSquare (Maybe Piece))

data Side = Black | White deriving (Enum, Eq, Show)

data CastlingState = CastlingState Bool Bool Bool Bool deriving (Eq, Show)

data BoardSquare = BoardSquare File Rank deriving (Eq, Ix, Ord, Show)

newtype File = File Char deriving (Eq, Ix, Ord, Show)
newtype Rank = Rank Int deriving (Eq, Ix, Ord, Show)

data Piece = Piece PieceType Side deriving (Eq, Show)

data PieceType = Pawn deriving (Enum, Eq, Show)

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
        | isJust (fromFEN [f] :: Maybe File) && isJust (fromFEN [r] :: Maybe Rank) = Just $ BoardSquare (File f) (Rank $ digitToInt r)
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
    fromFEN s
        | s == "-" = Just $ CastlingState False False False False
        | otherwise = Just $ CastlingState ('K' `elem` s) ('Q' `elem` s) ('k' `elem` s) ('q' `elem` s) -- TODO Too permissive

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
sideToFEN :: Side -> Char -> Char
sideToFEN White = toUpper
sideToFEN Black = toLower
sideFromFEN :: Char -> Side
sideFromFEN c = if' (isUpper c) White Black

instance FENable Piece where
    toFEN (Piece t s) = map (sideToFEN s) (toFEN t)
    fromFEN = undefined

-- Bodge
instance FENable PieceType where
    toFEN Pawn = "p"
    fromFEN = undefined

chunkList :: Eq a => Int -> [a] -> [[a]]
chunkList size list =
    let
        (pre, post) = splitAt size list
    in if' (post == []) [pre] ([pre] ++ (chunkList size post))

-- Hack :(
boardToFEN :: Board -> String

rowToFEN :: [Maybe String] -> String
-- rowToFEN {...p..P.} == "3p2P1"
-- rowToFEN {........} == "8"
-- rowToFEN {pppppppp} == "pppppppp"
rowToFEN = snd . rowToFEN' (0, "")
rowToFEN' :: (Int, String) -> [Maybe String] -> (Int, String)
rowToFEN' (0, s) [] = (0, s)
rowToFEN' (i, s) [] = (0, s ++ (show i))
rowToFEN' (i, s) (Nothing:xs) = rowToFEN' (i+1, s) xs
rowToFEN' (0, s) ((Just a):xs) = rowToFEN' (0, s++a) xs
rowToFEN' (i, s) ((Just a):xs) = rowToFEN' (0, s ++ (show i) ++ a) xs

boardToFEN (Board board) =
    let
        rows = chunkList 8 $ elems board :: [[Maybe Piece]] -- so now we have a 2D structure
        fenSquares = map (map (fmap toFEN)) rows :: [[Maybe String]] -- and now the elements have maybe been FENified
        joinRows = intercalate "," :: [String] -> String
    in joinRows $ map rowToFEN fenSquares

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


