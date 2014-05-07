module Hess.Core where

import Control.Monad
import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils

data GameState = GameState {
    gameBoard :: Board,
    gameActiveSide :: Side,
    gameCastlingState :: CastlingState,
    gameEnPassanti :: EnPassant,
    gameHalfMove :: Int,
    gameFullMove :: Int
} deriving (Eq, Show)

data Board = Board (Array BoardSquare (Maybe Piece)) deriving (Eq, Show)

onBoard :: Board -> BoardSquare -> Bool
onBoard (Board b) bs = elem bs $ indices b

data Side = Black | White deriving (Enum, Eq, Show)

data CastlingState = CastlingState Bool Bool Bool Bool deriving (Eq, Show)

data BoardSquare = BoardSquare {
    file :: File,
    rank :: Rank
} deriving (Eq, Ix, Ord, Show)

type BSDelta = (Int, Int)

bsDeltaPlus :: BoardSquare -> BSDelta -> BoardSquare
bsDeltaPlus (BoardSquare f r) (fd, rd) = BoardSquare (f `fileDeltaPlus` fd) (r `rankDeltaPlus` rd)

newtype File = File Char deriving (Eq, Ix, Ord, Show)

fileDeltaPlus :: File -> Int -> File
fileDeltaPlus (File f) i = File $ chr $ i + ord f

newtype Rank = Rank Int deriving (Eq, Ix, Ord, Show)

rankDeltaPlus :: Rank -> Int -> Rank
rankDeltaPlus (Rank r) i = Rank $ r + i

data Piece = Piece {
    pieceType :: PieceType,
    pieceSide :: Side
} deriving (Eq, Show)

data PieceType = Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King
    deriving (Enum, Eq, Show)

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

boardSquare' :: String -> BoardSquare -- Unsafe utility function
boardSquare' = fromJust . fromFEN

instance FENable EnPassant where
    toFEN (EnPassant s) = maybe "-" toFEN s
    fromFEN "-" = Just $ EnPassant Nothing
    fromFEN s = fmap (EnPassant . Just) (fromFEN s :: Maybe BoardSquare)

instance FENable GameState where
    toFEN (GameState board active castling enPassant halfMove fullMove) =
        (boardToFEN board) ++ " " ++ (toFEN active) ++ " " ++ (toFEN castling) ++ " " ++ (toFEN enPassant) ++ " " ++ (toFEN halfMove) ++ " " ++ (toFEN fullMove)
    fromFEN s =
        let
            ws = words s
            board = (ws `lindex` 0) >>= boardFromFEN
            active = (ws `lindex` 1) >>= fromFEN :: Maybe Side
            castling = (ws `lindex` 2) >>= fromFEN :: Maybe CastlingState
            enPassant = (ws `lindex` 3) >>= fromFEN :: Maybe EnPassant
            halfMove = (ws `lindex` 4) >>= fromFEN :: Maybe Int
            fullMove = (ws `lindex` 5) >>= fromFEN :: Maybe Int
            allJust = and [isJust board, isJust active, isJust castling, isJust enPassant, isJust halfMove, isJust fullMove] -- Grim but yay totality!
        in if' allJust (Just $ GameState (fromJust board) (fromJust active) (fromJust castling) (fromJust enPassant) (fromJust halfMove) (fromJust fullMove)) Nothing

lindex :: [a] -> Int -> Maybe a
lindex xs i
    | i < length xs = Just $ xs !! i
    | otherwise = Nothing

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
    fromFEN (c:[]) = if' (isDigit c) (Just $ digitToInt c) Nothing
    fromFEN _ = Nothing

-- Hack :(
sideToFEN :: Side -> Char -> Char
sideToFEN White = toUpper
sideToFEN Black = toLower
sideFromFEN :: Char -> Side
sideFromFEN c = if' (isUpper c) White Black

instance FENable Piece where
    toFEN (Piece t s) = [(sideToFEN s) (fenChar t)]
    fromFEN (c:[]) =
        let
            side = sideFromFEN c
            c' = toLower c
            pt = charToPieceType c'
        in maybe Nothing (\t -> Just $ Piece t side) pt
    fromFEN _ = Nothing

fenChar :: PieceType -> Char
-- Always lower-case
-- We're not using FENable because it's not strictly true because of dual
-- representation
fenChar Pawn = 'p'
fenChar Rook = 'r'
fenChar Knight = 'n'
fenChar Bishop = 'b'
fenChar Queen = 'q'
fenChar King = 'k'
charToPieceType c
    | c == 'p' = Just Pawn
    | c == 'r' = Just Rook
    | c == 'n' = Just Knight
    | c == 'b' = Just Bishop
    | c == 'q' = Just Queen
    | c == 'k' = Just King
    | otherwise = Nothing

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
        joinRows = intercalate "/" :: [String] -> String
    in joinRows $ map rowToFEN fenSquares

rowFromFEN :: String -> Maybe [Maybe Piece]
rowFromFEN s = -- Totally a way of making this nicer...
    let ss = map rowFromFEN' s :: [Maybe [Maybe Piece]]
    -- And now, if any elements of ss are Nothing, we return Nothing,
    -- otherwise we want Just a concatenated list of internals...
    -- in foldl (\x y -> maybe Nothing (\z -> Just $ x ++ z) y) (Just []) ss
    in foldM (\x y -> maybe Nothing (\z -> Just $ x ++ z) y) [] ss

rowFromFEN' :: Char -> Maybe [Maybe Piece]
rowFromFEN' c
    | isDigit c = Just $ replicate (digitToInt c) Nothing
    | otherwise = (fromFEN [c]) >>= (\x -> Just [Just x]) -- Probably could be nicer with Monoid stuff or sth

boardFromFEN :: String -> Maybe Board
boardFromFEN s =
    let
        rows = split "/" s
        rows' = map rowFromFEN rows :: [Maybe [Maybe Piece]]
        joined = fmap concat $ sequence rows' :: Maybe [Maybe Piece]
    in fmap (Board . listArray ((boardSquare' "a1"), (boardSquare' "h8"))) joined

isPromotionMove :: GameState -> Move -> Bool
isPromotionMove gameState move =
    let piece = fromJust $ gameState `pieceAtSquare` (moveStart move)
        endRank = rank $ moveEnd move
    in pieceType piece == Pawn && (pieceSide piece == Black && endRank == Rank 1 || pieceSide piece == White && endRank == Rank 8)

validateMove :: GameState -> Move -> Maybe MoveError
validateMove = undefined

pieceAtSquare :: GameState -> BoardSquare -> Maybe Piece
-- ^
--
-- >>> let pas g b = pieceAtSquare g (boardSquare' b)
-- >>> fmap toFEN $ pas newGame "a1"
-- Just "r"
-- >>> pas newGame "d5"
-- Nothing
-- >>> fmap toFEN $ pas newGame "h8"
-- Just "R"
pieceAtSquare g square = pieceAtSquare' (gameBoard g) square

pieceAtSquare' (Board b) square = b ! square

moveStart (Move s _) = s
moveEnd (Move _ e) = e

startingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

newGame :: GameState
newGame = fromJust $ fromFEN startingFEN

stalemate :: GameState -> Bool
-- ^Are we in stalemate, i.e. not in check but unable to move
--
-- >>> stalemate newGame
-- False
--
-- >>> let g = fromJust $ fromFEN "r1r5/1K6/7r/8/8/8/8/8 w - - 0 1" :: GameState
-- >>> stalemate g
-- True
stalemate = not . canMove

canMove :: GameState -> Bool
-- ^Can the current side make a valid move
canMove g = (length $ allMoves g) > 0

-- >>> let g = fromJust $ fromFEN "r1r5/1K6/7r/8/8/8/8/8 w - - 0 1" :: GameState
-- >>> allMoves g
-- []
allMoves g =
    let
        (Board b) = gameBoard g
        -- TODO Filter by activeSide
        populatedSquares = catMaybes $ map (\(x, y) -> maybe Nothing (\_ -> Just x) y) $ assocs b
        moves = concatMap (validEnds g) $ populatedSquares
    in
        moves

validEnds :: GameState -> BoardSquare -> [BoardSquare]
-- ^Given a game and a square containing a piece (TODO encode this
-- assumption in the type system somehow?), return a list of squares to
-- which that piece could move
--
-- Currently partial, will eventually return [] when given square with no
-- piece
--
-- Another "could/should return a set" (c.f. activePieces)
validEnds g start =
    let
        piece = fromJust $ pieceAtSquare g start
        board = gameBoard g
        -- These functions aren't right
        ms = moveSquares piece board start
        ts = threatSquares piece board start
    in ms `union` ts -- FIXME Wrong

safeBang :: Ix i => Array i e -> i -> Maybe e
safeBang a i
    | elem i (indices a) = Just $ a ! i
    | otherwise = Nothing

generateEnds :: Board -> BoardSquare -> BSDelta -> Maybe Int -> Bool -> [BoardSquare]
generateEnds board start bsDelta limit canTake =
    let
        foos = iterate (flip bsDeltaPlus bsDelta) start -- Infinite list of potential endsquares
        randomfn = maybe id take limit -- Convert our Maybe Int into either `id` or `take` to apply limit if it's present
        bars = randomfn foos
        appropriateEnd :: Board -> BoardSquare -> Bool
        appropriateEnd b bs = onBoard b bs && ((isNothing $ pieceAtSquare' b bs) || (canTake)) -- TODO canTake
    in
        takeWhile (appropriateEnd board) bars

moveSquares :: Piece -> Board -> BoardSquare -> [BoardSquare]
moveSquares (Piece Rook _) board start = generateEnds board start (0, 1) Nothing True
moveSquares _ _ _ = [] -- TODO

threatSquares :: Piece -> Board -> BoardSquare -> [BoardSquare]
threatSquares _ _ _ = [] -- TODO

otherSide :: Side -> Side
otherSide Black = White
otherSide White = Black

activePieces :: GameState -> [(BoardSquare, Piece)]
-- ^Returns a list of the active side's pieces, as (BoardSquare, Piece)
-- pairs.
--
-- Strictly, this returns a set (order is irrelevant and
-- implementation-defined, each element should be distinct) but is a list
-- for convenience and convention
--
-- Note, the below may fail due to ordering - TODO Fix this
--
-- >>> let pretty (x, y) = (toFEN x, toFEN y)
-- >>> map pretty $ activePieces newGame
-- [("g1","P"),("g2","P"),("g3","P"),("g4","P"),("g5","P"),("g6","P"),("g7","P"),("g8","P"),("h1","R"),("h2","N"),("h3","B"),("h4","Q"),("h5","K"),("h6","B"),("h7","N"),("h8","R")]
-- >>> map pretty $ activePieces $ fromJust $ fromFEN "8/8/8/8/8/8/8/R7 w - - 0 1"
-- [("h1","R")]
--
activePieces g =
    let
        activeSide = gameActiveSide g
        Board b = gameBoard g
        b' = assocs b :: [(BoardSquare, Maybe Piece)]
        isPieceAndActive :: Maybe Piece -> Bool
        isPieceAndActive Nothing = False
        isPieceAndActive (Just (Piece _ s)) = s == activeSide
        f' = filter (isPieceAndActive . snd) b' :: [(BoardSquare, Maybe Piece)]
        h = map (\(x, y) -> (x, fromJust y)) f' -- This is grim, BUT because `isPieceAndActive Nothing = False` it works. TODO Refactor the last few to use `catMaybes` or similar for niceness
    in h
