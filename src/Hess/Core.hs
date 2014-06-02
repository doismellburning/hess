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
    gameEnPassant :: EnPassant,
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

bsDeltaPlus :: Board -> BoardSquare -> BSDelta -> Maybe BoardSquare
bsDeltaPlus b (BoardSquare f r) (fd, rd) =
    let
        new = BoardSquare (f `fileDeltaPlus` fd) (r `rankDeltaPlus` rd)
    in
        if' (onBoard b new) (Just new) Nothing

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
    in joinRows $ map rowToFEN $ transpose $ map reverse fenSquares

rowFromFEN :: String -> Maybe [Maybe Piece]
-- ^
--
-- >>> rowFromFEN "8"
-- Just [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
--
-- Ignore the multiple fmaps below, they're just for presentation
-- >>> (fmap (fmap (fmap toFEN))) $ rowFromFEN "3r4"
-- Just [Nothing,Nothing,Nothing,Just "r",Nothing,Nothing,Nothing,Nothing]
--
rowFromFEN s = -- Totally a way of making this nicer...
    let ss = map rowFromFEN' s :: [Maybe [Maybe Piece]]
    -- And now, if any elements of ss are Nothing, we return Nothing,
    -- otherwise we want Just a concatenated list of internals...
    -- in foldl (\x y -> maybe Nothing (\z -> Just $ x ++ z) y) (Just []) ss
    in foldM (\x y -> maybe Nothing (\z -> Just $ x ++ z) y) [] ss

rowFromFEN' :: Char -> Maybe [Maybe Piece]
-- ^Turns a character of board FEN into a series of Maybe Pieces
--
-- The return type is a bit awkward; it's Just a list of Maybe Pieces
-- assuming we parse the Char ok, otherwise it's Nothing
--
-- >>> rowFromFEN' '2'
-- Just [Nothing,Nothing]
--
-- Ignore the multiple fmaps below, they're just for presentation
-- >>> fmap (fmap (fmap toFEN)) $ rowFromFEN' 'r'
-- Just [Just "r"]
rowFromFEN' c
    | isDigit c = Just $ replicate (digitToInt c) Nothing
    | otherwise = (fromFEN [c]) >>= (\x -> Just [Just x]) -- Probably could be nicer with Monoid stuff or sth

boardFromFEN :: String -> Maybe Board
boardFromFEN s =
    let
        rows = split "/" s
        rows' = map rowFromFEN rows :: [Maybe [Maybe Piece]]
        joined = fmap concat $ fmap (map reverse) $ fmap transpose $ sequence rows' :: Maybe [Maybe Piece]
    in fmap (Board . listArray ((boardSquare' "a1"), (boardSquare' "h8"))) joined

isPromotionMove :: GameState -> Move -> Bool
isPromotionMove gameState move' =
    let piece = fromJust $ gameState `pieceAtSquare` (moveStart move')
        endRank = rank $ moveEnd move'
    in pieceType piece == Pawn && (pieceSide piece == Black && endRank == Rank 1 || pieceSide piece == White && endRank == Rank 8)

validateMove :: GameState -> Move -> Maybe MoveError
validateMove = undefined

pieceAtSquare :: GameState -> BoardSquare -> Maybe Piece
-- ^
--
-- >>> let pas g b = pieceAtSquare g (boardSquare' b)
-- >>> fmap toFEN $ pas newGame "a1"
-- Just "R"
-- >>> pas newGame "d5"
-- Nothing
-- >>> fmap toFEN $ pas newGame "h8"
-- Just "r"
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
--
-- >>> let g' = fromJust $ fromFEN "r1r5/1K6/7r/8/8/8/8/8 b - - 0 1" :: GameState
-- >>> stalemate g'
-- False
stalemate = not . canMove

canMove :: GameState -> Bool
-- ^Can the current side make a valid move
canMove g = (length $ allMoves g) > 0

allMoves :: GameState -> [BoardSquare]
-- ^
-- Given a game, what squares can be moved to by the active side
--
-- Returns a pseudo-set (a `nub`-ed list)
--
-- >>> let prettyPrintMoves g = map toFEN $ sort $ allMoves g
--
-- >>> let g = fromJust $ fromFEN "r1r5/1K6/7r/8/8/8/8/8 w - - 0 1" :: GameState
-- >>> prettyPrintMoves g
-- []
--
-- >>> let g' = fromJust $ fromFEN "r6r/8/8/8/8/8/8/8 b - - 0 1" :: GameState
-- >>> prettyPrintMoves g'
-- ["a1","a2","a3","a4","a5","a6","a7","b8","c8","d8","e8","f8","g8","h1","h2","h3","h4","h5","h6","h7"]
--
-- >>> let g'' = fromJust $ fromFEN "8/3k4/8/8/8/8/8/8 b - - 0 1" :: GameState
-- >>> prettyPrintMoves g''
-- []
--
allMoves g =
    let
        (Board b) = gameBoard g
        populatedSquares = map fst $ filter (\(_, x) -> maybe False (isPieceActive g) x) $ assocs b
        moves = concatMap (validEnds g) $ populatedSquares
    in
        nub moves

isPieceActive :: GameState -> Piece -> Bool
isPieceActive g p = (gameActiveSide g) == (pieceSide p)

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


-- Could use a refactor
generateEnds :: Board -> BoardSquare -> BSDelta -> Maybe Int -> Bool -> [BoardSquare]
generateEnds _ _ _ (Just 0) _ = []
generateEnds board start bsDelta limit canTake = generateEnds' board start start bsDelta limit canTake
generateEnds' board realStart start bsDelta limit canTake =
    let
        end = bsDeltaPlus board start bsDelta
    in case end of
        Nothing -> []
        Just bs ->
            let
                endPiece = pieceAtSquare' board bs
            in case endPiece of
                Nothing -> [bs] ++ generateEnds' board realStart bs bsDelta (fmap (\x -> x - 1) limit) canTake
                Just piece -> if' (canTake && (pieceSide $ fromJust $ pieceAtSquare' board realStart) /= (pieceSide piece)) [bs] []


-- TODO Need to refactor these types
moveSquares :: Piece -> Board -> BoardSquare -> [BoardSquare]
moveSquares (Piece Rook _) board start =
    generateEnds board start (0, 1) Nothing True ++
    generateEnds board start (0, -1) Nothing True ++
    generateEnds board start (1, 0) Nothing True ++
    generateEnds board start (-1, 0) Nothing True
moveSquares (Piece Pawn side) board start =
    let
        startRank = rank start
        isOnStartingRank = (side == White && startRank == Rank 2) || (side == Black && startRank == Rank 7)
        delta = if' (side == White) 1 (-1) :: Int
        limit = if' isOnStartingRank 2 1
    in generateEnds board start (0, delta) (Just limit) False
moveSquares _ _ _ = [] -- TODO

threatSquares :: Piece -> Board -> BoardSquare -> [BoardSquare]
threatSquares = moveSquares -- TODO

otherSide :: Side -> Side
otherSide Black = White
otherSide White = Black

move :: GameState -> BoardSquare -> BoardSquare -> Either MoveError GameState
-- ^Applies a move to a GameState if it's valid (returning a Right
-- GameState) or a Left MoveErrror if it's not a valid move
--
-- TODO Implementation currently makes this a lie; massively partial
--
-- >>> let g = newGame
-- >>> let prettyMove game start end = either (\_ -> "Fail :(") toFEN $ move game (boardSquare' start) (boardSquare' end)
-- >>> toFEN g
-- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
-- >>> prettyMove g "a2" "a4"
-- "rnbqkbnr/pppppppp/8/8/P7/8/1PPPPPPP/RNBQKBNR b KQkq a3 0 1"
move g start end =
    let
        b = gameBoard g
        newB = moveBoard b start end
        piece = fromJust $ pieceAtSquare g start -- TODO Ew
        s = gameActiveSide g
        newSide = otherSide s
        newFM = if' (s == Black) (1+) (0+) $ gameFullMove g
        -- TODO Castling
        newEP = EnPassant $
            case (pieceType piece) of
                Pawn -> Just $ if' (pieceSide piece == Black) (fromJust $ bsDeltaPlus b start (0, -1)) (fromJust $ bsDeltaPlus b start (0, 1)) -- TODO EW
                _ -> Nothing
        isCapture = isJust $ pieceAtSquare g end :: Bool
        newHM = if' (isCapture || (pieceType piece == Pawn)) 0 (1 + gameHalfMove g)
    in
        Right g {gameActiveSide = newSide, gameFullMove = newFM, gameBoard = newB, gameEnPassant = newEP, gameHalfMove = newHM}

unsafeMove :: GameState -> BoardSquare -> BoardSquare -> GameState
unsafeMove g a b = either undefined id $ move g a b

moveBoard :: Board -> BoardSquare -> BoardSquare -> Board
-- Hacky, partial
moveBoard (Board b) start end =
    let
        pieceAtStart = fromJust $ pieceAtSquare' (Board b) start
    in
        Board $ b // [(end, Just pieceAtStart), (start, Nothing)]

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
-- [("a1","R"),("a2","P"),("b1","N"),("b2","P"),("c1","B"),("c2","P"),("d1","Q"),("d2","P"),("e1","K"),("e2","P"),("f1","B"),("f2","P"),("g1","N"),("g2","P"),("h1","R"),("h2","P")]
-- >>> map pretty $ activePieces $ fromJust $ fromFEN "8/8/8/8/8/8/8/R7 w - - 0 1"
-- [("a1","R")]
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
