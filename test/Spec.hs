import Test.Hspec
import Hess.Core

import Data.Maybe

pawnlessFEN = "rnbqkbnr/8/8/8/8/8/8/RNBQKBNR w KQkq - 0 1"
simpleRookFEN = "k7/8/8/8/8/8/8/3R4 w - - 0 1"
simpleBishopFEN = "k7/8/8/8/8/8/8/3B4 w - - 0 1"
checkFEN = "rnbqkbnr/8/8/1B6/8/8/8/RNBQK1NR b KQkq - 1 1"

unsafeMove' g a b = unsafeMove g (Move {moveStart = boardSquare' a, moveEnd = boardSquare' b, promotionData = Nothing})

main :: IO ()
main = hspec $ do
    describe "Hess" $ do
        it "writes out a starting game correctly" $ do
            (toFEN newGame) `shouldBe` startingFEN

        it "writes the FEN it reads" $ do
            let
                fens = [startingFEN, pawnlessFEN, simpleRookFEN, simpleBishopFEN, checkFEN]
                states = map fromFEN fens :: [Maybe GameState]
                readwritten = map (fmap toFEN) states :: [Maybe String]
                in (map (fromMaybe ":(") readwritten) `shouldBe` fens

        it "is happy loading a valid BoardSquare" $ do
            let
                bs = fromFEN "e5" :: Maybe BoardSquare
                in (maybe "" toFEN bs) `shouldBe` "e5"

        it "objects to loading an invalid BoardSquare" $ do
            (fromFEN "x9" :: Maybe BoardSquare) `shouldBe` Nothing

        it "loads a side" $ do
            (fromFEN "w" :: Maybe Side) `shouldBe` Just White

        it "loads lacking EnPassant" $ do
            (fromFEN "-" :: Maybe EnPassant) `shouldBe` Just (EnPassant Nothing)

        it "loads a side's EnPassant" $ do
            let
                str = "e5"
                in (fromFEN str :: Maybe EnPassant) `shouldBe` Just (EnPassant (fromFEN str :: Maybe BoardSquare))

        it "generates proper FEN for initial CastlingState" $ do
            let
                cs = CastlingState True True True True
                in (toFEN cs) `shouldBe` "KQkq"

        it "generates proper FEN when no-one can castle" $ do
            let
                cs = CastlingState False False False False
                in (toFEN cs) `shouldBe` "-"

        it "generates proper FEN for some given CastlingState" $ do
            let
                cs = CastlingState False True False True
                in (toFEN cs) `shouldBe` "Qq"

        it "reads and writes the same FEN for CastlingState" $ do
            let
                cs = CastlingState False True True False
                in (fromFEN $ toFEN cs) `shouldBe` Just cs

        it "decodes board FEN with the correct rank/file orderings" $ do
            let
                g = fromJust $ fromFEN simpleRookFEN :: GameState
                in pieceAtSquare g (boardSquare' "d1") `shouldBe` fromFEN "R"

        it "correctly increments the halfmove counter" $ do
            let
                g = newGame
                g' = unsafeMove' g "g1" "f3"
                in (gameHalfMove g') `shouldBe` 1

        it "correctly updates en-passant" $ do
            let
                moves = [("e2", "e4"), ("c7", "c5"), ("g1", "f3"), ("a7", "a5")]
                move' = \g (a,b) -> unsafeMove' g a b
                games = scanl move' newGame moves
                eps = map gameEnPassant games :: [EnPassant]
                eps' = map (\(EnPassant e) -> fmap toFEN e) eps :: [Maybe String]
                in eps' `shouldBe` [Nothing, Just "e3", Just "c6", Nothing, Just "a6"]

        it "correctly updates castling state" $ do
            let
                game = fromJust $ fromFEN pawnlessFEN :: GameState
                move' = \g (a,b) -> unsafeMove' g a b
                moves = [("a1", "a2"), ("e8", "e7"), ("e1", "e2")]
                games = scanl move' game moves
                cs = map (toFEN . gameCastlingState) games
                in cs `shouldBe` ["KQkq", "Kkq", "K", "-"]

        it "handles en-passant correctly" $ do
            let
                game = fromJust $ fromFEN pawnlessFEN
                move' = \g (a,b) -> unsafeMove' g a b
                game' = move' game ("b2", "b4")
                game'' = move' game' ("a4", "b3")
                in (pieceAtSquare game'' (boardSquare' "b4")) `shouldBe` Nothing
