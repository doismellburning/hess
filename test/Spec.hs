import Test.Hspec
import Hess.Core

pawnlessFEN = "rnbqkbnr/8/8/8/8/8/8/RNBQKBNR w KQkq - 0 1"
simpleRookFEN = "k7/8/8/8/8/8/8/3R4 w - - 0 1"
simpleBishopFEN = "k7/8/8/8/8/8/8/3B4 w - - 0 1"
checkFEN = "rnbqkbnr/8/8/1B6/8/8/8/RNBQK1NR b KQkq - 1 1"

main :: IO ()
main = hspec $ do
	describe "Hess" $ do
		it "writes the FEN it reads" $ do
			let
				fens = [startingFEN, pawnlessFEN, simpleRookFEN, simpleBishopFEN, checkFEN]
				states = map fromFEN fens :: [Maybe GameState]
				readwritten = map (fmap toFEN) states
				in readwritten `shouldBe` (map Just fens)

		it "starts in the right state" $ do
			(toFEN newGame) `shouldBe` startingFEN

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
