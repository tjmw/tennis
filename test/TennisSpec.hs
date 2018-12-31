module TennisSpec (main, spec) where

import Test.Hspec

import Tennis

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "serverScores" $ do
    it "Love -> Fifteen" $ do
      let score = Score (Love, Love)
      serverScores score `shouldBe` Score (Fifteen, Love)

    it "Fifteen -> Thirty" $ do
      let score = Score (Fifteen, Love)
      serverScores score `shouldBe` Score (Thirty, Love)

    it "Thirty -> Forty" $ do
      let score = Score (Thirty, Fifteen)
      serverScores score `shouldBe` Score (Forty, Fifteen)

    it "Deuce -> Advantage" $ do
      let score = Deuce
      serverScores score `shouldBe` ServerAdvantage

    it "Advantage -> win" $ do
      let score = ServerAdvantage
      serverScores score `shouldBe` ServerWin

    it "Back to deuce" $ do
      let score = ReceiverAdvantage
      serverScores score `shouldBe` Deuce

    it "To deuce" $ do
      let score = Score (Thirty, Forty)
      serverScores score `shouldBe` Deuce

  describe "receiverScores" $ do
    it "Love -> Fifteen" $ do
      let score = Score (Love, Love)
      receiverScores score `shouldBe` Score (Love, Fifteen)

    it "Fifteen -> Thirty" $ do
      let score = Score (Love, Fifteen)
      receiverScores score `shouldBe` Score (Love, Thirty)

    it "Thirty -> Forty" $ do
      let score = Score (Fifteen, Thirty)
      receiverScores score `shouldBe` Score (Fifteen, Forty)

    it "Deuce -> Advantage" $ do
      let score = Deuce
      receiverScores score `shouldBe` ReceiverAdvantage

    it "Advantage -> win" $ do
      let score = ReceiverAdvantage
      receiverScores score `shouldBe` ReceiverWin

    it "Back to deuce" $ do
      let score = ServerAdvantage
      receiverScores score `shouldBe` Deuce

    it "To deuce" $ do
      let score = Score (Forty, Thirty)
      receiverScores score `shouldBe` Deuce
