module TennisSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Tennis

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "playerOnePointWin" $ do
    it "increments the point" $ do
      let game = InProgressGame (Love, Love)
      playerOnePointWin game `shouldBe` InProgressGame (Fifteen, Love)
      
    it "does nothing for a complete game" $ do
      playerOnePointWin CompleteGame `shouldBe` CompleteGame

  describe "playerTwoPointWin" $ do
    it "increments the point" $ do
      let game = InProgressGame (Love, Love)
      playerTwoPointWin game `shouldBe` InProgressGame (Love, Fifteen)
      
    it "does nothing for a complete game" $ do
      playerTwoPointWin CompleteGame `shouldBe` CompleteGame
