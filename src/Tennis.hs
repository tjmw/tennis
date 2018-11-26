 module Tennis
    ( Point(..), Score(..), Game(..), playerOnePointWin, playerTwoPointWin
    ) where

data Point = Love | Fifteen | Thirty | Forty | Advantage deriving (Eq, Show)
type Score = (Point, Point)
data Game = InProgressGame Score | CompleteGame deriving (Eq, Show)

playerOnePointWin :: Game -> Game
playerOnePointWin game =
  case game of
      InProgressGame score -> InProgressGame (incrementP1Score score)
      CompleteGame -> CompleteGame

playerTwoPointWin :: Game -> Game
playerTwoPointWin game =
  case game of
      InProgressGame score -> InProgressGame (incrementP2Score score)
      CompleteGame -> CompleteGame

incrementP1Score :: Score -> Score
incrementP1Score (Love, s2) = (Fifteen, s2)
incrementP1Score (Fifteen, s2) = (Thirty, s2)
incrementP1Score (Thirty, s2) = (Forty, s2)
incrementP1Score (Forty, s2) = (Forty, s2)
incrementP1Score (Advantage, s2) = (Advantage, s2)

incrementP2Score :: Score -> Score
incrementP2Score (p1, p2)
    = let (newP2, newP1) = incrementP1Score (p2, p1)
      in  (newP1, newP2)
