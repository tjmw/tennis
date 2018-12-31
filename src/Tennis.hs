 module Tennis
    ( Point(..), Score(..), serverScores, receiverScores
    ) where

data Point = Love | Fifteen | Thirty | Forty deriving (Eq, Show)
data Score = Score (Point, Point) | Deuce | ServerAdvantage | ReceiverAdvantage
           | ServerWin | ReceiverWin deriving (Eq, Show)

serverScores :: Score -> Score
serverScores score =
  case score of
    ServerAdvantage -> ServerWin
    ReceiverAdvantage -> Deuce
    Deuce -> ServerAdvantage
    Score (Thirty, Forty) -> Deuce
    Score (Forty, _) -> ServerWin
    Score (Thirty, p) -> Score (Forty, p)
    Score (Fifteen, p) -> Score (Thirty, p)
    Score (Love, p) -> Score (Fifteen, p)
    s -> s

receiverScores :: Score -> Score
receiverScores = flipScore . serverScores . flipScore

flipScore :: Score -> Score
flipScore score =
  case score of
    ServerWin -> ReceiverWin
    ReceiverWin -> ServerWin
    ServerAdvantage -> ReceiverAdvantage
    ReceiverAdvantage -> ServerAdvantage
    Score (s1, s2) -> Score (s2, s1)
    s -> s
