import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (sconcat)

-- data Card =
--   A | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | J | Q | K deriving (Eq, Show)
data Card =
  A | N Int | J | Q | K deriving (Eq, Show)

sumHand :: [Card] -> Int
sumHand cards =
  let possiblePoints  = map toPoint cards
      scoreCandidates = foldl plusEach (0 :| []) possiblePoints
      noBust          = NonEmpty.filter (<= 21) scoreCandidates
  in 
    if null noBust
      then NonEmpty.head scoreCandidates
      else maximum noBust

toPoint :: Card -> NonEmpty Int
toPoint A = 1 :| [11]
toPoint (N n) = n :| []
toPoint _ = 10 :| []

plusEach :: NonEmpty Int -> NonEmpty Int -> NonEmpty Int
plusEach list1 list2 =
  sconcat(
    NonEmpty.map(\element2 ->
      NonEmpty.map(\element1 ->
        element1 + element2
      ) list2
    ) list1
  )
