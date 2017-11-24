-- data Card =
--   A | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | J | Q | K deriving (Eq, Show)
data Card =
  A | N Int | J | Q | K deriving (Eq, Show)

sumHand :: [Card] -> Int
sumHand cards =
  let possiblePoints  = map toPoint cards
      scoreCandidates = foldl plusEach [0] possiblePoints
      noBust          = filter (<= 21) scoreCandidates
  in 
    if null noBust
      then head scoreCandidates
      else maximum noBust

toPoint :: Card -> [Int]
toPoint A = [1, 11]
toPoint (N n) = [n]
toPoint _ = [10]

plusEach :: [Int] -> [Int] -> [Int]
plusEach list1 list2 =
  concatMap(\element1 ->
    map(\element2 ->
      element1 + element2
    ) list2
  ) list1

