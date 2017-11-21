-- data Card =
--   A | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | J | Q | K deriving (Eq, Show)
data Card =
  A | N Int | J | Q | K deriving (Eq, Show)
