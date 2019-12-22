or1 :: Bool -> Bool -> Bool
True  `or1` True  = True
True  `or1` False = True
False `or1` True  = True
False `or1` False = True

or2 :: Bool -> Bool -> Bool
False `or2` False = False
_     `or2` _     = True

or3 :: Bool -> Bool -> Bool
False `or3` b = b
True  `or3` _ = True

or4 :: Bool -> Bool -> Bool
a `or4` b | a == b    = b
          | otherwise = True