luhnDouble :: Int -> Int
luhnDouble n | n <= 5    = 2 * n
             | otherwise = 2 * n - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =
    (luhnDouble a + b + luhnDouble c + d)
    `mod` 10 == 0