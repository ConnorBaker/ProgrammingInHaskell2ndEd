safetailConditional :: [a] -> [a]
safetailConditional xs = if null xs
                         then []
                         else tail xs

safetailGuarded :: [a] -> [a]
safetailGuarded xs | null xs   = []
                   | otherwise = tail xs

safetailPatternMatch :: [a] -> [a]
safetailPatternMatch [] = []
safetailPatternMatch (_:xs) = xs