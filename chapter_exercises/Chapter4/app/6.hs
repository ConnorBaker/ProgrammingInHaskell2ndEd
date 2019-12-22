formalConjunction :: Bool -> Bool -> Bool
formalConjunction a b =
    if a then
        b
    else
        False