formalConjunction :: Bool -> Bool -> Bool
formalConjunction a b =
    if a then 
        if b then 
            True
        else False
    else False