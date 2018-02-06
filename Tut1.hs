{-
4TB3 Tutorial January 19, 2018 
Recursive Descent Parser for the grammar A.
A -> aAe | b[c]{d}

To use, load the file into ghci and called the function a on the string.

-}

{-
the top-level function, which calls a'.  If a' returns an
empty list, the entire string has been consumed and the parse
is successful.  Otherwise, error, because there are extra 
characters.
-} 
a :: String -> Bool
a xs = 
    if a' xs == []
    then True
    else error "String overflow"

-- The function which actually consumes characters.
a' :: String -> String
a' [] = error "Empty string"
a' (x:xs) = 
    case x of 
        'a' -> consumeE (a' xs)
        'b' -> (consumeDs . consumeC) xs

-- consume either c or nothing
consumeC :: String -> String
consumeC [] = []
consumeC (x:xs) = 
    if x == 'c' then xs else x:xs

-- consume any number of d's or nothing
consumeDs :: String -> String
consumeDs [] = []
consumeDs (x:xs) = 
    if x == 'd' then consumeDs xs else x:xs 

-- consume one and only one e
consumeE :: String -> String
consumeE [] = error "Could not find expected e"
consumeE (x:xs) = 
    if x == 'e' 
    then xs 
    else error $ "Could not find expected e.  Found " ++ show x