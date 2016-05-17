{-User's input are 'char' and "string" and by calling a Helper Function a condition will be created-}
allPositionsOf :: (Eq a) => a -> [a] -> [Int]
allPositionsOf symbol list = reverse (allPositionsOfHelper (symbol == ) 0 list [])

{-Counter is a number of a position, if a condition is True, this number will be added in a list -}
allPositionsOfHelper :: (a -> Bool) -> Int -> [a] -> [Int] -> [Int]
allPositionsOfHelper condition counter [] saved = saved
allPositionsOfHelper condition counter (x:list) saved
         | condition x = allPositionsOfHelper condition (counter+1) list (counter:saved)
         | otherwise = allPositionsOfHelper condition (counter+1) list saved

