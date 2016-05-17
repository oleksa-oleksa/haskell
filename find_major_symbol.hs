majority :: (Ord a, Eq a) => [a] -> Maybe a
majority list | reps >= l = Just sym -- if an amount of max repeats is >= then this symbol as major symbol
               | otherwise = Nothing
    where (sym, reps) = longestSequence (findRepeats list) -- (sym, reps) is tuple with max number of repeats
          l = (length list `div` 2) + 1

-- Finds a tuple with maximum number of repeats
longestSequence :: [(a, Int)] -> (a, Int)
longestSequence (h:tail) = finder h tail
    where 
        finder :: (a, Int) -> [(a, Int)] -> (a, Int)
        finder x [] = x
        finder max@(_, m) ((s, c):lst) | m < c = finder (s, c) lst 
                                       | otherwise = finder max lst

findRepeats :: (Ord a, Eq a) => [a] -> [(a, Int)]
findRepeats list = countElems (quickSort (>) list) 1

-- Sorts input list
quickSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
quickSort p [] = []
quickSort p [x] = [x]
quickSort p (x:xs) = quickSort p (filter (p x) xs) ++ [x] ++ quickSort p (filter (np x) xs)
    where 
        np x y = not (p x y)

-- Counts amount of repeats and saves a symbol and its amount
countElems :: (Eq a) => [a] -> Int -> [(a, Int)]
countElems [] _ = []
countElems [x] counter = [(x, counter)] 
countElems (x:y:list) counter | x == y = countElems (y:list) (counter + 1)
                                  | otherwise = (x, counter) : (countElems (y:list) 1)



