primZahlen :: Integer -> [Integer]
primZahlen n = sieb [2..n]
  where
  sieb [] = []
  sieb (p:xs) = p:(sieb [k | k <- xs, (k `mod` p > 0)])

goldbachParis :: Integer -> [(Integer,Integer)]
goldbachParis n | n `mod` 2 == 0 = [(x,y)| x <- (primZahlen n), y <- (primZahlen n), x + y == n, x>y]
                | otherwise = error "Please enter even number only!"

