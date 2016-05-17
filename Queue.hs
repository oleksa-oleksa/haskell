
data Queue a = Queue [a] [a]

enqueue :: a -> Queue a -> Queue a
enqueue n (Queue xs ys) = Queue xs (n:ys)

-- Elemente werden immer aus der
--ersten Liste entfernt und neue Elemente werden am Anfang der zweiten Liste eingefügt. Wenn 
--die erste Liste leer ist und ein weiteres Element entfernt werden soll, wird die zweite Liste 
--umgedreht und als erste Liste gesetzt.

dequeue :: Queue a -> (Queue a, a)
dequeue (Queue [] ys) = dequeue $ Queue (reverse ys) []
dequeue (Queue xs ys) | isEmpty (Queue xs ys) == False = ((Queue (tail xs) ys), head xs)
                      | otherwise = error "Attempt to take unexisting element"                         

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty (Queue xs ys) = False

makeQueue :: Queue a
makeQueue = Queue [] []

-- Makes one list from two lists respecting the order for queue 
showQueue :: Queue a -> [a] 
showQueue (Queue a b) = a ++ reverse b


instance Show a => Show (Queue a) 
    where
        show q = show $ showQueue q


instance Eq a => Eq (Queue a)
    where
     (==) a b  = showQueue a == showQueue b

instance (Ord a) => Ord (Queue a) 
    where
    (>) a b = showQueue a > showQueue b

    (<) a b = showQueue a < showQueue b
     
    (>=) a b = showQueue a >= showQueue b

    (<=) a b = showQueue a <= showQueue b 
     

--- Test Functions
testEn1 :: Bool
testEn1 = if enqueue 1 (makeQueue) == Queue [] [1] then True else False

testEn2 :: Bool
testEn2 = if enqueue 33 (Queue [1,2] [10,11]) == Queue [1,2] [33,10,11] then True else False

testEmpty :: Bool
testEmpty = if isEmpty (Queue [] []) == True then True else False

testShow :: Bool
testShow = if showQueue (Queue [1,2,3] [6,5,4]) == [1,2,3,4,5,6] then True else False



