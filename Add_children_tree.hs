data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a) 
    deriving (Show, Eq)

twoChildren :: (Ord a) => BSearchTree a -> Bool 
-- entscheidet, ob jeder Knoten des Baums genau zwei Kinder hat oder nicht
twoChildren (Node x Nil rt) | rt /= Nil = False
twoChildren (Node x lt Nil) | lt /= Nil = False
twoChildren (Node x Nil Nil) = True
twoChildren (Node x lt rt) = twoChildren lt && twoChildren rt 

full :: (Ord a) => BSearchTree a -> Bool  
-- überprüft, ob ein Baum vollständig ist.
full tree | nodes tree == (2 ^ height tree - 1) = True
          | otherwise = False

nodes :: BSearchTree a -> Integer
nodes Nil = 0
nodes (Node x lt rt) = 1 + nodes lt + nodes rt

height :: BSearchTree a -> Integer
height Nil = 0
height (Node x lt rt) = (max (height lt) (height rt)) + 1

mapTree :: (Ord a, Ord b) => (a -> b) -> BSearchTree a -> BSearchTree b
mapTree p Nil = Nil
mapTree p (Node x lt rt) = Node (p x) (mapTree p lt) (mapTree p rt)

foldTree :: (Ord a) => b -> (a -> b -> b -> b) -> BSearchTree a -> b
foldTree c p Nil = c
foldTree c p (Node x lt rt) = (p x (foldTree c p lt) (foldTree c p rt)) 