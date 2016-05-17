data SimpleBT = L | N SimpleBT SimpleBT
    deriving (Show, Eq)

height :: SimpleBT -> Integer
height L = 0
height (N lt rt) = (max (height lt) (height rt)) + 1

balanced :: SimpleBT -> Bool
balanced L = True
balanced (N lt rt) = (balanced lt) && (balanced rt) && height lt == height rt

insLeaf :: SimpleBT -> SimpleBT -- ein Blatt wird in den Baum eingefügt
insLeaf L = N L L
insLeaf (N a L) | a /= L = N a (N L L)
insLeaf (N L a) | a /= L = N (N L L) a
insLeaf (N a b) | balanced a == False = N (insLeaf a) b
                | balanced b == False = N a (insLeaf b)
                | otherwise = N (insLeaf a) b

insLeaves :: SimpleBT -> Integer -> SimpleBT -- eine eingegebene Anzahl von Blättern
insLeaves tree 0 = tree
insLeaves tree k = insLeaves (insLeaf tree) (k-1)

delLeaf :: SimpleBT -> SimpleBT -- löscht ein Blatt aus dem Baum
delLeaf (N L L) = L
delLeaf (N L a) | a /= L = N L L
delLeaf (N a L) | a /= L = N L L
delLeaf (N a b) | balanced a == False = N (delLeaf a) b
                | balanced b == False = N a (delLeaf b)
                | otherwise = N (delLeaf a) b

delLeaves :: SimpleBT -> Integer -> SimpleBT -- eine eingegebene Anzahl von Blättern
delLeaves L k = L
delLeaves tree 0 = tree
delLeaves tree k = delLeaves (delLeaf tree) (k-1)