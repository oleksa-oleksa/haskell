data Nat = Zero | S Nat
    deriving (Show, Eq)

foldn :: (a -> a) -> a -> Nat -> a
foldn h c Zero = c
foldn h c (S n) = h (foldn h c n)

succn :: Nat -> Nat
succn a = (S a)

predn :: Nat -> Nat
predn Zero = Zero
predn (S a) = a

plus :: Nat -> Nat -> Nat
plus n m = foldn succn n m

cutSub :: Nat -> Nat -> Nat
cutSub n m = foldn predn n m

natUngerade :: Nat -> Bool
natUngerade n = foldn not False n


natMax :: Nat -> Nat -> Nat
natMax a b | cutSub a b == Zero = b
           | cutSub a b /= Zero = a  

natFib :: Nat -> Nat
natFib n = quickFib Zero (S Zero) n
       where 
            quickFib a b n | n == Zero = b
                           | otherwise = quickFib b (plus a b) (predn n)
