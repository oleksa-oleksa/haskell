data Nat = Zero | S Nat
    deriving (Show, Eq)

data ZInt = Z Nat Nat
    deriving (Show, Eq)

zsimplify :: ZInt -> ZInt
zsimplify (Z Zero b) = Z Zero b
zsimplify (Z a Zero) = Z a Zero
zsimplify (Z (S a) (S b)) = zsimplify (Z a b)

-- Test
zint2Int :: ZInt -> Integer
zint2Int (Z Zero Zero) = 0
zint2Int (Z Zero (S b)) = 1 + zint2Int (Z Zero b)
zint2Int (Z (S a) Zero) = zint2Int (Z a Zero) - 1

int2Zint :: Integer -> ZInt -- transformiert von Integer nach ZInt
int2Zint x | x == 0 = Z Zero Zero
           | x > 0 = Z Zero (int2nat x)
           | x < 0 = Z (int2nat x) Zero

int2nat :: Integer -> Nat -- transformiert von Integer nach Nat
int2nat 0 = Zero
int2nat x | x > 0 = S (int2nat (x-1))
          | x < 0 = S (int2nat (x+1))
        
nat2int :: Nat -> Integer
nat2int Zero = 0
nat2int (S a) = 1 + nat2int a

-- From previous Aufgabe
foldn :: (a -> a) -> a -> Nat -> a
foldn h c Zero = c
foldn h c (S n) = h (foldn h c n)

succn :: Nat -> Nat
succn a = (S a)

predn :: Nat -> Nat
predn Zero = Zero
predn (S a) = a

-- Substraction
cutSub :: Nat -> Nat -> Nat
cutSub n m = foldn predn n m

-- Minusz takes simplified digits and selects executions branch according to results of comparisson 
minusz :: ZInt -> ZInt -> ZInt
minusz (Z Zero (S a)) (Z Zero (S b)) | gt (S a) (S b) == True = Z Zero (cutSub (S a) (S b))
                                     | eq (S a) (S b) == True = (Z Zero Zero)
                                     | otherwise = Z (cutSub (S b) (S a)) Zero
minusz (Z Zero (S a)) (Z (S b) Zero) = Z Zero (plus (S a) (S b))
minusz (Z (S a) Zero) (Z Zero (S b)) = Z (plus (S a) (S b)) Zero

-- Multiplication
mult :: Nat -> Nat -> Nat
mult n m | n == Zero || m == Zero = Zero
         | otherwise = foldn (plus n) Zero m

multz :: ZInt -> ZInt -> ZInt
multz (Z Zero Zero) (Z Zero Zero) = Z Zero Zero
multz (Z Zero (S a)) (Z Zero (S b)) = Z Zero (mult (S a) (S b)) 
multz (Z (S a) Zero) (Z Zero (S b)) = Z (mult (S a) (S b)) Zero
multz (Z Zero (S a)) (Z (S b) Zero) = Z (mult (S a) (S b)) Zero
multz (Z (S a) Zero) (Z (S b) Zero) = Z Zero (mult (S a) (S b))
multz a b = multz (zsimplify a) (zsimplify b)

-- Addition
plus :: Nat -> Nat -> Nat
plus n m = foldn succn n m

zadd :: ZInt -> ZInt -> ZInt
zadd (Z a b) (Z c d) = Z (plus a c) (plus b c)

-- Functions than I need to code

zpow :: ZInt -> Nat -> ZInt -- Potenz
zpow a Zero = Z Zero (S Zero) 
zpow a b = foldn (multz a) (Z Zero (S Zero)) b

zneg :: ZInt -> ZInt -- negative Zahl
zneg (Z Zero Zero) = Z Zero Zero
zneg (Z (S a) Zero) = Z (S a) Zero
zneg (Z Zero (S a)) = Z (S a) Zero
zneg (Z (S a) (S b)) = zneg $ zsimplify (Z (S a) (S b))

zabs :: ZInt -> ZInt -- Absolute Zahl
zabs (Z Zero Zero) = Z Zero Zero
zabs (Z (S a) Zero) = Z Zero (S a)
zabs (Z Zero (S a)) = Z Zero (S a)
zabs (Z (S a) (S b)) = zabs $ zsimplify (Z (S a) (S b))

modulo :: ZInt -> ZInt -> ZInt 
modulo a b = fmod (zabs a) (zabs b)
    where
        fmod a b | zeq a b = Z Zero Zero
                 | zlt a b = a
                 | zgt a b = fmod (minusz a b) b

zIsDivisor :: ZInt -> ZInt -> Bool
zIsDivisor a b = zeq (modulo a b) (Z Zero Zero)


-- Using Euclid´s algorithm
zggt :: ZInt -> ZInt -> ZInt -- größter gemeinsamer Teiler
zggt a b | zeq b (Z Zero Zero) = a
         | otherwise = zggt b (modulo a b)

-- Comparison
eq :: Nat -> Nat -> Bool
eq Zero Zero = True
eq (S a) (S b) = eq a b
eq Zero (S b) = False
eq (S a) Zero = False

lt :: Nat -> Nat -> Bool
lt Zero Zero = False
lt Zero (S b) = True
lt (S a) Zero = False
lt (S a) (S b) = lt a b

gt :: Nat -> Nat -> Bool
gt Zero Zero = False
gt (S a) Zero = True
gt Zero (S b) = False
gt (S a) (S b) = gt a b


zeq :: ZInt -> ZInt -> Bool
zeq (Z a b) (Z c d) = eq (plus b c) (plus a d)


zlt :: ZInt -> ZInt -> Bool
zlt (Z a b) (Z c d) = lt (plus b c) (plus a d)


zgt :: ZInt -> ZInt -> Bool
zgt (Z a b) (Z c d) = gt (plus b c) (plus a d)