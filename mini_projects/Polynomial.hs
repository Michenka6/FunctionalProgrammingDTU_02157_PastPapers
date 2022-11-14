-- Polynomial Mini-Project

-- Polynomial:  P(x) = a_0 * x^0 + a_1 · x^1 + ... + a_n · x^n
import           Data.List

type Poly = [Int]

poly1 :: [Int]
poly1 = [2, 3, 0, 1]
poly2 :: [Int]
poly2 = [1, 2, 3]

-- Part 1
-- Preserves the invariant
add :: Poly -> Poly -> Poly
add []       x        = x
add x        []       = x
add (x : xs) (y : ys) = (x Prelude.+ y) : add xs ys

-- If x is 0, does not preserve the invariant
mulC :: Int -> Poly -> Poly
mulC x = map (Prelude.* x)

-- Preserves the invarianr
sub :: Poly -> Poly -> Poly
sub []       x        = x
sub x        []       = x
sub (x : xs) (y : ys) = (x Prelude.- y) : sub xs ys

-- Preserves the invariant
mulX :: Poly -> Poly
mulX = (0 :)

-- Since mulC does not preserve neither does mul
mul :: Poly -> Poly -> Poly
mul []       x = []
mul (x : xs) p = mulC x p `add` mulX (mul xs p)

power :: Int -> Int -> Int
power x 0 = 1
power x n = x Prelude.* power x (n Prelude.- 1)

eval :: Int -> Poly -> Int
eval x xs = sum $ zipWith (\m n -> m Prelude.* power x n) xs [0 ..]

-- Part 2
isLegal :: [Int] -> Bool
isLegal [] = True
isLegal ls = (/= 0) $ last ls

-- Preserves the invariant
prune :: [Int] -> Poly
prune ls | isLegal ls = ls
         | otherwise  = prune $ init ls

toString :: Poly -> [Char]
toString ls =
    init $ concat $ zipWith (\x n -> show x ++ "*x^" ++ show n ++ "+") ls [0 ..]

-- Preserves the invariant
derivative :: Poly -> Poly
derivative ls = zipWith (Prelude.*) [1 ..] $ tail ls

-- Since mul does not preserve neither does polyPower
polyPower :: Poly -> Int -> Poly
polyPower _  0 = [1]
polyPower ls n = mul ls $ polyPower ls (n Prelude.- 1)

-- Since polyPower and mulC do not preserve neither does compose
compose :: Poly -> Poly -> Poly
compose ls q =
    foldl add [] $ zipWith (\x n -> mulC x $ polyPower q n) ls [0 ..]

-- Part 3
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Part 4
data Degree = MinusInf | Fin Int
    deriving Show

instance Eq Degree where
    MinusInf == MinusInf = True
    MinusInf == Fin _    = False
    Fin _    == MinusInf = False
    Fin x    == Fin y    = x == y

instance Ord Degree where
    Fin x    `compare` Fin y    = x `compare` y
    Fin _    `compare` MinusInf = GT
    MinusInf `compare` Fin _    = LT
    MinusInf `compare` MinusInf = EQ

degree :: Poly -> Degree
degree [] = MinusInf
degree ls = Fin (length ls Prelude.- 1)

d :: [Degree]
d = [Fin 3, Fin 2, Fin 0, MinusInf]

addD :: Degree -> Degree -> Degree
addD (Fin x)  (Fin y)  = Fin (x Prelude.+ y)
addD (Fin _)  MinusInf = MinusInf
addD MinusInf (Fin _)  = MinusInf
addD MinusInf MinusInf = MinusInf

-- Part 5
-- Basically we are making a monoid with Poly as its argument space
idAdd :: Poly
idAdd = []

idMul :: Poly
idMul = [1]

idSub :: Poly
idSub = []

idCompose :: Poly
idCompose = [0, 1]

-- Part 6
ofList :: [Int] -> Poly
ofList ls | isLegal ls = ls
          | otherwise  = ofList $ init ls
toList :: Poly -> [Int]
toList = id

(+) :: Poly -> Poly -> Poly
x + y = add x y

(*) :: Poly -> Poly -> Poly
x * y = mul x y

(-) :: Poly -> Poly -> Poly
x - y = sub x y
