{-# LANGUAGE InstanceSigs #-}
import           Data.List


data Lambda =
    V String
    | O String
    | I Int
    | B Bool
    | L (String, Lambda)
    | A (Lambda, Lambda)

instance Show Lambda where
    show :: Lambda -> String
    show (V x       ) = x
    show (O x       ) = x
    show (I x       ) = show x
    show (B b       ) = show b
    show (L (str, l)) = "(λ" ++ str ++ "." ++ show l ++ ")"
    show (A (x  , y)) = "(" ++ show x ++ " " ++ show y ++ ")"

free' :: Lambda -> [String]
free' (V x     ) = [x]
free' (L (x, l)) = x : free' l
free' (A (x, y)) = free' x ++ free' y
free' _          = []

free :: Lambda -> [String]
free = nub . free'

t1 :: Lambda
t1 = L ("x", L ("y", A (A (O "+", V "x"), V "y")))

t2 :: Lambda
t2 = A (t1, I 3)

t3 :: Lambda
t3 = A (t2, I 4)

subst :: Lambda -> String -> Lambda -> Lambda
subst (V x) x' 