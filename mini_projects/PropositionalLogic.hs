-- Mini Project Propositional Logic
-- Part 1
data Prop a =
    A a | Dis (Prop a,Prop a) | Con (Prop a, Prop a) | Neg (Prop a)
    deriving Show

sem :: Eq a => Prop a -> [a] -> Bool
sem (A   a     ) ls = a `elem` ls
sem (Dis (a, b)) ls = sem a ls && sem b ls
sem (Con (a, b)) ls = sem a ls || sem b ls
sem (Neg a     ) ls = not $ sem a ls

-- Part 2
toNnf :: Prop a -> Prop a
toNnf (Neg (Neg a     )) = a
toNnf (Neg (Con (a, b))) = Dis (Neg (toNnf a), Neg (toNnf b))
toNnf (Neg (Dis (a, b))) = Con (Neg (toNnf a), Neg (toNnf b))
toNnf x                  = x

onNnf :: Prop a -> Bool
onNnf (Neg (Con _)) = False
onNnf (Neg (Dis _)) = False
onNnf (Dis (a, b) ) = onNnf a && onNnf b
onNnf (Con (a, b) ) = onNnf a && onNnf b
onNnf (Neg a      ) = onNnf a
onNnf x             = True
