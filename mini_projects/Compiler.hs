-- Mini Project Compiler
-- Part 1
data Instruction = ADD | SUB | SIGN | ABS | PUSH Int
    deriving Show

type Stack = [Int]

intpInstr :: Stack -> Instruction -> Stack
intpInstr (x : y : tail) ADD      = (y + x) : tail
intpInstr (x : y : tail) SUB      = (y - x) : tail
intpInstr (x     : tail) SIGN     = (-x) : tail
intpInstr (x     : tail) ABS      = abs x : tail
intpInstr stack          (PUSH t) = t : stack
intpInstr _ _ = error "Invalid Instruction or stack underflow"

inst = [PUSH 2, PUSH 3, ADD, PUSH 4, SIGN, ADD]

exec :: [Instruction] -> Int
exec ls = head $ foldl intpInstr [] ls

-- Part 2
data Exp = X | C Int | Abs Exp | Minus Exp | Add (Exp,Exp) | Sub (Exp,Exp)

sem :: Exp -> Int -> Int
sem X              x = x
sem (C     x     ) y = x
sem (Abs   x     ) y = abs (sem x y)
sem (Minus x     ) y = -(sem x y)
sem (Add   (x, y)) z = sem x z + sem y z
sem (Sub   (x, y)) z = sem x z - sem y z

-- Part 3
expr = Add (X, Sub (X, C 2))

compile :: Exp -> Int -> [Instruction]
compile X              x = [PUSH x]
compile (C     x     ) y = [PUSH x]
compile (Abs   x     ) y = compile x y
compile (Minus x     ) y = SIGN : compile x y
compile (Add   (x, y)) z = compile x z ++ compile y z ++ [ADD]
compile (Sub   (x, y)) z = compile x z ++ compile y z ++ [SUB]

-- Part 4
red :: Exp -> Exp
red (Add   (C i, C j)) = C (i + j)
red (Add   (e  , C 0)) = e
red (Add   (C 0, e  )) = e
red (Sub   (C i, C j)) = C (i - j)
red (Sub   (e  , C 0)) = e
red (Sub   (C 0, e  )) = Minus e
red (Minus (C     i) ) = C (-i)
red (Minus (Minus e) ) = e
red (Abs   (C     i) ) = C (abs i)
red (Abs   (Minus e) ) = Abs e
red (Abs   (Abs   e) ) = Abs e
red x                  = x

reducible :: Exp -> Bool
reducible (Add   (C i, C j)) = True
reducible (Add   (e  , C 0)) = True
reducible (Add   (C 0, e  )) = True
reducible (Add   (x  , y  )) = reducible x || reducible y
reducible (Sub   (C i, C j)) = True
reducible (Sub   (e  , C 0)) = True
reducible (Sub   (C 0, e  )) = True
reducible (Sub   (x  , y  )) = reducible x || reducible y
reducible (Minus (C     i) ) = True
reducible (Minus (Minus e) ) = True
reducible (Minus x         ) = reducible x
reducible (Abs   (C     i) ) = True
reducible (Abs   (Minus e) ) = True
reducible (Abs   (Abs   e) ) = True
reducible (Abs   x         ) = reducible x
reducible x                  = False
