{-#LANGUAGE GADTs #-}
-- here we bind the restricted type to their constructors
-- and we ask the type checker to remember that association
-- the value gives you the type

data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
-- since we can only have I when a is Int, we dont impose
-- constraints on type a here
eval (I n) = n
eval (B b) = b
-- that is why we have no problem here as well
-- the value Add _ _ can only be of type Expr Int
-- so we can add e1 and e2
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2
