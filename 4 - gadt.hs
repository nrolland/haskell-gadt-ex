{-#LANGUAGE GADTs #-}
-- here we bind the restricted type to their constructors
-- and we ask the type checker to remember that association
-- the VALUE constructed gives you the precise TYPE

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


-- To summarise, GADTs allows us to RESTRICT the return types of constructors 
-- and thus enable us to take advantage of Haskell's type system for our DSL
   
data FooInGadtClothing a where
 MkFooInGadtClothing :: a -> FooInGadtClothing a

--which is no different from:  data Haskell98Foo a = MkHaskell98Foo a ,

--by contrast, consider:

data TrueGadtFoo a where
  MkTrueGadtFoo :: a -> TrueGadtFoo Int
