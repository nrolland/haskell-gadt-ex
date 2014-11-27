{-# LANGUAGE ScopedTypeVariables #-}
data Expr = I Int
          | B Bool           -- boolean constants
          | Add Expr Expr
          | Mul Expr Expr
          | Eq  Expr Expr    -- equality test
          deriving (Show) 
          
eval :: Expr -> Either Int Bool
eval (I n) = Left n
eval (B b) = Right b
-- eval (Add e1 e2) = eval e1 + eval e2  -- ???

isWellTyped ::Expr = I 1 

isWellTypedToo ::Expr = B True `Add` I 5
