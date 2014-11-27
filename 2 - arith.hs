data Expr = I Int               -- integer constants
            | B Bool            -- boolean constants
            | Add Expr Expr     -- add two expressions
            | Mul Expr Expr     -- multiply two expressions
            | Eq  Expr Expr     -- equality test
            deriving (Show)  

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just (Left n)
eval (B b) = Just (Right b)

eval (Add (I n) (I m) ) = Just (Left (n+m))
eval (Add (I n) (B _) ) = Nothing
eval (Add (I n) (Add e1 e2) ) = let ov1 = eval(Add e1 e2)
                                in case ov1 of 
                                     Just(Left v1) -> Just (Left(v1 + n))
                                     Nothing -> Nothing
eval (Add (I n) (Mul e1 e2)) =  let ov1 = eval(Mul e1 e2)
                                in case ov1 of 
                                     Just(Left v1) -> Just (Left(v1 * n))
                                     Nothing -> Nothing
eval (Add (I n) (Eq _ _) ) = Nothing
eval (Add (B _) _ ) = Nothing
-- etc..



