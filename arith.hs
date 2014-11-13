
data Expr = I Int               -- integer constants
            | B Bool            -- boolean constants
            | Add Expr Expr     -- add two expressions
            | Mul Expr Expr     -- multiply two expressions
            | Eq  Expr Expr     -- equality test

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just (Left n)
eval (B b) = Just (Right b)

eval (Add Nothing _) = Nothing
eval (Add _ Nothing) = Nothing
eval (Add (Just (Right _)) _) = Nothing
eval (Add _ (Just (Right _))) = Nothing
eval (Add (Just (Left a)) (Just (Left b))) = Just (Left (a + b))
eval (Add e1 e2) = eval (Add (eval e1) (eval e2))

eval (Mul Nothing _) = Nothing
eval (Mul _ Nothing) = Nothing
eval (Mul (Just (Right _)) _) = Nothing
eval (Mul _ (Just (Right _))) = Nothing
eval (Mul (Just (Left a)) (Just (Left b))) = Just (Left (a * b))
eval (Mul e1 e2) = eval (Mul (eval e1) (eval e2))

eval (Eq Nothing _) = Nothing
eval (Eq _ Nothing) = Nothing
eval (Eq (Just (Right _)) _) = Nothing
eval (Eq _ (Just (Right _))) = Nothing
eval (Eq (Just (Left a)) (Just (Left b))) = Just (Right (a == b))
eval (Eq e1 e2) = eval (Eq (eval e1) (eval e2))
