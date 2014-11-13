
data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq  (Expr a) (Expr a)

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B

add :: Expr Int -> Expr Int -> Expr Int
add = Add

mul :: Expr Int -> Expr Int -> Expr Int
mul = Mul

eq :: Expr Int -> Expr Int -> Expr Bool
eq = Eq
