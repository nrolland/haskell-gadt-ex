{-# LANGUAGE ScopedTypeVariables #-}

-- the return type of the constructor is not uniform 
-- an expr a does not CONTAIN a value :: 'a
-- this a exist only at type level - it is a phantom type
data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq  (Expr a) (Expr a)
            deriving(Show)

-- smart constructor
-- Expr a is general, this give a RESTRICTED type
-- we will only build through those !
i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B

-- we accept in a restricted type
add :: Expr Int -> Expr Int -> Expr Int
add = Add

mul :: Expr Int -> Expr Int -> Expr Int
mul = Mul

-- 

--this is well typed as 
-- I 5 does not generate any constraint on a in I 5 : Expr a
-- B True neither
-- so a is free to be Int
isWellTyped ::Expr Int = B True `Add` I 5 

-- This does not type check as we are imposing constraint on a
-- add expects some::Expr Int and b True :: Expr Bool 
-- so it fails to type check
--isWellTypedNot = add (b True) (i 5)


-- for the same reason, this does not type check 
-- the definition of Eq in Expr a binds a to the SAME type 
--eq :: Expr Int -> Expr Int -> Expr Bool
--eq = Eq


-- this fails as well
-- because a is free on the left, the function should work for all such type a
-- it can not be constrained on the right and here we are saying it should be an Int...
-- eval :: Expr a -> a
-- eval (I n) = n

validStill =  I 5  :: Expr String


-- if users are only using the smart constructor, this is not a real constraint
-- as the only way they have a I n is through i which only constructs Expr Int 
-- So we would like to bind a case in the union, a value, to a type, and have the compiler
-- know this association..


