{-# LANGUAGE GADTs #-}

-- we have to define these types
data Empty
data NonEmpty

-- the idea is that you can have either 
--    SafeList a Empty
-- or SafeList a NonEmpty
data SafeList a b where
-- to be implemented
  Nil  :: SafeList a Empty
  Cons :: a -> SafeList a b -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons a _)  = a
