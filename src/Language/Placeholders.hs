{-# LANGUAGE
 TupleSections,
 MultiParamTypeClasses,
 FlexibleInstances,
 FunctionalDependencies,
 UndecidableInstances,
 CPP
 #-}
module Language.Placeholders
       ( CurryingApp(..)
       , UncurryingApp(..)
         -- * Example Usage
         -- $simpleExample
       ) where

infixl 0 .$.

class CurryingApp a b e | b a -> e, e b -> a where
  -- | @foo'.$.'arg@ curries @foo@ the correct amount and composes it with @arg@.
  -- @arg@ must be of the form @a0 -> ... -> aN -> (z0,...,zN1, a0,z0,...zN1,a1,..., aN,z0,...,zNk)@
  -- @foo@ must be of the form @(z0,...,zN1, a0,z0,...zN1,a1,..., aN,z0,...,zNk) -> r@
  -- '.$.' has the same fixity as '$'.
  (.$.) :: a -> b -> e

instance CurryingApp a d e => CurryingApp a (b -> d) (b -> e) where
  (.$.) f g b = f .$. g b
 
#define CURRYING_INST(arg) \
instance CurryingApp (arg -> a0) arg a0 where \
  (.$.) = ($)

CURRYING_INST((a,b))
CURRYING_INST((a,b,c))
CURRYING_INST((a,b,c,d))
CURRYING_INST((a,b,c,d,e))
CURRYING_INST((a,b,c,d,e,f))
CURRYING_INST((a,b,c,d,e,f,g))
CURRYING_INST((a,b,c,d,e,f,g,h))
CURRYING_INST((a,b,c,d,e,f,g,h,i))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y))
CURRYING_INST((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z))
#undef CURRYING_INST


infixl 0 .@.

class UncurryingApp a b e | b a -> e, e b -> a where
  -- | @foo'.@.'arg@ curries @foo@ the correct amount and composes it with @arg@.
  -- @arg@ must be of the form @a0 -> ... -> aN -> (z0,...,zN1, a0,z0,...zN1,a1,..., aN,z0,...,zNk)@
  -- and @foo@ must be of the form @z0-> ...-> zN1 -> a0 -> ... -> aN -> z0 -> ... -> zNk -> r@
  (.@.) :: a -> b -> e
  
instance UncurryingApp a d e => UncurryingApp a (b -> d) (b -> e) where
  (.@.) f g b = f .@. g b

instance UncurryingApp (a -> b -> a0) (a,b) a0 where
  (.@.) f (a,b) = f a b

instance UncurryingApp (a -> b -> c -> a0) (a,b,c) a0 where
  (.@.) f (a,b,c) = f a b c

instance UncurryingApp (a -> b -> c -> d -> a0) (a,b,c,d) a0 where
  (.@.) f (a,b,c,d) = f a b c d

instance UncurryingApp (a -> b -> c -> d -> e -> a0) (a,b,c,d,e) a0 where
  (.@.) f (a,b,c,d,e) = f a b c d e

instance UncurryingApp (a -> b -> c -> d -> e -> f -> a0) (a,b,c,d,e,f) a0 where
  (.@.) foo (a,b,c,d,e,f) = foo a b c d e f

instance UncurryingApp (a -> b -> c -> d -> e -> f -> g -> a0) (a,b,c,d,e,f,g) a0 where
  (.@.) foo (a,b,c,d,e,f,g) = foo a b c d e f g

instance UncurryingApp (a -> b -> c -> d -> e -> f -> g -> h -> a0) (a,b,c,d,e,f,g,h) a0 where
  (.@.) foo (a,b,c,d,e,f,g,h) = foo a b c d e f g h

instance UncurryingApp (a -> b -> c -> d -> e -> f -> g -> h -> i -> a0) (a,b,c,d,e,f,g,h,i) a0 where
  (.@.) foo (a,b,c,d,e,f,g,h,i) = foo a b c d e f g h i

instance UncurryingApp (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> a0) (a,b,c,d,e,f,g,h,i,j) a0 where
  (.@.) foo (a,b,c,d,e,f,g,h,i,j) = foo a b c d e f g h i j

instance UncurryingApp (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> a0) (a,b,c,d,e,f,g,h,i,j,k) a0 where
  (.@.) foo (a,b,c,d,e,f,g,h,i,j,k) = foo a b c d e f g h i j k

instance UncurryingApp (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> a0) (a,b,c,d,e,f,g,h,i,j,k,l) a0 where
  (.@.) foo (a,b,c,d,e,f,g,h,i,j,k,l) = foo a b c d e f g h i j k l
  
instance UncurryingApp (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> a0) (a,b,c,d,e,f,g,h,i,j,k,l,m) a0 where
  (.@.) foo (a,b,c,d,e,f,g,h,i,j,k,l,m) = foo a b c d e f g h i j k l m

foo (a,b,c,d,e) = a + b + c + d * c + e

curried_foo = foo.$.(2, , 3 , , )

uncurried_foo = curried_foo.@.( , 3 , )

{- $simpleExample

@
-- LANGUAGE TupleSections
module Main where
import Language.Placeholders

foo (a,b,c,d,e) = a '+' b '+' c '+' d '*' c '+' e

curried_foo = foo'.$.'(2, , 3 , , )

uncurried_foo = curried_foo'.@.'( , 3 , )

main = do
    'putStrLn' '$' 'show' '$' curried_foo 4 5
@
-}
