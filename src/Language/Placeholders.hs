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
         -- * Example Usage
         -- $simpleExample
       ) where

infixr 0 .$.

class CurryingApp a b e | b a -> e, b e -> a where
  -- | @foo'.$.'arg@ curries @foo@ the correct amount and composes it with @arg@.
  -- @arg@ must be of the form @a0 -> ... -> aN -> (z0,...,zN1, a0,z0,...zN1,a1,..., aN,z0,...,zNk)@
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

foo (a,b,c) = a + b + c
fiat = foo.$.(2, , )

{- $simpleExample

@
-- LANGUAGE TupleSections
module Main where
import Language.Placeholders

foo (a,b,c) = a '+' b '+' c

curried_foo = foo'.$.'(2, , )

main = do
    'putStrLn' '$' 'show' '$' curried_foo 4 5
@
-}
