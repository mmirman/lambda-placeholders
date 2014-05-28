{-# LANGUAGE
 TupleSections,
 MultiParamTypeClasses,
 FlexibleInstances,
 FunctionalDependencies,
 UndecidableInstances,
 CPP
 #-}
module Language.Placeholders (CurryingApp(..)) where

infixr 0 .$.

class CurryingApp a b e | b a -> e, b e -> a where
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
