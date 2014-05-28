{-# LANGUAGE
 TupleSections,
 MultiParamTypeClasses,
 FlexibleInstances,
 FunctionalDependencies,
 UndecidableInstances
 #-}
module Language.Placeholders (App(..)) where

infixr 0 .$.

class App a b e | b -> e, a e -> b where
  (.$.) :: a -> b -> e
 
instance App a d e => App a (b -> d) (b -> e) where
  (.$.) f g = \b -> f .$. g b

instance App (a -> c) a c where
  (.$.) = ($)
 
 
foo :: (Integer , Integer, Integer) -> Integer
foo (a,b,c) = a + b + c

fiat :: Integer -> Integer -> Integer
fiat = foo.$.(2, , )