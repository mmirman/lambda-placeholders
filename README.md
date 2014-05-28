Placeholders
============

Placeholders is a Haskell library to emulate the placeholders feature of Scala. 

Background
----------

* Placeholders in Scala act similar to those used in category theory.  
* Rather than create a lambda or name a function, an underscore is used in place of a parameter 
and the function is abstracted over that location.
* This placeholders library doesn't use underscores, rather it leverages similar capabilities found in TupleSections.

Example
-------

```haskell
{-# LANGUAGE
 TupleSections 
 #-}
module Main where 
import Language.Placeholders

foo (a,b,c) = a + b + c

curried_foo = foo.$.(2, , )

main = do
    putStrLn $ show $ curried_foo 4 5
```
