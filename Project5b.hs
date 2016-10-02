{-
import Data.Char (digitToInt)
luhn = (0 ==) . (`mod` 10) . sum . map (uncurry (+) . (`divMod` 10)) .
       zipWith (*) (cycle [1,2]) . map digitToInt . reverse
-}


import Data.Char (digitToInt)

luhn = (0 ==) . (`mod` 10) . sum . map (\x -> x `mod` 10 + x `div` 10)
       . zipWith (*) (cycle [1,2]) . map digitToInt . reverse


--divMod :: Integral a => a -> a -> (a, a)
{-

import Data.List (tails)
divhello:: [Int] -> [[Int]] 
divhello x = tail $ tails x

Currying is the process of transforming a function that takes 
multiple arguments into a function that takes just a single argument 
and returns another function if any arguments are still needed.
-}


{-



second
Num a =>(c,c)->c
uncurry (+)(0,3)    ---> 3

first
a->a->(a,a)
3 `divMod` 10       ---> (0,3)
-}