--import Data.List (isPrefixOf)
--mapping = [("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),
--           ("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1)]

 {-mapping = [("D",500),("CD",400),("C",100),("XC",90),
           ("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1),("M",1000),("CM",900)]
 -}
toArabic :: String -> Int
toArabic "" = 0
toArabic str = num + toArabic rest
    where (num, rest) = oneStep str
{- 
Note: could also be written as follows.
toArabic str = 
    let (num, rest) = oneStep str 
    in num + toArabic rest
-}

oneStep :: String  -> (Int, String)
{-
oneStep str =
   head [(num, drop (length roman) str) | 
                (roman,num) <- mapping, roman `isPrefixOf` str]  
-}
   
oneStep [] = (0,[])
oneStep (x:y:ys)
     | [x] == "M" = (1000,y:ys) 
     | [x,y] == "CM" = (900,ys) 
     | [x] == "D" = (500,y:ys)
     | [x,y] == "CD" = (400,ys)
     | [x] == "C" = (100,y:ys)
     | [x,y] == "XC" = (90,ys)
     | [x] == "L" = (50,y:ys)
     | [x,y] == "XL" = (40,ys)
     | [x] == "X" = (10,y:ys)
     | [x,y] == "IX" = (9,ys)
     | [x] == "V" = (5,y:ys)
     | [x,y] == "IV" = (4,ys)
     | [x] == "I" = (1,y:ys)
     | otherwise = (0,y:ys)

oneStep (x:[])
     | [x] == "M" = (1000,[]) 
     | [x] == "D" = (500,[])
     | [x] == "C" = (100,[])
     | [x] == "L" = (50,[])
     | [x] == "X" = (10,[])
     | [x] == "V" = (5,[])
     | [x] == "I" = (1,[])
     | otherwise = (0,[])


testCases = ["MCMXC", "MMVIII", "MDCLXVI"]
test = zip testCases (map toArabic testCases)
