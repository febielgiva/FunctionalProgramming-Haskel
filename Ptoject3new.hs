import Data.List (isPrefixOf)
--mapping = [("D",500),("CD",400),("C",100),("XC",90),
--          ("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1),("M",1000),("CM",900)]

 --mapping = [("D",500),("CD",400),("C",100),("XC",90),
 --          ("L",50),("XL",40),("X",10),("IX",9),("V",5),("IV",4),("I",1),("M",1000),("CM",900)]
 
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

--oneStep str =
--   head [(num, drop (length roman) str) | 
--                (roman,num) <- mapping, roman `isPrefixOf` str]  

   

oneStep ('D' : rest) = (500, rest)
oneStep ('C' : 'D' : rest) = (400, rest)
oneStep ('C' : rest) = (100, rest)
oneStep ('X' : 'C' : rest) = (90, rest)
oneStep ('L' : rest) = (50, rest)
oneStep ('X' : 'L' : rest) = (40, rest)
oneStep ('X' : rest) = (10, rest)
oneStep ('I' : 'X' : rest) = (9, rest)
oneStep ('V' : rest) = (5, rest)
oneStep ('I' : 'V' : rest) = (4, rest)
oneStep ('I' : rest) = (1, rest)
oneStep ('M' : rest) = (1000, rest)
oneStep ('C' : 'M' : rest) = (900, rest)
oneStep (_: rest) = (0,rest)


testCases = ["MCMXC", "MMVIII", "MDCLXVI"]
test = zip testCases (map toArabic testCases)
