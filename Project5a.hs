import Data.Char(digitToInt)

convertToNumber :: String -> [Int]
convertToNumber = map digitToInt

doubleSecondEach :: [Int] -> [Int]
doubleSecondEach = zipWith (*) (cycle [1,2]) 

sumEachDigit :: [Int] -> [Int]
sumEachDigit = map (\n -> n `mod` 10 + n `div` 10)
--sumEachDigit x = [ n `mod` 10 + n `div` 10 | n <- x ] 

sumDigits :: [Int] -> Int
sumDigits = foldl (\a n -> a + n `mod` 10 + n `div` 10) 0 
 
validate :: String -> Bool
validate x = ((sumDigits . doubleSecondEach . reverse . convertToNumber) x ) `mod` 10 == 0 


--convertToNumber :: String -> [Int]
--convertToNumber x = map (read . (:[])) x :: [Int]