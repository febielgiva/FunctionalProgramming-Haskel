
convertToNumber :: String -> Int
convertToNumber str = read str :: Int

moduloDivNumber :: Int -> (Int,Int)
moduloDivNumber x = (x `mod` 10,x `div` 10)

toList :: Int -> [Int]
toList 0 = []
toList x = mod : toList div
      where(mod,div) = moduloDivNumber x

--doubling all the second number
doubleSecondEach :: [Int] -> [Int]
doubleSecondEach [] = []
doubleSecondEach [x] = [x]
doubleSecondEach (x:y:xs) = x : (2*y) : doubleSecondEach xs 

{- sum all the element in list -} 
sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) = mod + div + sumDigits xs
    where(mod,div) = moduloDivNumber x

validate :: String -> Bool
validate x = ((sumDigits . doubleSecondEach . toList . convertToNumber) x ) `mod` 10 == 0 
     


