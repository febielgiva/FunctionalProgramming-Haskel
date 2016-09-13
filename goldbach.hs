
--isPrime ::Int -> Bool
isPrime g | g <=1 = False | otherwise = not $ 0 `elem` (map (mod g)[2..g-1])

--rangeOfNumbers :: [Int]
rangeOfNumbers = [3,5 .. 5999]

--oddnotprime :: [Int]
oddnotprime = [d | d  <- rangeOfNumbers , not (isPrime d)]

--primenumbers :: [Int]
primenumbers = 2:[p | p <- rangeOfNumbers ,not $ 0 `elem` (map (mod p)[2..(p `div` 2)])]

--isASquare ::Int ->Bool
isASquare s = sq * sq == s where sq = floor $ sqrt $ (fromIntegral s:: Double)

--isGoldBach ::Int -> [Int]
isGoldBach x = [p | p<-primenumbers,isASquare ((x - p) `div` 2)]

--finalanswers :: [Int]
finalanswers = [o|o<-oddnotprime,isGoldBach o ==[]]
