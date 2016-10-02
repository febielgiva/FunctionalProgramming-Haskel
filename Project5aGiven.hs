-- digitToInt is defined in Data.Char, but we can define it ourselves
myDigitToInt :: Char -> Int
myDigitToInt = read . (:[]) -- The type tells read to produce an Int.

{-
myDigitToInt could have been defined using a parameter
myDigitToInt c = read [c]
That’s the same as
myDigitToInt c = read (c : [])
That’s the same as
myDigitToInt c = (read . (:[])) c
That’s the same as
myDigitToInt = read . (:[])
-}
toDigits :: String -> [Int]
toDigits = map myDigitToInt 
cycle12 :: [Int]
cycle12 = cycle [1,2]
pairs :: [Int] -> [(Int, Int)]
pairs ds 
 | odd (length ds) = zip ds cycle12 -- If odd (length ds) cycle12 ends with 1
 | otherwise       = zip ds (tail cycle12)
doubleEveryOther :: [Int] -> [Int] -- Could use zipWith in pairs instead.
doubleEveryOther = map (\(d, m) -> d * m) . pairs 

sumDigits  :: [Int] -> Int
sumDigits = sum . concat . map (toDigits . show) -- Why not just sum?
checkSum :: String -> Int
checkSum = sumDigits . doubleEveryOther . toDigits  
isValid :: String -> Bool
isValid n = checkSum n `mod` 10 == 0
testCC :: [Bool]
testCC = map isValid ["79927398713", "79927398714"] 
-- => [True, False]


