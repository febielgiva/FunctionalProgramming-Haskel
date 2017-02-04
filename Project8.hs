import Data.List  
import Data.List.Split 

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . filter (/= "") .splitOn "," . replace1 ' ' ',' 
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs

replace1 :: Char -> Char -> String -> String 
replace1 a b = map (\x -> if (a == x) then b else x)


--mySplit :: String -> [String]
--mySplit input = filter (/= "") . splitOn "," . foldl (\a b -> (a++","++b)) [] (words input)


--"10,8,,+" ["10","8","","+"]




