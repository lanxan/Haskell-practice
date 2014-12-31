--no pattern matching
_map' :: (Int -> Int) -> [Int] -> [Int]
_map' mapper lt =
    if null lt then lt
    else (mapper $ head lt) : (_map' mapper $ tail lt)
    
_length' :: [a] -> Int
_length' lt =
    if null lt then 0
    else 1 + (_length' $ tail lt)

        
--pattern matching
map' :: (Int -> Int) -> [Int] -> [Int]
map' _ [] = []
map' mapper (x:xs) =
    if null xs then mapper x : []
    else (mapper x) : (map' mapper xs)
--or
--  map' mapper (x:xs) = mapper x : (map' mapper xs) 
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + (length' xs) 

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum' xs)
    


--print
main = do
    putStrLn $ show $ map' (+5) [1,2,3,4] -- [6,7,8,9]
    putStrLn $ show $ length' [1,2,3,4] -- 4
    putStrLn $ show $ sum' [1,2,3,4] -- 10
