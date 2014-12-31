--no pattern matching
_map' :: (Int -> Int) -> [Int] -> [Int]
_map' mapper lt =
    if null lt then lt
    else (mapper $ head lt) : (_map' mapper $ tail lt)
    
--pattern matching
map' :: (Int -> Int) -> [Int] -> [Int]
map' _ [] = []
map' mapper (x:xs) =
    if null xs then mapper x : []
    else (mapper x) : (map' mapper xs)
--or
--  map' mapper (x:xs) = mapper x : (map' mapper xs) 
    
    
--print
main = do
  putStrLn $ show $ map' (+5) [1,2,3,4]
