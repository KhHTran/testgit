union :: [Int] -> [Int] -> [Int]
union x y = unique (x ++ y)

unique :: [Int] -> [Int]
unique [] = []
unique (x:xs)
    | has x xs = unique xs
    | otherwise = x : unique xs

has :: Int -> [Int] -> Bool
has _ [] = False
has y (x:xs)
    | y == x = True
    | otherwise = has y xs

