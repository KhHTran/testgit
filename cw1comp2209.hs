import Data.Char
--EX1
st :: ([Int],Int) -> [Int]
st ([],x) = [x]
st ([x],y) = [x+y]
st ((x:rs),y) = [y+x] ++ st (rs, x+y)
subtotal :: [Int] -> [Int]
subtotal [] = []
subtotal [x] = [x]
subtotal (x:xs) = st((x:xs),0)
-- EX2
countRange :: [Int]->Int->Int -> Int
countRange [] _ _ = 0
countRange (x:xs) a b = 
	if x >= a && x <= b
	then 1 + countRange xs a b
	else countRange xs a b
countN :: [Int]->Int->Int -> [Int]
countN [] _ _ = []
countN _ _ (-1) = []
countN xs n i = [countRange xs (n*i) (n*i+n-1)] ++ countN xs n (i-1) 
histogram :: Int->[Int] -> [Int]
histogram _ [] = []
histogram n xs = reverse (countN xs n ((maximum xs) `div` n))
--EX3
ucasPoint :: [Char] -> Int
ucasPoint [] = 0
ucasPoint "E" = 16
ucasPoint "*" = 8
ucasPoint "D" = 24
ucasPoint "C" = 32
ucasPoint "B" = 40
ucasPoint "A" = 48
ucasPoint (x:xs) = ucasPoint [x] + ucasPoint xs
meetsOffer :: [Char]->Int -> Bool
meetsOffer [] _ = False
meetsOffer xs n = 
	if ucasPoint xs >= n
	then True
	else False
--EX4
ascending :: [Int] -> Bool
ascending [] = True
ascending [x] = True
ascending (x:xs) =
	if head xs <= x 
	then False
	else ascending xs

nondescending :: [Int] -> Bool
nondescending [] = True
nondescending [x] = True
nondescending (x:xs) =
	if head xs < x 
	then False
	else nondescending xs

constant :: [Int] -> Bool
constant [] = True
constant [x] = True
constant (x:xs) =
	if head xs == x 
	then constant xs
	else False

nonascending :: [Int] -> Bool
nonascending [] = True
nonascending [x] = True
nonascending (x:xs) =
	if head xs > x 
	then False
	else nonascending xs

descending :: [Int] -> Bool
descending [] = True
descending [x] = True
descending (x:xs) =
	if head xs >= x 
	then False
	else descending xs

data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted deriving (Show)


sortType :: [Int] -> TypeOfSort
sortType [] = Ascending
sortType [x] = Ascending
sortType xs =
	if ascending xs
	then Ascending
	else if descending xs
	then Descending
	else if constant xs
	then Constant
	else if nonascending xs
	then NonAscending
	else if nondescending xs
	then NonDescending
	else NotSorted
--EX5
operate :: Char -> Double -> Double -> Double
operate '+' x y = x + y
operate '-' x y = x - y
operate '*' x y = x * y
operate '/' x y = x / y

calc :: [Char] -> [Double] -> Double
calc [] [x] = x
calc [] [] = 0
calc ls xs = 
	if head ls == '+' || head ls == '-' || head ls == '*' || head ls == '/'
	then calc (tail ls) ([operate (head ls) (head (tail xs)) (head xs)] ++ (tail (tail xs)))
	else calc (tail ls) ([fromIntegral(ord (head ls) - ord '0')] ++ xs)

rpcalc :: [Char] -> Double
rpcalc [] = 0
rpcalc xs = calc xs []
--EX6
distance :: (Int,Int) -> (Int,Int) -> Float
distance (a,b) (c,d) = sqrt(fromIntegral((a - c) * (a - c) + (b - d) * (b - d)))

makelist :: Int -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)] -> Float -> [(Int,Int)]
makelist _ _ [] ls _ = ls
makelist 0 _ _ _ _ = []
makelist k (a,b) xs ls t = 
	if k > length ls
	then 
		if (distance (a,b) (head xs)) >= t
		then makelist k (a,b) (tail xs) ([head xs] ++ ls) t
		else makelist k (a,b) (tail xs) ([head xs] ++ ls) (distance (a,b) (head xs))
	else 
		if (distance (a,b) (head xs)) >= t
		then makelist k (a,b) (tail xs) ls t
		else makelist k (a,b) (tail xs) ([head xs] ++ tail ls) (distance (a,b) (head xs))
neighbours :: Int -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
neighbours k (a,b) xs = makelist k (a,b) xs [] 9999999
--EX7
data SearchTree = Node (SearchTree) Int (SearchTree) | Leaf (Int) deriving (Eq, Ord, Show, Read)

heightTree :: SearchTree -> Int
heightTree  (Leaf a) = 1
heightTree (Node lt a rt) = 1 + maximum [heightTree lt, heightTree rt]

value :: SearchTree -> Int
value (Leaf a) = a
value (Node lt a rt) = a 

tranverse :: SearchTree -> Bool
tranverse (Leaf a) = True
tranverse (Node lt a rt) =
	if value lt < a && value rt > a
		then True && tranverse lt && tranverse rt
		else False


balance :: SearchTree -> Bool
balance (Leaf a) = True
balance (Node lt a rt) =
	if (heightTree lt) - (heightTree rt) < 2 && (heightTree lt) - (heightTree rt) > -2
		then True && balance lt && balance rt
		else False

balanced :: SearchTree -> Bool
balanced xs = balance xs && tranverse xs
--EX8
newtonRootD :: (Double,Double) -> (Double,Double)
newtonRootD (x,d) = ((x+x/d)/2,d)

newtonRootS :: Double -> [(Double,Double)]
newtonRootS d = iterate newtonRootD (1,d)

newtonRootSequence :: Double -> [Double]
newtonRootSequence d = [fst a | a <- newtonRootS d]

newtonRootCompare ::  Double->Double->Double -> Double
newtonRootCompare d x e =
	if fst (newtonRootD (x,d)) - x <= e && fst (newtonRootD (x,d)) - x >= 0 - e
	then fst (newtonRootD (x,d))
	else newtonRootCompare d (fst (newtonRootD (x,d))) e

newtonRoot :: Double->Double -> Double
newtonRoot d e = newtonRootCompare d 1 e
--EX9
operator :: Int -> (Int->Int -> Int)
operator 0 = f where
	f a b = b + 1
operator 1 = f where
	f a b = a + b
operator n = f where
	f a b = foldl1 (operator (n - 1)) $ replicate b a

hyperOperation :: Int->Int->Int -> Int
hyperOperation x a b = (operator x) a b
--EX10 & 11
numtobin :: Int->Int -> [Int]
numtobin 0 0 = [0]
numtobin 0 1 = [1]
numtobin k x = 
	if x >= 2^k
	then [1] ++ numtobin (k - 1) (x - 2^k)
	else [0] ++ numtobin (k - 1) x

count1 :: [Int] -> Int
count1 [] = 0
count1 xs = 
	if head xs == 1
	then 1 + count1 (tail xs)
	else 0 + count1 (tail xs)
addlast :: [Int] -> [Int]
addlast xs =
	if count1 xs `mod` 2 == 0
	then xs ++ [0]
	else xs ++ [1]

chartobin :: Char -> [Int]
chartobin x = addlast (numtobin 7 (ord x))

encode :: [Char] -> [Int]
encode [] = []
encode xs = chartobin (head xs) ++ encode (tail xs)

bintonum :: [Int] -> Int
bintonum [] = 0
bintonum xs = (head xs) * 2^((length xs) - 1) + bintonum (tail xs)

bintochar :: [Int] -> Char
bintochar xs = chr (bintonum xs)

decode :: [Int] -> [Char]
decode [] = []
decode xs = 
	if length xs `mod` 9 == 0
	then [bintochar (take 8 $ xs)] ++ decode (drop 9 $ xs) 
	else []
--EX12
sumOfChanges :: [Int]->[Int] -> Int
sumOfChanges [] [] = 0
sumOfChanges (x:xs) (y:ys) = x * y + sumOfChanges xs ys
checkAnswer :: [Int]->[[Int]]->Int -> [Int]
checkAnswer xs [] _ = replicate (length xs) (-1)
checkAnswer xs (y:ys) n = 
	if sumOfChanges xs y == n
	then y
	else checkAnswer xs ys n
addArray :: [Int]->[[Int]] -> [[Int]]
addArray x xs = [(a:xss) | xss <- xs, a <- x ]
backtrack :: Int->Int -> [[Int]]
backtrack x 1 = [ [a] | a <- [0..x]]
backtrack x n = addArray [ a| a <- [0..x]] (backtrack x (n-1))
makeChange :: Int->[Int] -> [Int]
makeChange k xs = checkAnswer xs (backtrack a b) k where
	a = k `div` (minimum xs)
	b = length xs
--Ex13
goodsequence :: (Int,[Int]) ->Int -> Int
goodsequence (_,[]) _ = 0
goodsequence (x,xs) k = (goodsequence (x,tail xs) (k + 1)) + x^k * head xs 

int2sequence :: Int->Int->Int -> [Int]
int2sequence 0 k a = []
int2sequence x k a = 
	if k^(x-1) > a
		then [0] ++ int2sequence (x-1) k a
		else [a `div` k^(x-1)] ++ int2sequence (x-1) k (a `mod` k^(x-1))

logNum :: Int->Int->Int -> Int
logNum k x a =
	if x^k > a
		then k -1
		else logNum (k+1) x a
nextvalue :: (Int,[Int]) -> (Int,[Int])
nextvalue (x,xs) = (x+1,reverse (int2sequence ((logNum 0 (x + 1) a) + 1) (x+1) a))
	where a = (goodsequence (x+1,xs) 0) - 1

goodsteinSequence :: (Int,[Int]) -> [(Int,[Int])]
goodsteinSequence (k,[]) = [(k,[])]
goodsteinSequence xs = [xs] ++ goodsteinSequence a
	where a = nextvalue xs 
--EX15
calw :: Int -> Int
calw z = floor a where
	a = (sqrt(fromIntegral (8 * z - 1)) - 1) / 2
calt :: Int -> Int
calt w = (w * w + w) `div` 2
pairvalue :: Int->Int -> Int
pairvalue a b = (a + b)*(a + b + 1) `div` 2 + b
valuetopair :: Int -> (Int,Int)
valuetopair z = (a,b) where
	w = calw z
	t = calt w
	b = z - t
	a = w - b
isCantorPair :: Int -> Bool
isCantorPair n =
	if x + y == b
		then True
		else False
	where 
		a = fst (valuetopair n)
		b = snd (valuetopair n)
		x = fst (valuetopair a)
		y = snd (valuetopair a)