import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Number

--ex1

zipL :: [Int] -> [Int] -> [(Int,Int)]
zipL [] _ = []
zipL _ [] = []
zipL (x:xs) (y:ys) = [(x,y)] ++ zipL xs ys

unzipL :: [(Int,Int)] -> ([Int],[Int])
unzipL x = (a,b)
	where 
		a = [fst k | k <- x]
		b = [snd k | k <- x]

--ex2

_zipL :: [Int] -> [Int] -> [[Int]]
_zipL [] x = [x]
_zipL x [] = [x]
_zipL (x:xs) (y:ys) = [[x,y]] ++ _zipL xs ys

--ex3

multiZipL :: [[Int]] -> [[Int]]
multiZipL [] = []
multiZipL x = [[head k | k <- x, (null k) == False]] ++ multiZipL [tail k | k <- x, (null k) == False, (null (tail k)) == False]

--ex4

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = int
eol = char '\n'


multiZipF :: FilePath -> IO [[Int]]
multiZipF fP = do 
	xs <- parseFromFile csvFile fP
	return xs

