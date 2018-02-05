import Data.Char
import Control.Applicative
data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)
-- Exercise 1a

remove :: Int -> [Int] -> [Int]
remove x [] = []
remove x (y:ys) = 
	if x == y
	then remove x ys
	else [y] ++ remove x ys
freeVariables :: Expr -> [Int]
freeVariables (Var x) = [x]
freeVariables (App e1 e2) = union (freeVariables e1)  (freeVariables e2)
freeVariables (Lam x e) = remove x (freeVariables e)

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

-- Exercise 1b

changevalue :: Int -> Int -> Int -> Int
changevalue x n1 n2 
	| x == n1 = n2
	| x /= n1 = x

renameCount :: Int -> Expr -> Int -> Int -> Expr
renameCount 2 (Lam x e) n1 n2 = Lam x (renameCount 2 e n1 n2)
renameCount 0 (Lam x e) n1 n2
	| x == n1 = Lam n2 (renameCount 1 e n1 n2)
	| x /= n1 = Lam x (renameCount 0 e n1 n2)
renameCount 1 (Lam x e) n1 n2
	| x == n1 = Lam x (renameCount 2 e n1 n2)
	| x /= n1 = Lam x (renameCount 1 e n1 n2)
renameCount 0 (Var x) n1 n2 = Var x
renameCount 1 (Var x) n1 n2 = Var (changevalue x n1 n2)
renameCount 2 (Var x) n1 n2 = Var x
renameCount k (App e1 e2) n1 n2 = App (renameCount k e1 n1 n2) (renameCount k e2 n1 n2)

rename :: Expr -> Int -> Int -> Expr
rename e n1 n2 = renameCount 0 e n1 n2

--Exercise 1c

cantorPair :: Int -> Int -> Int
cantorPair x y = (x + y)*(x + y + 1) `div` 2 + y

alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent (Var x) (Var y) = x == y
alphaEquivalent (Var x) (Lam y e) = False
alphaEquivalent (Var x) (App e e1) = False
alphaEquivalent (Lam y e) (Var x) = False
alphaEquivalent (App e e1) (Var x) = False
alphaEquivalent (Lam y e1) (App e2 e3) = False
alphaEquivalent (Lam y e1) (Lam x e2)
	| y == x = alphaEquivalent e1 e2
	| y /= x = alphaEquivalent (rename (Lam y e1) y a) (rename (Lam x e2) x a)
	where a = maximum [maximumValue (Lam y e1), maximumValue (Lam x e2)] + 1
alphaEquivalent (App e1 e2) (App e3 e4) = alphaEquivalent e1 e3 && alphaEquivalent e2 e4

--Exercise 1d

isLambda :: Expr -> Bool
isLambda (Var x) = False
isLambda (Lam x e) = True
isLambda (App e1 e2) = False

isVar :: Expr -> Bool
isVar (Var x) = True
isVar (Lam x e) = False
isVar (App e1 e2) = False
hasRedex :: Expr -> Bool
hasRedex (Var x) = False
hasRedex (App e1 e2) = (isVar e2 && isLambda e1) || hasRedex e1 || hasRedex e2
hasRedex (Lam x e) = hasRedex e

--Exercise 1e

free :: Int -> Expr -> Bool
free x (Var y) = x == y
free x (App e1 e2) = free x e1 || free x e2
free x (Lam y e)
	| x == y = False
	| x /= y = free x e

maximumValue :: Expr -> Int
maximumValue (Var x) = x
maximumValue (Lam x e) = maximum [x,maximumValue e]
maximumValue (App e1 e2) = maximum [maximumValue e1, maximumValue e2]

substitute :: Expr -> Int -> Expr -> Expr
substitute (Var x) y e
	| x == y = e
	| x /= y = Var x
substitute (Lam x e1) y e2
	| x == y = Lam x e1
	| x /= y && (free x e2) == False = Lam x (substitute e1 y e2)
	| x /= y && (free x e2) = substitute b y e2
	where 
		d = Lam x e1
		c = maximumValue d + 1
		b = rename d x c

substitute (App e1 e2) y e3 = App (substitute e1 y e3) (substitute e2 y e3)

-- Exercise 2

isAllVar :: Expr -> Bool
isAllVar (Var _) = True
isAllVar (Lam _ _) = False
isAllVar (App e1 e2) = isAllVar e1 && isAllVar e2

bracket :: Int -> Expr -> Bool
bracket _ (Lam x e) = True
bracket _ (Var x) = False
bracket 0 (App e1 e2) = bracket 0 e1
bracket 1 (App e1 e2) = bracket 1 e2

prettyPrintFull :: Expr -> String
prettyPrintFull (Var x) = "x" ++ show x
prettyPrintFull (App e1 e2) 
	| isAllVar (App e1 e2) == False && isAllVar e1 && bracket 1 e2 = prettyPrintFull e1 ++ "(" ++ prettyPrintFull e2 ++ ")"
	| isAllVar (App e1 e2) == False && isAllVar e2 && bracket 0 e1 = "(" ++ prettyPrintFull e1 ++ ")" ++ prettyPrintFull e2
	| isAllVar (App e1 e2) == False && isAllVar e2 && bracket 0 e1 == False = prettyPrintFull e1 ++ prettyPrintFull e2
	| isAllVar (App e1 e2) == False && isAllVar e1 && bracket 1 e2 == False = prettyPrintFull e1 ++ prettyPrintFull e2
	| isAllVar (App e1 e2) && isVar e2 = prettyPrintFull e1 ++ prettyPrintFull e2
	| isAllVar (App e1 e2) && isVar e2 == False = prettyPrintFull e1 ++ "(" ++ prettyPrintFull e2 ++ ")"
	| allFalse && bracket 0 e1 && bracket 1 e2 = "(" ++ prettyPrintFull e1 ++ ")" ++ "(" ++ prettyPrintFull e2 ++ ")"
	| allFalse && bracket 0 e1 = "(" ++ prettyPrintFull e1 ++ ")" ++ prettyPrintFull e2
	| allFalse && bracket 1 e2 = prettyPrintFull e1 ++ "(" ++ prettyPrintFull e2 ++ ")"
	| otherwise = prettyPrintFull e1 ++ prettyPrintFull e2
	where allFalse = isAllVar (App e1 e2) == False && isAllVar e1 == False && isAllVar e2 == False
prettyPrintFull (Lam x e) = "/x" ++ show x ++ "." ++ prettyPrintFull e

clean :: String -> String
clean [] = []
clean (y:ys) 
	| y == '.' && head ys == '/' = tail ys
	| otherwise = [y] ++ clean ys

replace :: String -> String
replace [] = []
replace (x:xs) 
	| x == '.' = "->" ++ replace xs
	| x == ' ' = replace xs
	| otherwise = (x: replace xs)

prettyPrint :: Expr -> String 
prettyPrint e = replace (clean (prettyPrintFull e))

-- Parsing Import

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)


-- Exercise 3

data ExtExpr = ExtApp ExtExpr ExtExpr | ExtLam [Int] ExtExpr | ExtVar Int deriving (Show, Eq)
data Expression = Variable String | Lambda String Expression | Application Expression Expression deriving (Show, Eq)

split :: Char -> String -> String -> [String] -> [String]
split _ x [] ys = ys ++ [x]
split k x (x0:xs) ys 
	| x0 == k = split k [] xs (ys ++ [x])
	| otherwise = split k (x ++ [x0]) xs ys  

splitOn :: Char -> String -> [String]
splitOn k x = split k [] x [] 


string2int :: String -> Int
string2int [] = 0
string2int (x:xs) = (ord x - ord '0') * 10^(length xs) + string2int xs

variableIdentifier :: Parser String
variableIdentifier =
    do
    char 'x'
    e <- some digit
    return e

singleLambdaParser :: Parser Expression
singleLambdaParser = 
    (do char '\\'  -- lambda: \vars . e 
        vl <- some alphanum
        space
        char '-'
        char '>'
        space
        e <- lambdaParser
        return  (Lambda vl e))
    <|> (do char '(' -- parenthesized expression: (e)
            e <- lambdaParser
            char ')'
            return e)
    <|> do v <- variableIdentifier -- simple variable: v
           return (Variable v)

lambdaParser :: Parser Expression
lambdaParser = 
	do 
	l <- some singleLambdaParser 
	space
	return $ foldl1 (\f x -> Application f x) l

parseLambda :: String -> Maybe Expression
parseLambda s = 
    let ps = parse lambdaParser s in
        if ps == [] then Nothing
        else let [(e,s')] = ps in 
        if s' /= "" then Nothing      
        else Just e

convert :: Expression -> ExtExpr
convert (Variable x) = ExtVar (string2int x)
convert (Lambda x e) = ExtLam (tail (map string2int (splitOn 'x' x))) (convert e)
convert (Application e1 e2) = ExtApp (convert e1) (convert e2)

convertable :: Expression -> Bool
convertable (Variable x) = True
convertable (Lambda x e) = syntax x && convertable e
convertable (Application e1 e2) = convertable e1 && convertable e2

syntax :: String -> Bool
syntax "x" = False
syntax "1" = True
syntax "2" = True
syntax "3" = True
syntax "4" = True
syntax "5" = True
syntax "6" = True
syntax "7" = True
syntax "8" = True
syntax "9" = True
syntax "0" = True
syntax (x:xs) 
	| x == 'x' = x /= head xs && syntax xs
	| x == '0' = syntax xs
	| x == '1' = syntax xs
	| x == '2' = syntax xs
	| x == '3' = syntax xs
	| x == '4' = syntax xs
	| x == '5' = syntax xs
	| x == '6' = syntax xs
	| x == '7' = syntax xs
	| x == '8' = syntax xs
	| x == '9' = syntax xs
	| otherwise = False

convertMaybe :: Maybe Expression -> Maybe ExtExpr
convertMaybe Nothing = Nothing
convertMaybe (Just e)
	| convertable e = Just (convert e)
	| otherwise = Nothing

parseLam :: String -> Maybe ExtExpr
parseLam s = convertMaybe (parseLambda s) 

-- Question 4

data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int deriving (Show)
data Ecl = EVar Int | EApp Ecl Ecl | ELam Int Ecl | S' | K' deriving (Eq)
translate :: Expr -> ExprCL
translate e = ecl2exprcl (switchback (switch e)) 

switch :: Expr -> Ecl
switch (Var x) = EVar x
switch (Lam x e) = ELam x (switch e)
switch (App e1 e2) = EApp (switch e1) (switch e2)

freeEcl :: Int -> Ecl -> Bool
freeEcl x (EVar y) = x == y
freeEcl x (ELam y e)
	| y == x = False
	| otherwise = freeEcl x e
freeEcl x (EApp e1 e2) = freeEcl x e1 || freeEcl x e2
freeEcl x K' = False
freeEcl x S' = False

switchback :: Ecl -> Ecl
switchback (EVar x) = EVar x
switchback (EApp e1 e2) = EApp (switchback e1) (switchback e2)
switchback K' = K'
switchback S' = S'
switchback (ELam x (EVar y)) 
	| x == y = EApp (EApp S' K') K' 
switchback (ELam x e) 
	| freeEcl x e == False = EApp K' (switchback e)
switchback (ELam x (ELam y e))
	| freeEcl x e = switchback (ELam x (switchback(ELam y e)))
switchback (ELam x (EApp e1 e2)) 
	| freeEcl x (EApp e1 e2) = EApp (EApp S' (switchback (ELam x e1))) (switchback (ELam x e2))

ecl2exprcl :: Ecl -> ExprCL
ecl2exprcl (EVar x) = VarCL x
ecl2exprcl (EApp e1 e2) = AppCL (ecl2exprcl e1) (ecl2exprcl e2)
ecl2exprcl K' = K
ecl2exprcl S' = S 
