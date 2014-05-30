module Ex.String(mkString, strs, strs', MultiLineShow(..), ind, showOp, showOp', cap, MathTp(..), BoolTp(..), 
	tryCon, zipWithIndex, startsWith, lines2, pstrs, pstrs', pmkString,
	mapFirst, mapLast, appendLast, appp, glue, glueAll, mapNotFirst, wrapStr, splitOn) where

import           Data.Char

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith (a:as) (b:bs) = a == b && startsWith as bs
startsWith _ _ = False

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex = doZip 0
	where
		doZip :: Int -> [a] -> [(a, Int)]
		doZip _ [] = []
		doZip i (x:xs) = (x, i) : doZip (i + 1) xs

pmkString :: (a -> String) -> String -> String -> String -> [a] -> String
pmkString _ _ _ _ [] = ""
pmkString f p _ s [x] = p ++ f x ++ s
pmkString f p d s xs = p ++ mkString f d xs ++ s


mkString :: (a -> String) -> String -> [a] -> String
mkString _ _ [] = ""
mkString f _ [x] = f x
mkString f d (x:xs) = f x ++ d ++ mkString f d xs

strs :: String -> [String] -> String
strs = mkString id

strs' :: (Show a) => String -> [a] -> String
strs' = mkString show

pstrs :: String -> String -> String -> [String] -> String
pstrs = pmkString id

pstrs' :: (Show a) => String -> String -> String -> [a] -> String
pstrs' = pmkString show


break2 :: (a -> Bool) -> [a] -> ([a],Maybe [a])
break2 _ xs@[]           =  (xs, Nothing)
break2 p xs@(x:xs')
           | p x        =  ([], Just xs)
           | otherwise  =  let (ys,zs) = break2 p xs' in (x:ys, zs)

lines2 :: String -> [String]
lines2 s = 
	let 
		(l, s') = break2 (== '\n') s
		ss = case s' of
			Nothing -> []
			Just "" -> [""]
			Just (_:s'') -> lines2 s''
		in  l : ss

class (Show a) => MultiLineShow a where
	multiLineShow :: a -> [String]

showOp :: (Show a, Show b) => a -> String -> b -> String
showOp l op r = show l ++ " " ++ op ++ " " ++ show r
showOp' :: (Show a, Show b) => a -> String -> b -> String
showOp' l op r = show l ++ op ++ show r

ind :: String -> String
ind = ("    " ++ )

cap :: String -> String
cap "" = ""
cap (x:xs) = toUpper x : xs

tryCon :: String -> String -> String
tryCon _ "" = ""
tryCon "" _ = ""
tryCon a b = a ++ b

wrapStr :: String -> String -> String -> String
wrapStr _ _ "" = ""
wrapStr p s ss = p ++ ss ++ s

splitOn :: (Eq a) =>  a -> [a] -> [[a]]
splitOn d arr = recsl [] arr 
	where
		recsl [] [] = []
		recsl a [] = [reverse a] 
		recsl a (x:xs)
			| x == d = reverse a : recsl [] xs
			| otherwise = recsl (x:a) xs

data MathTp = Plus | Minus | Mul | Div deriving (Eq)
data BoolTp = Eq | NotEq | More | MoreEq | Less | LessEq | And | Or | ExactEq | ExactNotEq deriving (Eq)
instance Show MathTp where
	show Plus = "+"
	show Minus = "-"
	show Mul = "*"
	show Div = "/"
instance Show BoolTp where
	show Eq = "=="
	show ExactEq = "==="
	show ExactNotEq = "!=="
	show NotEq = "!="
	show More = ">"
	show MoreEq = ">="
	show Less = "<"
	show LessEq = "<="
	show And = "&&"
	show Or = "||"


mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ [] = []
mapFirst f a = f (head a) : tail a
mapNotFirst :: (a -> a) -> [a] -> [a]
mapNotFirst _ [] = []
mapNotFirst f a = head a : map f (tail a)
mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f a = init a ++ [f $ last a]
appendLast :: String -> [String] -> [String]
appendLast s [] = [s]
appendLast s r = mapLast (++ s) r
appp :: [String] -> String -> [String]
a `appp` b = appendLast b a
glue :: [String] -> [String] -> [String]
[] `glue` [] = []
[] `glue` b = b
a `glue` [] = a
a `glue` b = init a ++ [last a ++ head b] ++ tail b
glueAll :: String -> [[String]] -> [String]
glueAll _ [] = []
glueAll _ [x] = x
glueAll s (a:b:xs) = glueAll s $ ((a `appp` s) `glue` b):xs

