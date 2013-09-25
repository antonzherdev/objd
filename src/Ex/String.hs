module Ex.String(mkString, strs, strs', MultiLineShow(..), ind, showOp, showOp', cap, MathTp(..), BoolTp(..), tryCon, zipWithIndex, startsWith, lines2) where

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

mkString :: (a -> String) -> String -> [a] -> String
mkString _ _ [] = ""
mkString f _ [x] = f x
mkString f d (x:xs) = f x ++ d ++ mkString f d xs

strs :: String -> [String] -> String
strs = mkString id

strs' :: (Show a) => String -> [a] -> String
strs' = mkString show

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


data MathTp = Plus | Minus | Mul | Div
data BoolTp = Eq | NotEq | More | MoreEq | Less | LessEq | And | Or
instance Show MathTp where
	show Plus = "+"
	show Minus = "-"
	show Mul = "*"
	show Div = "/"
instance Show BoolTp where
	show Eq = "=="
	show NotEq = "!="
	show More = ">"
	show MoreEq = ">="
	show Less = "<"
	show LessEq = "<="
	show And = "&&"
	show Or = "||"
