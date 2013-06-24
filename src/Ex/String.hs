module Ex.String(mkString, strs, strs', MultiLineShow(..), ind, showOp, showOp', cap, MathTp(..), BoolTp(..)) where

import           Data.Char


mkString :: (a -> String) -> String -> [a] -> String
mkString _ _ [] = ""
mkString f _ [x] = f x
mkString f d (x:xs) = f x ++ d ++ mkString f d xs

strs :: String -> [String] -> String
strs = mkString id

strs' :: (Show a) => String -> [a] -> String
strs' = mkString show

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


data MathTp = Plus | Minus | Mul | Div
data BoolTp = Eq | NotEq | More | MoreEq | Less | LessEq	
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
