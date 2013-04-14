module Ex.String(mkString, strs, strs', MultiLineShow(..)) where

mkString :: (a -> String) -> String -> [a] -> String
mkString _ _ [] = ""
mkString f _ [x] = f x
mkString f d (x:xs) = f x ++ d ++ mkString f d xs

strs :: String -> [String] -> String
strs = mkString (id)

strs' :: (Show a) => String -> [a] -> String
strs' = mkString (show)

class (Show a) => MultiLineShow a where
	multiLineShow :: a -> [String]
