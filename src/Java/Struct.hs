module Java.Struct ( 
	File(..), Class(..)
) where

import           Ex.String
--import 			 Data.Decimal
--import           Control.Monad
--import 			 Control.Arrow

data File = File Package [Import] Class
type Package = [String]
type Import = [String]
instance Show File where
	show (File pack imps cls) = "package " ++ strs "." pack ++ ";\n\n" ++ showImports ++ show cls
		where
			showImports = case imps of
				[] -> ""
				_ -> (unlines $ map ((++ ";") . ("import " ++ ) . strs ".") imps) ++ "\n"

data Class = Class {className :: String}
instance Show Class where
	show _ = ""