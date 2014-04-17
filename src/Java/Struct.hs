module Java.Struct ( 
	File(..), Class(..), Visibility(..), ClassType(..),
	TP(..), Generic(..), ref
) where

import           Ex.String
--import 			 Data.Decimal
--import           Control.Monad
--import 			 Control.Arrow

data File = File{fileIsTest :: Bool, filePackage :: Package, fileImports :: [Import], fileClass :: Class}
type Package = [String]
type Import = [String]
instance Show File where
	show (File _ pack imps cls) = "package " ++ strs "." pack ++ ";\n\n" ++ showImports ++ show cls
		where
			showImports = case imps of
				[] -> ""
				_ -> (unlines $ map ((++ ";") . ("import " ++ ) . strs ".") imps) ++ "\n"

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Class
 -----------------------------------------------------------------------------------------------------------------------------------------------}

data Class = Class {classVisibility :: Visibility, classType :: ClassType, className :: String, classGenerics :: [Generic]
	, classExtends :: Maybe TP, classImplements :: [TP]} 
instance Show Class where
	show cl@Class{} = show (classVisibility cl) ++ show (classType cl) ++ " " ++ className cl 
		++ pstrs' "<" ", " ">" (classGenerics cl) 
		++ maybe "" ((" extends " ++ ). show)  (classExtends cl)
		++ pstrs' (if classType cl == ClassTypeClass then " implements " else " extends ") ", " "" (classImplements cl)
		++ " {\n" ++
		 "}"

data Visibility = Private | Protected | Public | Package
instance Show Visibility where
	show Private = "private "
	show Protected = "protected "
	show Public = "public "
	show Package = ""
data ClassType = ClassTypeClass | ClassTypeInterface deriving (Eq)
instance Show ClassType where
	show ClassTypeClass = "class"
	show ClassTypeInterface = "interface"
	
data Generic = Generic{genericExtends :: [TP], genericName :: String}
instance Show Generic where
	show (Generic [] nm) = nm
	show (Generic exts nm ) = nm ++ " extends " ++ strs' " & " exts

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - DataType
 -----------------------------------------------------------------------------------------------------------------------------------------------}
data TP = TPRef [TP] String
ref :: String -> TP
ref = TPRef []
instance Show TP where
	show (TPRef [] nm) = nm
	show (TPRef exts nm) = nm ++ "<" ++ strs' ", " exts ++ ">"
