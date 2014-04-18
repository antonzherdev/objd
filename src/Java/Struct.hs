module Java.Struct ( 
	File(..), Class(..), Visibility(..), ClassType(..),
	TP(..), Generic(..), tpRef, Def(..), DefPar, DefMod(..), Stm(..), Exp(..)
) where

import           Ex.String
import 			 Data.List
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
	, classExtends :: Maybe TP, classImplements :: [TP], classDefs :: [Def]} 
instance Show Class where
	show cl@Class{} = wrapStr "" " " (show (classVisibility cl)) ++ show (classType cl) ++ " " ++ className cl 
		++ pstrs' "<" ", " ">" (classGenerics cl) 
		++ maybe "" ((" extends " ++ ). show)  (classExtends cl)
		++ pstrs' (if classType cl == ClassTypeClass then " implements " else " extends ") ", " "" (classImplements cl)
		++ " {\n" 
		++ (unlines . map ind . concatMap (showDef cl) )  (classDefs cl)
		++ "}"

data Visibility = Private | Protected | Public | Package deriving (Eq)
instance Show Visibility where
	show Private = "private"
	show Protected = "protected"
	show Public = "public"
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
data TP = TPRef [TP] String | TPArr TP Int
tpRef :: String -> TP
tpRef = TPRef []
instance Show TP where
	show (TPRef [] nm) = nm
	show (TPRef exts nm) = nm ++ "<" ++ strs' ", " exts ++ ">"
	show (TPArr tp n) = show tp ++ "[" ++ show n ++ "]"

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Def
 -----------------------------------------------------------------------------------------------------------------------------------------------}
data Def = 
	  Def{defMods :: [DefMod], defTp :: TP, defName :: String, defPars :: [DefPar], defStms :: [Stm]}
	| Constructor {defMods :: [DefMod], defPars :: [DefPar], defStms :: [Stm]}
	| Field {defMods :: [DefMod], defTp :: TP, defName :: String, defExp :: Exp}
type DefPar = (TP, String)
data DefMod = DefModStatic | DefModAbstract | DefModFinal | DefModVisability Visibility deriving (Eq)

showDef :: Class -> Def -> [String]
showDef _ d@Field{} = [mods ++ " " ++ show (defTp d) ++ " " ++ defName d] `glue` body
	where 
		mods = strs' " " (defMods d)
		body = case defExp d of
			Nop -> [";"] 
			e -> [" = "] `glue` mapNotFirst ind (showExp e) `appp` ";"
showDef cl d@Def{} = [wrapStr "" " " mods ++ show (defTp d) ++ " " ++ defName d ++ showPars d] `glue` body
	where 
		clTp = classType cl
		mods = strs' " " (if clTp == ClassTypeInterface then filter isValidModForInterface (defMods d) else defMods d)
		isValidModForInterface _ = False
		body = if clTp == ClassTypeInterface || DefModAbstract `elem` defMods d then [";"] else (mapFirst (" " ++ ) (showStmsInBrackets (defStms d)))
showDef cl d@Constructor{} = [wrapStr "" " " (strs' " " (delete DefModStatic (defMods d))) ++ 
	className cl ++ showPars d ++ " "] `glue` showStmsInBrackets (defStms d)

showPars :: Def -> String
showPars d = "(" ++ mkString showDefPar "," (defPars d) ++ ")"
	where showDefPar (tp, nm) = show tp ++ " " ++ nm

instance Show DefMod where
	show DefModStatic = "static"
	show DefModAbstract = "abstract"
	show DefModFinal = "final"
	show (DefModVisability v) = show v

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Stm
 -----------------------------------------------------------------------------------------------------------------------------------------------}
showStmsInBrackets :: [Stm] -> [String]
showStmsInBrackets stms = ["{"] ++ (map ind . concatMap showStm) stms ++ ["}"]

data Stm = Stm Exp | Braces [Stm] | Return Exp

showStm :: Stm -> [String]
showStm (Stm Nop) = []
showStm (Stm e) = showExp e `appp` ";"
showStm (Return e) = ["return "] `glue` showExp e `appp` ";"
showStm (Braces stms) = showStmsInBrackets stms


data Exp = Nop | IntConst Int | ExpError String | Call String [TP] [Exp] | New Exp | Dot Exp Exp | Ref String

showExp :: Exp -> [String]
showExp Nop = []
showExp (IntConst i) = [show i]
showExp (Ref s) = [s]
showExp (Call name gens pars) = [name ++ pstrs' "<" ", " ">" gens ++ "("] `glue` (glueAll ", " . map showExp) pars `appp` ")"
showExp (New e) = ["new "] `glue` showExp e
showExp (Dot l r) = showExp l `appp` "." `glue` showExp r
showExp (ExpError e) = ["ERROR: " ++ e]
