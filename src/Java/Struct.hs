module Java.Struct ( 
	File(..), Class(..), Visibility(..), ClassType(..),
	TP(..), Generic(..), tpRef, Def(..), DefPar, DefMod(..), Stm(..), Exp(..), DefAnnotation(..), Import, ClassMod(..)
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

data Class = Class {classMods :: [ClassMod], classType :: ClassType, className :: String, classGenerics :: [Generic]
	, classExtends :: Maybe TP, classImplements :: [TP], classDefs :: [Def]} 
instance Show Class where
	show cl@Class{} = wrapStr "" " " (strs' " " (classMods cl)) ++ show (classType cl) ++ " " ++ className cl 
		++ pstrs' "<" ", " ">" (classGenerics cl) 
		++ maybe "" ((" extends " ++ ). show)  (classExtends cl)
		++ pstrs' (if classType cl == ClassTypeClass then " implements " else " extends ") ", " "" (classImplements cl)
		++ " {\n" 
		++ (unlines . map ind . concatMap (showDef (classType cl, className cl)) )  (classDefs cl)
		++ "}"

data ClassMod = ClassModVisibility Visibility | ClassModAbstract | ClassModFinal
instance Show ClassMod where
	show (ClassModVisibility v) = show v
	show ClassModAbstract = "abstract"
	show ClassModFinal = "final"
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
	
data Generic = Generic{genericExtends :: [TP], genericName :: String} deriving (Eq)
instance Show Generic where
	show (Generic [] nm) = nm
	show (Generic exts nm ) = nm ++ " extends " ++ strs' " & " exts

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - DataType
 -----------------------------------------------------------------------------------------------------------------------------------------------}
data TP = TPRef [TP] String | TPArr TP Int | TPAnyGeneric | TPUnknown String deriving (Eq)
tpRef :: String -> TP
tpRef = TPRef []
instance Show TP where
	show (TPAnyGeneric) = "?"
	show (TPRef [] nm) = nm
	show (TPRef exts nm) = nm ++ "<" ++ strs' ", " exts ++ ">"
	show (TPArr tp n) = show tp ++ "[" ++ show n ++ "]"
	show (TPUnknown e) = e
removeGenerics :: TP -> TP
removeGenerics (TPRef _ nm) = TPRef [] nm
removeGenerics t = t
{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Def
 -----------------------------------------------------------------------------------------------------------------------------------------------}
data Def = 
	  Def{defAnnotations :: [DefAnnotation], defMods :: [DefMod], defGenerics :: [Generic]
	  , defTp :: TP, defName :: String, defPars :: [DefPar], defStms :: [Stm]}
	| Constructor {defAnnotations :: [DefAnnotation], defMods :: [DefMod], defPars :: [DefPar], defStms :: [Stm]}
	| Field {defAnnotations :: [DefAnnotation], defMods :: [DefMod], defTp :: TP, defName :: String, defExp :: Exp} deriving (Eq)
type DefPar = ([DefMod], TP, String)
data DefMod = DefModStatic | DefModAbstract | DefModFinal | DefModOverride | DefModVisability Visibility deriving (Eq)

showDef :: (ClassType, String) -> Def -> [String]
showDef _ d@Field{} = concatMap showAnnotation (defAnnotations d) 
	++ [mods ++ " " ++ show (defTp d) ++ " " ++ defName d] `glue` body
	where 
		mods = strs' " " (defMods d)
		body = case defExp d of
			Nop -> [";"] 
			e -> [" = "] `glue` mapNotFirst ind (showExp e) `appp` ";"
showDef (clTp, _) d@Def{} = concatMap showAnnotation (defAnnotations d)
	 ++ [wrapStr "" " " mods ++ pstrs' "<" ", " "> " (defGenerics d) ++ show (defTp d) ++ " " ++ defName d ++ showPars d] `glue` body
	where 
		mods = strs' " " (if clTp == ClassTypeInterface then filter isValidModForInterface (defMods d) else defMods d)
		isValidModForInterface _ = False
		body = if clTp == ClassTypeInterface || DefModAbstract `elem` defMods d then [";"] else (mapFirst (" " ++ ) (showStmsInBrackets (defStms d)))
showDef (_, clNm) d@Constructor{} = concatMap showAnnotation (defAnnotations d) 
	++ [wrapStr "" " " (strs' " " (delete DefModStatic (defMods d))) ++ 
	clNm ++ showPars d ++ " "] `glue` showStmsInBrackets (defStms d)

showPars :: Def -> String
showPars d = "(" ++ mkString showDefPar ", " (defPars d) ++ ")"
	where showDefPar (mods, tp, nm) = pstrs' "" " " " " mods ++ show tp ++ " " ++ nm

instance Show DefMod where
	show DefModStatic = "static"
	show DefModAbstract = "abstract"
	show DefModFinal = "final"
	show DefModOverride = ""
	show (DefModVisability v) = show v

data DefAnnotation = DefAnnotation String [Exp] deriving (Eq)
showAnnotation :: DefAnnotation -> [String]
showAnnotation (DefAnnotation nm []) = ["@" ++ nm]
showAnnotation (DefAnnotation nm pars) = ["@" ++ nm ++ "("] `glue` (glueAll ", " . map showExp) pars `appp` ")"

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Stm
 -----------------------------------------------------------------------------------------------------------------------------------------------}
showStmsInBrackets :: [Stm] -> [String]
showStmsInBrackets stms = ["{"] ++ (map ind . concatMap showStm) stms ++ ["}"]

data Stm = Stm Exp | Braces [Stm] | Return Exp | If Exp [Stm] [Stm] | While Exp [Stm]
	| Throw Exp | Set (Maybe MathTp) Exp Exp | Val [DefMod] TP String Exp | Break deriving (Eq)

showStm :: Stm -> [String]
showStm (Stm Nop) = []
showStm (Break) = ["break;"]
showStm (Stm e) = showExp e `appp` ";"
showStm (Return e) = ["return "] `glue` showExp e `appp` ";"
showStm (Set tp l r) = showExp l `appp` maybe " = " (\t -> " " ++ show t ++ "= ") tp `glue` showExp r `appp` ";"
showStm (Throw e) = ["throw "] `glue` showExp e `appp` ";"
showStm (If cond t []) = (["if("] `glue` showExp cond `appp` ") ") `glue` showStmsInBrackets t
showStm (While cond w) = (["while("] `glue` showExp cond `appp` ") ") `glue` showStmsInBrackets w
showStm (If cond t f) = (showStm (If cond t []) `appp` " else ") `glue` showStmsInBrackets f 
showStm (Braces stms) = showStmsInBrackets stms
showStm (Val mods tp nm Nop) = [pstrs' "" " " " " mods ++ show tp ++ " " ++ nm ++ ";"]
showStm (Val mods tp nm e) = [pstrs' "" " " " " mods ++ show tp ++ " " ++ nm ++ " = "] `glue` showExp e `appp` ";"


data Exp = Nop | IntConst Int | ExpError String 
	| Call Bool String [TP] [Exp] | New [Def] Exp | Dot Exp Exp | Ref String | InlineIf Exp Exp Exp | This
	| BoolOp BoolTp Exp Exp | MathOp MathTp Exp Exp | Null | BoolConst Bool | InstanceOf Exp TP | Cast TP Exp
	| StringConst String | Index Exp Exp | Not Exp | Negative Exp | MinusMinus Exp | PlusPlus Exp  deriving (Eq)

showExp :: Exp -> [String]
showExp Nop = []
showExp Null = ["null"]
showExp This = ["this"]
showExp (BoolConst True) = ["true"]
showExp (BoolConst False) = ["false"]
showExp (IntConst i) = [show i]
showExp (StringConst s) = [show s]
showExp (Ref s) = [s]
showExp (Call isStatic name gens pars) = [(if isStatic then pstrs' "<" ", " ">" gens ++ name else name ++ pstrs' "<" ", " ">" gens)++ "("] 
	`glue` (glueAll ", " . map showExp) pars `appp` ")"
showExp (New [] e) = ["new "] `glue` showExp e
showExp (New defs e) = (["new "] `glue` showExp e `appp` " {") ++  (map ind . concatMap (showDef (ClassTypeClass, ""))) defs ++ ["}"]
showExp (Dot l r) = showExp l `appp` "." `glue` showExp r
showExp (Index e i) = (showExp e `appp` "[") `glue` (showExp i `appp` "]")
showExp (InlineIf cond t f) = ((["("] `glue` showExp cond `appp` ") ? (") `glue` showExp t `appp` ") : (")  `glue` showExp f `appp` ")"
showExp (InstanceOf e tp) = showExp e `appp` (" instanceof " ++ show (removeGenerics tp))
showExp (Cast tp e) = ["((" ++ show tp ++ ")"] `glue` showExp e `appp` ")"
showExp (Not e) = ["!("] `glue` showExp e `appp` ")"
showExp (Negative e) = ["-("] `glue` showExp e `appp` ")"
showExp (MinusMinus e) = showExp e `appp` "--"
showExp (PlusPlus e) = showExp e `appp` "++"
showExp (ExpError e) = ["ERROR: " ++ e]
showExp (BoolOp t l r) = (mbb l `appp` (" " ++ show t ++ " ")) `glue` mbb r
	where 
		mbb :: Exp -> [String]
		mbb b@(BoolOp tt _ _) 
			| needb t tt = ["("] `glue` showExp b `appp` ")"
			| otherwise = showExp b
		mbb e = showExp e
		needb And Or = True
		needb Or And = True
		needb _ _ = False
showExp (MathOp t l r) = (mbb l `appp` (" " ++ show t ++ " ")) `glue` mbb r
	where 
		mbb :: Exp -> [String]
		mbb b@(MathOp tt _ _) 
			| needb t tt = ["("] `glue` showExp b `appp` ")"
			| otherwise = showExp b
		mbb e = showExp e
		needb Div Plus = True
		needb Div Minus = True
		needb Div Mul = True
		needb Mul Plus = True
		needb Mul Minus = True
		needb Minus Minus = True
		needb _ _ = False

