module ObjC.Struct ( Property(..), PropertyModifier(..),FileStm(..), ImplSynthesize(..), ImplFun(..), Fun(..), FunType(..), FunPar(..),
  Stm(..), Exp(..), ImplField(..), StructField(..), CFunPar(..), CFunMod(..)
) where

import           Ex.String

data FileStm =
	Import String | ImportLib String | EmptyLine 
	| Interface { interfaceName :: String, interfaceExtends :: String, interfaceProperties :: [Property], interfaceFuns :: [Fun] }
	| Implementation {implName :: String
		, implFields :: [ImplField]
		, implSynthesizes :: [ImplSynthesize]
		, implFuns :: [ImplFun]
		, implStaticFields :: [ImplField]}
	| Struct {structName :: String, structFields :: [StructField]}
	| TypeDefStruct {oldName :: String, newName :: String}
	| CFun {cfunMods :: [CFunMod], cfunReturnType :: String, cfunName :: String, cfunPars :: [CFunPar], cfunExps :: [Stm]}

data StructField = StructField{structFieldType :: String, structFieldName :: String}

data Property = Property {propertyName :: String, propertyType :: String, propertyModifiers :: [PropertyModifier]}

data PropertyModifier = ReadOnly | NonAtomic | Retain deriving(Eq)

data ImplField = ImplField {implFieldName :: String, implFieldType :: String}

data ImplSynthesize = ImplSynthesize String String

data ImplFun = ImplFun {implFunType :: Fun, implExps :: [Stm]}

data Fun = Fun {funType :: FunType, funReturnType :: String, funName :: String, funPars :: [FunPar]}
data FunType = ObjectFun | InstanceFun
data FunPar = FunPar {funParName :: String, funParDataType :: String, funParVar :: String}

data CFunPar = CFunPar {cfunParDataType :: String, cfunParName :: String}
data CFunMod = CFunStatic | CFunInline
{- EXPRESSIONS -}

data Stm =
	If Exp [Stm] [Stm]
	| Set (Maybe MathTp) Exp Exp
	| Stm Exp
	| Return Exp
	| Throw Exp
	| Var{varType :: String, varName :: String, varExp :: Exp}

data Exp =
	Self | Super
	| Call {callInst :: Exp, callName :: String, callPars :: [(String, Exp)]}
	| CCall {callName :: String, ccallPars :: [Exp]}
	| Ref String
	| IntConst Int
	| BoolConst Bool
	| FloatConst Int Int
	| StringConst String
	| BoolOp BoolTp Exp Exp 
	| MathOp MathTp Exp Exp 
	| Dot Exp String
	| PlusPlus Exp
	| MinusMinus Exp
	| Nop
	| Nil
	| InlineIf Exp Exp Exp
	
showStms :: [Stm] -> String
showStms = unlines . stms
stms :: [Stm] -> [String]
stms = map ind . concatMap stmLines
unlines' :: [String] -> String
unlines' [] = ""
unlines' a = unlines a ++ "\n"


instance Show FileStm where
	show (Import s) = "#import \"" ++ s ++ "\""
	show (ImportLib s) = "#import <" ++ s ++ ">"
	show (EmptyLine) = ""
	show (TypeDefStruct o n) = "typedef struct " ++ o ++ " " ++ n ++ ";"
	show (Struct name fields) = "struct " ++ name ++ " {\n" ++
		(unlines . map (ind . show)) fields ++
		"};"
	show (CFun mods ret name pars exps) = strs " " (map show mods ++ [ret]) ++ " " ++ name ++ "(" ++ (strs ", " . map show) pars ++ ") {\n" ++
			showStms exps ++
		"}"
	show (Interface name extends properties funs) =
		"@interface " ++ name ++ " : " ++ extends ++ "\n"
		 ++ (unlines' . map show) properties
		 ++ (unlines  . map (( ++ ";") . show)) funs
		 ++ "@end\n\n"
	show Implementation{implName = iName, implFields = fields, implSynthesizes = synzs, implFuns = funs, implStaticFields = stFields} =
		"@implementation " ++ iName
		++ showImplFields fields
		++ showStFields stFields
		++ showSynthenizes synzs ++ "\n"
		++ showImplFuns funs
		++ "@end\n\n"
		where
		showImplFields [] = "\n"
		showImplFields a = "{\n"
			++ (unlines . map (ind . showImplField)) a
			++ "}\n"
		showImplField (ImplField name tp) = tp ++ " " ++ name ++ ";"
		showSynthenizes = unlines . map showSynthenize
		showStFields = unlines . map showStField
		showSynthenize (ImplSynthesize name "") = "@synthesize " ++ name ++ ";"
		showSynthenize (ImplSynthesize name var) = "@synthesize " ++ name ++ " = " ++ var ++ ";"
		showImplFuns = unlines . map show
		showStField (ImplField nm tp) = "static " ++ nm ++ " " ++ tp ++ ";"

instance Show CFunMod where
	show CFunStatic = "static"
	show CFunInline = "inline"
instance Show StructField where
	show (StructField t n) = t ++ " " ++ n ++ ";"
instance Show CFunPar where
	show (CFunPar t n) = t ++ " " ++ n

instance Show ImplFun where
	show (ImplFun fun exps) =
		show fun ++ " {\n"
		++ showStms exps
		++ "}\n"
instance Show Fun where
	show (Fun tp ret name pars) =
		show tp ++ " (" ++ ret ++ ")" ++ name ++ cap  (strs' " " pars)
instance Show FunPar where
 	show (FunPar name tp var) = name ++ ":(" ++ tp ++ ")" ++ var
instance Show FunType where
	show InstanceFun = "-"
	show ObjectFun = "+"


instance Show Property where
	show (Property name tp mods) = "@property (" ++ strs' ", " mods ++ ") " ++ tp ++ " " ++ name ++ ";"


instance Show PropertyModifier where
	show ReadOnly = "readonly"
	show NonAtomic = "nonatomic"
	show Retain = "retain"

instance Show Exp where
	show Self = "self"
	show Super = "super"
	show (Call inst name pars) = "[" ++ show inst ++ " " ++ name ++ (cap . strs " " . map (\(nm, e) -> nm ++ ":" ++ show e)) pars ++ "]"
	show (CCall name pars) = name ++ "(" ++ strs' ", " pars ++ ")"
	show (Ref name) = name
	show (IntConst i) = show i
	show Nil = "nil"
	show (BoolConst True) = "YES"
	show (BoolConst False) = "NO"
	show (FloatConst a b) = show a ++ "." ++ show b
	show (StringConst s) = '@' : show s
	show (BoolOp t l r) = showOp l (show t) r
	show (MathOp t l r) = showOp l  (show t) r
	show (Dot l r) = show l ++ "." ++ r
	show (PlusPlus e) = show e ++ "++"
	show (MinusMinus e) = show e ++ "--"
	show (InlineIf c t f) = show c ++ " ? " ++ show t ++ " : " ++ show f

instance Show Stm where
	show s = unlines $ stmLines s

stmLines :: Stm -> [String]
stmLines (If cond t f) =
	["if(" ++ show cond ++ ") {"] ++ stms t ++ ["}"]
	++ (case f of
		[] -> []
		ff -> ["else {"] ++ stms ff ++ ["}"])
stmLines (Set Nothing l r) = [show l ++ " = " ++ show r ++ ";"]
stmLines (Set (Just tp) l r) = [show l ++ " " ++ show tp ++ "= " ++ show r ++ ";"]
stmLines (Stm Nop) = [""]
stmLines (Stm e) = [show e ++ ";"]
stmLines (Return e) = ["return " ++ show e ++ ";"]
stmLines (Throw e) = ["@throw " ++ show e ++ ";"]
stmLines (Var tp name Nop) = [tp ++ " " ++ name ++ ";"]
stmLines (Var tp name e) = [tp ++ " " ++ name ++ " = " ++ show e ++ ";"]

