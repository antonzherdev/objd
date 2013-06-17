module ObjC.Struct
( Property(..),
  PropertyModifier(..),
  Statement(..),
  ImplSynthenyze(..),
  ImplFun(..),
  Fun(..),
  FunType(..),
  FunPar(..),
  Stm(..),
  Exp(..),
  ImplField(..)
) where

import           Data.Char
import           Ex.String




data Statement =
	Import String | ImportLib String
	| Interface { interfaceName :: String, interfaceExtends :: String, interfaceProperties :: [Property], interfaceFuns :: [Fun] }
	| Implementation {implName :: String, implFields :: [ImplField], implSynthenyzes :: [ImplSynthenyze], implFuns :: [ImplFun]}

data Property = Property {propertyName :: String, propertyType :: String, propertyModifiers :: [PropertyModifier]}

data PropertyModifier = ReadOnly | NonAtomic deriving(Eq)

data ImplField = ImplField {implFieldName :: String, implFieldType :: String}

data ImplSynthenyze = ImplSynthenyze String String

data ImplFun = ImplFun {implFunType :: Fun, implExps :: [Stm]}

data Fun = Fun {funType :: FunType, funReturnType :: String, funName :: String, funPars :: [FunPar]}
data FunType = ObjectFun | InstanceFun
data FunPar = FunPar {funParName :: String, funParDataType :: String, funParVar :: String}

{- EXPRESSIONS -}

data Stm =
	If Exp [Stm] [Stm]
	| Set Exp Exp
	| Stm Exp
	| Return Exp
	| Nop

data Exp =
	Self | Super
	| Call {callInst :: Exp, callName :: String, callPars :: [(String, Exp)]}
	| Ref String
	| IntConst Int
	| Eq Exp Exp | NotEq Exp Exp
	| Dot Exp String

ind :: String -> String
ind = ("    " ++ )
showStms :: [Stm] -> String
showStms = unlines . stms
stms :: [Stm] -> [String]
stms = map ind . concatMap stmLines
unlines' :: [String] -> String
unlines' [] = ""
unlines' a = unlines a ++ "\n"


instance Show Statement where
	show (Import s) = "#import \"" ++ s ++ "\""
	show (ImportLib s) = "#import <" ++ s ++ ">"
	show (Interface name extends properties funs) =
		"@interface " ++ name ++ " : " ++ extends ++ "\n"
		 ++ (unlines' . map show) properties
		 ++ (unlines  . map (( ++ ";") . show)) funs
		 ++ "@end"
	show Implementation{implName = iName, implFields = fields, implSynthenyzes = synzs, implFuns = funs} =
		"@implementation " ++ iName
		++ showImplFields fields
		++ showSynthenizes synzs
		++ showImplFuns funs
		++ "@end"
		where
		showImplFields [] = "\n"
		showImplFields a = "{\n"
			++ (unlines . map (ind . showImplField)) a
			++ "}\n"
		showImplField (ImplField name tp) = tp ++ " " ++ name ++ ";"
		showSynthenizes = unlines . map showSynthenize
		showSynthenize (ImplSynthenyze name "") = "@synthenize " ++ name ++ ";"
		showSynthenize (ImplSynthenyze name var) = "@synthenize " ++ name ++ " = " ++ var ++ ";"
		showImplFuns = unlines . map (("\n" ++ ) . show)

instance Show ImplFun where
	show (ImplFun fun exps) =
		show fun ++ " {\n"
		++ showStms exps
		++ "}\n"
instance Show Fun where
	show (Fun tp ret name pars) =
		show tp ++ " (" ++ ret ++ ")" ++ name ++ cap  (strs' " " pars)
		where
			cap :: String -> String
			cap "" = ""
			cap (x:xs) = toUpper x : xs
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

showOp :: (Show a, Show b) => a -> String -> b -> String
showOp l op r = show l ++ " " ++ op ++ " " ++ show r
instance Show Exp where
	show Self = "self"
	show Super = "super"
	show (Call inst name pars) = "[" ++ show inst ++ " " ++ name ++ (strs " " . map (\(nm, e) -> nm ++ ":" ++ show e)) pars ++ "]"
	show (Ref name) = name
	show (IntConst i) = show i
	show (Eq l r) = showOp l "==" r
	show (NotEq l r) = showOp l "!=" r
	show (Dot l r) = show l ++ "." ++ r

instance Show Stm where
	show s = unlines $ stmLines s

stmLines :: Stm -> [String]
stmLines (If cond t f) =
	["if(" ++ show cond ++ ") {"] ++ stms t ++ ["}"]
	++ (case f of
		[] -> []
		ff -> ["else {"] ++ stms ff ++ ["}"])
stmLines (Set l r) = [show l ++ " = " ++ show r ++ ";"]
stmLines (Stm e) = [show e ++ ";"]
stmLines (Return e) = ["return " ++ show e ++ ";"]
stmLines (Nop) = [""]

