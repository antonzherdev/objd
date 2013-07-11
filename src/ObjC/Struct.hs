module ObjC.Struct ( Property(..), PropertyModifier(..),FileStm(..), ImplSynthesize(..), ImplFun(..), Fun(..), FunType(..), FunPar(..),
  Stm(..), Exp(..), ImplField(..), CFunPar(..), CFunMod(..), DataType(..)
) where

import           Ex.String
import 			 Data.Decimal

data FileStm =
	Import String | ImportLib String | EmptyLine 
	| Interface { interfaceName :: String, interfaceExtends :: String, interfaceProperties :: [Property], interfaceFuns :: [Fun] }
	| Protocol { interfaceName :: String, interfaceFuns :: [Fun] }
	| Implementation {implName :: String
		, implFields :: [ImplField]
		, implSynthesizes :: [ImplSynthesize]
		, implFuns :: [ImplFun]
		, implStaticFields :: [ImplField]}
	| Struct {structName :: String, structFields :: [ImplField]}
	| TypeDefStruct {oldName :: String, newName :: String}
	| CFunDecl {cfunMods :: [CFunMod], cfunReturnType :: DataType, cfunName :: String, cfunPars :: [CFunPar]}
	| CFun {cfunMods :: [CFunMod], cfunReturnType :: DataType, cfunName :: String, cfunPars :: [CFunPar], cfunExps :: [Stm]}
	| ClassDecl String

data Property = Property {propertyName :: String, propertyType :: DataType, propertyModifiers :: [PropertyModifier]}

data PropertyModifier = ReadOnly | NonAtomic | Retain | Weak deriving(Eq)

data ImplField = ImplField {implFieldName :: String, implFieldType :: DataType, implFieldsMods :: [String]}

data ImplSynthesize = ImplSynthesize String String

data ImplFun = ImplFun {implFunType :: Fun, implExps :: [Stm]}

data Fun = Fun {funType :: FunType, funReturnType :: DataType, funName :: String, funPars :: [FunPar]}
data FunType = ObjectFun | InstanceFun
data FunPar = FunPar {funParName :: String, funParDataType :: DataType, funParVar :: String}

data CFunPar = CFunPar {cfunParDataType :: DataType, cfunParName :: String}
data CFunMod = CFunStatic | CFunInline
{- EXPRESSIONS -}

data DataType = TPSimple String | TPBlock DataType [DataType]

data Stm =
	If Exp [Stm] [Stm]
	| Set (Maybe MathTp) Exp Exp
	| Stm Exp
	| Return Exp
	| Throw Exp
	| Var{varType :: DataType, varName :: String, varExp :: Exp}

data Exp =
	Self | Super
	| Call {callInst :: Exp, callName :: String, callPars :: [(String, Exp)]}
	| CCall {callName :: String, ccallPars :: [Exp]}
	| Ref String
	| IntConst Int
	| BoolConst Bool
	| FloatConst Decimal
	| StringConst String
	| ObjCConst Exp
	| BoolOp BoolTp Exp Exp 
	| MathOp MathTp Exp Exp 
	| Dot Exp String
	| PlusPlus Exp
	| MinusMinus Exp
	| Nop
	| Nil
	| InlineIf Exp Exp Exp
	| Index Exp Exp
	| Arr [Exp]
	| Map [(Exp, Exp)]
	| Lambda [(String, DataType)] [Stm] DataType
	| Not Exp
	| Error String
	
showStms :: [Stm] -> String
showStms = unlines . stms
stms :: [Stm] -> [String]
stms = map ind . concatMap stmLines
unlines' :: [String] -> String
unlines' [] = ""
unlines' a = unlines a ++ "\n"

instance Show DataType where
	show (TPSimple s) = s
	show (TPBlock d t) = show d ++ "(^)" ++ "(" ++ strs' ", " t ++ ")"

showDecl ::  DataType -> String ->String
showDecl (TPSimple tp) name = tp ++ " " ++ name
showDecl (TPBlock d t) name = show d ++ "(^" ++ name ++ ")" ++ "(" ++ strs' ", " t ++ ")"

instance Show FileStm where
	show (Import s) = "#import \"" ++ s ++ "\""
	show (ImportLib s) = "#import <" ++ s ++ ">"
	show (EmptyLine) = ""
	show (TypeDefStruct o n) = "typedef struct " ++ o ++ " " ++ n ++ ";"
	show (Struct name fields) = "struct " ++ name ++ " {\n" ++
		(unlines . map (ind . show)) fields ++
		"};"
	show (CFunDecl mods ret name pars) = strs " " (map show mods ++ [show ret]) ++ " " ++ name ++ "(" ++ (strs ", " . map show) pars ++ ");"
	show (CFun mods ret name pars exps) = strs " " (map show mods ++ [show ret]) ++ " " ++ name ++ "(" ++ (strs ", " . map show) pars ++ ") {\n" ++
			showStms exps ++
		"}"
	show (Interface name extends properties funs) =
		"@interface " ++ name ++ " : " ++ extends ++ "\n"
		 ++ (unlines' . map show) properties
		 ++ (unlines  . map (( ++ ";") . show)) funs
		 ++ "@end\n\n"
	show (Protocol name funs) =
		"@protocol " ++ name ++ "\n"
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
			++ (unlines . map (ind . show)) a
			++ "}\n"
		
		showSynthenizes = unlines . map showSynthenize
		showStFields = unlines . map showStField
		showSynthenize (ImplSynthesize name "") = "@synthesize " ++ name ++ ";"
		showSynthenize (ImplSynthesize name var) = "@synthesize " ++ name ++ " = " ++ var ++ ";"
		showImplFuns = unlines . map show
		showStField (ImplField nm tp mods) = "static " ++  (strs " " mods) `tryCon` " " ++ showDecl tp nm ++ ";"
	show (ClassDecl name) = "@class " ++ name ++ ";"
instance Show ImplField where
	show(ImplField name tp mods) = (strs " " mods) `tryCon` " " ++ showDecl tp name ++ ";"
instance Show CFunMod where
	show CFunStatic = "static"
	show CFunInline = "inline"
instance Show CFunPar where
	show (CFunPar t n) = showDecl t n

instance Show ImplFun where
	show (ImplFun fun exps) =
		show fun ++ " {\n"
		++ showStms exps
		++ "}\n"
instance Show Fun where
	show (Fun tp ret name pars) =
		show tp ++ " (" ++ show ret ++ ")" ++ name ++ cap  (strs' " " pars)
instance Show FunPar where
 	show (FunPar name tp var) = name ++ ":(" ++ show tp ++ ")" ++ var
instance Show FunType where
	show InstanceFun = "-"
	show ObjectFun = "+"


instance Show Property where
	show (Property name tp mods) = "@property (" ++ strs' ", " mods ++ ") " ++ showDecl tp name ++ ";"


instance Show PropertyModifier where
	show ReadOnly = "readonly"
	show NonAtomic = "nonatomic"
	show Retain = "retain"
	show Weak = "weak"

instance Show Exp where
	show s = strs "\n" $ expLines s

instance Show Stm where
	show s = unlines $ stmLines s

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ [] = []
mapFirst f a = f (head a) : tail a
mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast f a = init a ++ [f $ last a]
appendLast :: String -> [String] -> [String]
appendLast s [] = [s]
appendLast s r = mapLast (++ s) r
app :: [String] -> String -> [String]
a `app` b = appendLast b a
glue :: [String] -> [String] -> [String]
[] `glue` [] = []
[] `glue` b = b
a `glue` [] = a
a `glue` b = init a ++ [last a ++ head b] ++ tail b
glueAll :: String -> [[String]] -> [String]
glueAll _ [] = []
glueAll _ [x] = x
glueAll s (a:b:xs) = glueAll s $ ((a `app` s) `glue` b):xs

multiLineIf :: Stm -> [String]
multiLineIf (If cond t f) = ["if(" ++ show cond ++ ") {" ] ++ stms t ++ ["} else {"] ++ stms f ++ ["}"]

stmLines :: Stm -> [String]
stmLines (If cond [t] []) = ["if(" ++ show cond ++ ") " ] `glue` stmLines t
stmLines (If cond t []) = ["if(" ++ show cond ++ ") {"] ++ stms t ++ ["}"]
stmLines i@(If cond [If{}] _) = multiLineIf i
stmLines i@(If cond _ [If{}]) = multiLineIf i
stmLines (If cond [t] [f]) = (["if(" ++ show cond ++ ") " ] `glue` stmLines t) ++ (["else "] `glue` stmLines f)
stmLines i@If{} = multiLineIf i

stmLines (Set Nothing l r) = (expLines l `app` " = ") `glue` (expLines r `app` ";")
stmLines (Set (Just tp) l r) = (expLines l `app` (" " ++ show tp ++ "= ")) `glue` (expLines r `app` ";")
stmLines (Stm Nop) = [""]
stmLines (Stm e) = appendLast ";" $ expLines e
stmLines (Return e) = ["return "] `glue` (expLines e `app` ";")
stmLines (Throw e) = ["@throw "] `glue` (expLines e `app` ";")
stmLines (Var tp name Nop) = [showDecl tp name ++ ";"]
stmLines (Var tp name e) = [showDecl tp name ++ " = "] `glue` (expLines e `app` ";")

expLines :: Exp -> [String]
expLines Self = ["self"]
expLines Super = ["super"]
expLines (Call inst name pars) = ["["] `glue` (expLines inst `app` (" " ++ name)) `glue` (pars' `app` "]")
	where pars' = (mapFirst cap . glueAll " " . map (\(nm, e) -> [nm ++ ":"] `glue` expLines e)) pars
expLines (CCall name pars) = [name ++ "("] `glue` (pars' `app` ")")
	where pars' = (glueAll ", " . map expLines) pars
expLines (Ref name) = [name]
expLines (IntConst i) = [show i]
expLines Nil = ["nil"]
expLines (BoolConst True) = ["YES"]
expLines (BoolConst False) = ["NO"]
expLines (FloatConst i) = [show i]
expLines (StringConst s) = ['@' : show s]
expLines (BoolOp t l r) = [showOp l (show t) r]
expLines (MathOp t l r) = [showOp l  (show t) r]
expLines (Dot l r) = [show l ++ "." ++ r]
expLines (PlusPlus e) = appendLast "++" (expLines e)
expLines (MinusMinus e) = appendLast "--" (expLines e)
expLines (InlineIf c t f) = (expLines c `app` " ? ") `glue` (expLines t `app` " : ") `glue` (expLines f) 
expLines (Index e i) = (expLines e `app` "[") `glue` (expLines i `app` "]")
expLines (Arr e) = ["(@[" ++ strs' ", " e ++ "])"]
expLines (Map e) = ["(@{" ++ (strs ", " . map(\(k, v) -> show k ++ " : " ++ show v) ) e ++ "})"]
expLines (ObjCConst e) = ["@" ++ show e]
expLines (Lambda pars e rtp) = ["^" ++ show rtp ++ "(" ++ strs ", " (map showPar pars) ++ ") {"] ++ stms e ++ ["}"]
	where showPar(name, tp) = showDecl tp name
expLines (Not e) = ["!("] `glue` (expLines e `app` ")")
expLines (Error s) = ["ERROR: "] `glue` lines s 

