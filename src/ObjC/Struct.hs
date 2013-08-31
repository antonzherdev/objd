module ObjC.Struct ( Property(..), PropertyModifier(..),FileStm(..), ImplSynthesize(..), ImplFun(..), Fun(..), FunType(..), FunPar(..),
  Stm(..), Exp(..), ImplField(..), CFunPar(..), CFunMod(..), DataType(..), Extends(..)
) where

import           Ex.String
import 			 Data.Decimal

data FileStm =
	Import String | ImportLib String | EmptyLine 
	| Interface { interfaceName :: String, interfaceExtends :: Extends, interfaceProperties :: [Property], interfaceFuns :: [Fun] }
	| Protocol { interfaceName :: String, interfaceExtends :: Extends, interfaceFuns :: [Fun] }
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
	| ProtocolDecl String
data Extends = Extends String [String]
data Property = Property {propertyName :: String, propertyType :: DataType, propertyModifiers :: [PropertyModifier]}

data PropertyModifier = ReadOnly | NonAtomic | Retain | Weak | Copy deriving(Eq)

data ImplField = ImplField {implFieldName :: String, implFieldType :: DataType, implFieldsMods :: [String], implFieldExp :: Exp}

data ImplSynthesize = ImplSynthesize String String

data ImplFun = ImplFun {implFunType :: Fun, implExps :: [Stm]}
instance Eq ImplFun where
	a == b = (implFunType a) == (implFunType b)

data Fun = Fun {funType :: FunType, funReturnType :: DataType, funName :: String, funPars :: [FunPar]}
instance Eq Fun where
	a == b = funType a == funType b && funName a == funName b && funPars a == funPars b

data FunType = ObjectFun | InstanceFun deriving(Eq)
data FunPar = FunPar {funParName :: String, funParDataType :: DataType, funParVar :: String}
instance Eq FunPar where
	a == b = funParName a == funParName b

data CFunPar = CFunPar {cfunParDataType :: DataType, cfunParName :: String}
data CFunMod = CFunStatic | CFunInline
{- EXPRESSIONS -}

data DataType = TPSimple String [String]| TPBlock DataType [DataType] | TPArr Int String 

data Stm =
	If Exp [Stm] [Stm]
	| While Exp [Stm]
	| Do Exp [Stm]
	| Set (Maybe MathTp) Exp Exp
	| Stm Exp
	| Return Exp
	| Throw Exp
	| Var{varType :: DataType, varName :: String, varExp :: Exp, varMods :: [String]}
	| Break

data Exp =
	Self | Super
	| Call {callInst :: Exp, callName :: String, callPars :: [(String, Exp)], callVargs :: [Exp]}
	| CCall Exp [Exp]
	| Ref String
	| IntConst Int
	| BoolConst Bool
	| FloatConst Decimal
	| StringConst String
	| ObjCConst Exp
	| BoolOp BoolTp Exp Exp 
	| MathOp MathTp Exp Exp 
	| Dot Exp Exp
	| PlusPlus Exp
	| MinusMinus Exp
	| Nop
	| Nil
	| InlineIf Exp Exp Exp
	| Index Exp Exp
	| Arr [Exp]
	| EArr [Exp]
	| Map [(Exp, Exp)]
	| Lambda [(String, DataType)] [Stm] DataType
	| Not Exp
	| Negative Exp
	| Error String
	| Cast DataType Exp
	| ShortCast DataType Exp
	| EArrConst String [Exp]
	| ProtocolRef Exp

showStms :: [Stm] -> String
showStms = unlines . stms
stms :: [Stm] -> [String]
stms = map ind . concatMap stmLines
unlines' :: [String] -> String
unlines' [] = ""
unlines' a = unlines a ++ "\n"
kw :: String -> String
kw "switch" = "aSwitch"
kw "default" = "aDefault"
kw s = s

instance Show DataType where
	show (TPSimple s []) = s
	show (TPArr 0 s) = s ++ "[]"
	show (TPArr n s) = s ++ "[" ++ show n ++ "]"
	show (TPSimple s pr) = s ++ "<" ++ strs ", " pr ++ ">"
	show (TPBlock d t) = show d ++ "(^)" ++ "(" ++ strs' ", " t ++ ")"

showDecl ::  DataType -> String ->String
showDecl (TPArr 0 tp) name = tp ++ " " ++ name ++ "[]"
showDecl (TPArr n tp) name = tp ++ " " ++ name ++ "[" ++ show n ++ "]"
showDecl tp@TPSimple{} name = show tp ++ " " ++ name
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
		"@interface " ++ name ++ " : " ++ show extends ++ "\n"
		 ++ (unlines' . map show) properties
		 ++ (unlines  . map (( ++ ";") . show)) funs
		 ++ "@end\n\n"
	show (Protocol name (Extends cl trs) funs) =
		"@protocol " ++ name ++ "<" ++ cl ++ unwords (map (", " ++ ) trs) ++ ">\n"
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
		showStField (ImplField nm tp mods Nop) = "static " ++  (strs " " mods) `tryCon` " " ++ showDecl tp nm ++  ";"
		showStField (ImplField nm tp mods e) = strs "\n" $ ["static " ++  (strs " " mods) `tryCon` " " ++ showDecl tp nm ++ " = "] `glue` (expLines e `app` ";")
	show (ClassDecl name) = "@class " ++ name ++ ";"
	show (ProtocolDecl name) = "@protocol " ++ name ++ ";"
instance Show Extends where
	show (Extends cl []) = cl
	show (Extends cl a) = cl ++ "<" ++ strs ", " a ++ ">"
instance Show ImplField where
	show(ImplField name tp mods _) = (strs " " mods) `tryCon` " " ++ showDecl tp (kw name) ++ ";"
instance Show CFunMod where
	show CFunStatic = "static"
	show CFunInline = "inline"
instance Show CFunPar where
	show (CFunPar t n) = showDecl t (kw n)

instance Show ImplFun where
	show (ImplFun fun exps) =
		show fun ++ " {\n"
		++ showStms exps
		++ "}\n"
instance Show Fun where
	show (Fun tp ret name pars) =
		show tp ++ " (" ++ show ret ++ ")" ++ kw name ++ cap  (strs' " " pars)
instance Show FunPar where
 	show (FunPar name tp var) = kw name ++ ":(" ++ show tp ++ ")" ++ kw var
instance Show FunType where
	show InstanceFun = "-"
	show ObjectFun = "+"


instance Show Property where
	show (Property name tp mods) = "@property (" ++ strs' ", " mods ++ ") " ++ showDecl tp (kw name) ++ ";"


instance Show PropertyModifier where
	show ReadOnly = "readonly"
	show NonAtomic = "nonatomic"
	show Retain = "retain"
	show Copy = "copy"
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
stmLines (While cond t) = ["while(" ++ show cond ++ ") {"] ++ stms t ++ ["}"]
stmLines (Do cond t) = ["do {"] ++ stms t ++ ["} while(" ++ show cond ++ ");"]
stmLines i@(If _ [If{}] _) = multiLineIf i
stmLines i@(If _ _ [If{}]) = multiLineIf i
stmLines (If cond [t] [f]) = (["if(" ++ show cond ++ ") " ] `glue` stmLines t) ++ (["else "] `glue` stmLines f)
stmLines i@If{} = multiLineIf i

stmLines (Set Nothing l r) = (expLines l `app` " = ") `glue` (expLines r `app` ";")
stmLines (Set (Just tp) l r) = (expLines l `app` (" " ++ show tp ++ "= ")) `glue` (expLines r `app` ";")
stmLines (Stm Nop) = [""]
stmLines (Stm e) = appendLast ";" $ expLines e
stmLines (Return e) = ["return "] `glue` (expLines e `app` ";")
stmLines (Throw e) = ["@throw "] `glue` (expLines e `app` ";")
stmLines (Var tp name Nop mods) = [(unwords . map (++ " ")) mods ++ showDecl tp name ++ ";"]
stmLines (Var tp name e mods) = [(unwords . map (++ " ")) mods ++ showDecl tp name ++ " = "] `glue` (expLines e `app` ";")
stmLines (Break) = ["break;"]

expLines :: Exp -> [String]
expLines Self = ["self"]
expLines Super = ["super"]
expLines (Call inst name pars vargs) = ["["] `glue` (expLines inst `app` (" " ++ kw name)) `glue` pars' `glue` (vargs'  `app` "]")
	where 
		pars' = (mapFirst cap . glueAll " " . map (\(nm, e) -> [kw nm ++ ":"] `glue` expLines e)) pars
		vargs' = (glueAll "" . map varg') vargs
		varg' = ([", "] `glue` ) . expLines
expLines (CCall name pars) = (expLines name `app` "(") `glue` (pars' `app` ")")
	where pars' = (glueAll ", " . map expLines) pars
expLines (Ref name) = [kw name]
expLines (IntConst i) = [show i]
expLines Nil = ["nil"]
expLines (BoolConst True) = ["YES"]
expLines (BoolConst False) = ["NO"]
expLines (FloatConst i) = [show i]
expLines (StringConst s) = case lines s of
		[] -> ["@\"\""]
		[x] -> ['@' : '"' : x ++ "\""]
		x:xs -> ('@' : '"' : x ++ "\\n\"") : map (\line -> ind $ '"' : line ++ "\\n\"" ) (init xs) ++ [ind $ '"' : last xs ++ "\""]
expLines (BoolOp t l r) = [mbb l ++ " " ++ show t ++ " " ++ mbb r]
	where 
		mbb :: Exp -> String
		mbb b@(BoolOp tt _ _) 
			| needb t tt = "(" ++ show b ++ ")"
			| otherwise = show b
		mbb e = show e
		needb And Or = True
		needb Or And = True
		needb _ _ = False
expLines (MathOp t l r) = [mbb l ++ " " ++ show t ++ " " ++ mbb r]
	where 
		mbb :: Exp -> String
		mbb b@(MathOp tt _ _) 
			| needb t tt = "(" ++ show b ++ ")"
			| otherwise = show b
		mbb e = show e
		needb Div Plus = True
		needb Div Minus = True
		needb Mul Plus = True
		needb Mul Minus = True
		needb _ _ = False
expLines (Dot l r) = (expLines l `app` ".") `glue` expLines r
expLines (PlusPlus e) = appendLast "++" (expLines e)
expLines (MinusMinus e) = appendLast "--" (expLines e)
expLines (InlineIf c t f) = ["(("] `glue` (expLines c `app` ") ? ") `glue` (expLines t `app` " : ") `glue` (expLines f `app` ")") 
expLines (Index e i) = (expLines e `app` "[") `glue` (expLines i `app` "]")
expLines (Arr e) = ["(@[" ++ strs' ", " e ++ "])"]
expLines (EArr e) = ["{" ++ strs' ", " e ++ "}"]
expLines (EArrConst name e) = ["[ " ++ name ++ "(" ++ show (length e) ++ ") {" ++ strs' ", " e ++ "}]"]
expLines (Map e) = ["(@{" ++ (strs ", " . map(\(k, v) -> show k ++ " : " ++ show v) ) e ++ "})"]
expLines (ObjCConst e) = ["@" ++ show e]
expLines (Lambda pars e rtp) = ["^" ++ show rtp ++ "(" ++ strs ", " (map showPar pars) ++ ") {"] ++ stms e ++ ["}"]
	where showPar(name, tp) = showDecl tp (kw name)
expLines (Not e) = ["!("] `glue` (expLines e `app` ")")
expLines (Negative e) = ["-"] `glue` expLines e
expLines (Cast tp e) =  ["((" ++ show tp ++ ")("] `glue` (expLines e `app` "))")
expLines (ShortCast tp e) =  ["(" ++ show tp ++ ")"] `glue` expLines e
expLines (ProtocolRef e) =  ["@protocol(" ++ show e ++ ")"]
expLines (Error s) = ["<#ERROR: "] `glue` (lines s `app` "#>")
expLines Nop = []
