module ObjC.Struct ( Property(..), PropertyModifier(..),FileStm(..), ImplSynthesize(..), ImplFun(..), Fun(..), FunType(..), FunPar(..),
  Stm(..), Exp(..), ImplField(..), CFunPar(..), CFunMod(..), DataType(..), Extends(..), tp, forExp, forStm, forStms, cfun, Visibility(..)
) where

import           Ex.String
import 			 Data.Decimal
import           Control.Monad
import 			 Control.Arrow

data FileStm =
	Import String | ImportLib String | EmptyLine 
	| Interface { interfaceName :: String, interfaceExtends :: Extends, interfaceProperties :: [Property], interfaceFuns :: [Fun], interfaceFields :: InterfaceFields}
	| Protocol { interfaceName :: String, interfaceExtends :: Extends, interfaceProperties :: [Property], interfaceFuns :: [Fun] }
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
data Visibility = Public | Private | Protected | Package
type InterfaceFields = [(Visibility, [ImplField])]
cfun :: FileStm -> [Stm] -> FileStm
cfun decl s = CFun{cfunMods = cfunMods decl, cfunReturnType = cfunReturnType decl, cfunName = cfunName decl, cfunPars = cfunPars decl, cfunExps = s}
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
tp :: String -> DataType
tp name = TPSimple name []

data Stm =
	If Exp [Stm] [Stm]
	| While Exp [Stm]
	| Do Exp [Stm]
	| Set (Maybe MathTp) Exp Exp
	| Stm Exp
	| Return Exp
	| Throw Exp
	| Braces [Stm]
	| Var{varType :: DataType, varName :: String, varExp :: Exp, varMods :: [String]}
	| Break | Continue
	| Synchronized Exp [Stm]
	| Try [Stm] [Stm]
	| ForIn DataType String Exp [Stm]

forStms :: MonadPlus m => (Stm -> Bool, Stm -> m a, Exp -> Bool, Exp -> m a) -> [Stm] -> m a 	
forStms f s = msum $ map (forStm f) s 

forStm :: MonadPlus m => (Stm -> Bool, Stm -> m a, Exp -> Bool, Exp -> m a) -> Stm -> m a
forStm f@(cs, fs, _, _) ee = mplus (fs ee) $ if cs ee then (go ee) else mzero
	where
		mmsum = msum . map (forStm f)
		mfore = forExp f 
		go (If e l r) = mfore e `mplus` mmsum l `mplus` mmsum r
		go (Braces l) = mmsum l
		go (While e l) = mfore e `mplus` mmsum l
		go (Do e l) = mfore e `mplus` mmsum l
		go (Synchronized e l) = mfore e `mplus` mmsum l
		go (Try e l) = mmsum e `mplus` mmsum l
		go (Set _ l r) = mfore l `mplus` mfore r
		go (Return l) = mfore l
		go (Throw l) = mfore l
		go (Stm l) = mfore l
		go (Var _ _ l _) = mfore l
		go (ForIn _ _ e s) = mfore e `mplus` mmsum s
		go _ = mzero
		
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
	| Arrow Exp Exp
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
	| EArrConst String String [Exp]
	| ProtocolRef Exp
	| GetRef Exp
	| RefUp Exp
	| ExpBraces [Stm]
forExp :: MonadPlus m => (Stm -> Bool, Stm -> m a, Exp -> Bool, Exp -> m a) -> Exp -> m a
forExp f@(_, _, ce, fe) ee = mplus (fe ee) $ if ce ee then (go ee) else mzero
	where	
		mmsum = msum . map (forExp f)
		mssum = msum . map (forStm f)
		mfor = forExp f 
		go (Call inst _ pars vargs) = msum (map (mfor. snd) pars) `mplus` mfor inst `mplus` mmsum vargs
		go (CCall inst exps) = mfor inst `mplus` mmsum exps
		go (Arr exps) = mmsum exps
		go (ExpBraces exps) = mssum exps
		go (EArr exps) = mmsum exps
		go (EArrConst _ _ exps) = mmsum exps
		go (Map exps) = msum $ map (forExp f *** forExp f >>> uncurry mplus) exps
		go (ObjCConst e) = mfor e
		go (PlusPlus e) = mfor e
		go (MinusMinus e) = mfor e
		go (Not e) = mfor e
		go (Negative e) = mfor e
		go (ProtocolRef e) = mfor e
		go (GetRef e) = mfor e
		go (RefUp e) = mfor e
		go (Cast _ e) = mfor e
		go (ShortCast _ e) = mfor e
		go (BoolOp _ l r) = mfor l `mplus` mfor r
		go (MathOp _ l r) = mfor l `mplus` mfor r
		go (Dot l r) = mfor l `mplus` mfor r
		go (Arrow l r) = mfor l `mplus` mfor r
		go (Index l r) = mfor l `mplus` mfor r
		go (InlineIf e l r) = mfor e `mplus` mfor l `mplus` mfor r
		go (Lambda _ s _) = msum $ map (forStm f) s
		go _ = mzero
	
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
kw "check" = "aCheck"
kw s = s

instance Show DataType where
	show (TPSimple s []) = s
	show (TPArr 0 s) = s ++ "[]"
	show (TPArr n s) = s ++ "[" ++ show n ++ "]"
	show (TPSimple s pr) = s ++ "<" ++ strs ", " pr ++ ">"
	show (TPBlock TPBlock{} t) = "id(^)" ++ "(" ++ strs' ", " t ++ ")"
	show (TPBlock d t) = show d ++ "(^)" ++ "(" ++ strs' ", " t ++ ")"

showDecl ::  DataType -> String ->String
showDecl (TPArr 0 tpp) name = tpp ++ " " ++ name ++ "[]"
showDecl (TPArr n tpp) name = tpp ++ " " ++ name ++ "[" ++ show n ++ "]"
showDecl tpp@TPSimple{} name = show tpp ++ " " ++ name
showDecl (TPBlock TPBlock{} t) name = "id(^" ++ name ++ ")" ++ "(" ++ strs' ", " t ++ ")"
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
	show (Interface name extends properties funs intFields) =
		"@interface " ++ name ++ " : " ++ show extends ++ 
		showFields (filter (not . null . snd) intFields)
		 ++ (unlines' . map show) properties
		 ++ (unlines  . map (( ++ ";") . show)) funs
		 ++ "@end\n\n"
		 where
		 	showFields [] = "\n"
		 	showFields fields = " {\n" ++ unlines (map showVisibilitySection fields) ++ "}\n"
		 	showVisibilitySection (visibility, fields) = show visibility ++ "\n" ++ mkString (ind . show) "\n" fields 
	show (Protocol name (Extends cl trs) properties funs) =
		"@protocol " ++ name ++ "<" ++ cl ++ unwords (map (", " ++ ) trs) ++ ">\n"
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
			++ (unlines . map (ind . show)) a
			++ "}\n"
		
		showSynthenizes = unlines . map showSynthenize
		showStFields = unlines . map showStField
		showSynthenize (ImplSynthesize name "") = "@synthesize " ++ kw name ++ ";"
		showSynthenize (ImplSynthesize name var) = "@synthesize " ++ kw name ++ " = " ++ kw var ++ ";"
		showImplFuns = unlines . map show
		showStField (ImplField nm tpp mods Nop) = "static " ++  (strs " " mods) `tryCon` " " ++ showDecl tpp nm ++  ";"
		showStField (ImplField nm tpp mods e) = strs "\n" $ ["static " ++  (strs " " mods) `tryCon` " " ++ showDecl tpp nm ++ " = "] `glue` (expLines e `appp` ";")
	show (ClassDecl name) = "@class " ++ name ++ ";"
	show (ProtocolDecl name) = "@protocol " ++ name ++ ";"
instance Show Extends where
	show (Extends cl []) = cl
	show (Extends cl a) = cl ++ "<" ++ strs ", " a ++ ">"
instance Show ImplField where
	show(ImplField name tpp mods _) = (strs " " mods) `tryCon` " " ++ showDecl tpp (kw name) ++ ";"
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
	show (Fun tpp ret name pars) =
		show tpp ++ " (" ++ show ret ++ ")" ++ kw name ++ cap  (strs' " " pars)
instance Show FunPar where
 	show (FunPar name tpp var) = kw name ++ ":(" ++ show tpp ++ ")" ++ kw var
instance Show FunType where
	show InstanceFun = "-"
	show ObjectFun = "+"


instance Show Property where
	show (Property name tpp mods) = "@property (" ++ strs' ", " mods ++ ") " ++ showDecl tpp (kw name) ++ ";"

instance Show Visibility where
	show Public = "@public"
	show Private = "@private"
	show Protected = "@protected"
	show Package = "@package"

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
appp :: [String] -> String -> [String]
a `appp` b = appendLast b a
glue :: [String] -> [String] -> [String]
[] `glue` [] = []
[] `glue` b = b
a `glue` [] = a
a `glue` b = init a ++ [last a ++ head b] ++ tail b
glueAll :: String -> [[String]] -> [String]
glueAll _ [] = []
glueAll _ [x] = x
glueAll s (a:b:xs) = glueAll s $ ((a `appp` s) `glue` b):xs

multiLineIf :: Stm -> [String]
multiLineIf (If cond t []) = ["if(" ++ show cond ++ ") {" ] ++ stms t ++ ["}"]
multiLineIf (If cond t f) = ["if(" ++ show cond ++ ") {" ] ++ stms t ++ ["} else {"] ++ stms f ++ ["}"]

stmLines :: Stm -> [String]
stmLines i@(If _ [If {}] _) = multiLineIf i
stmLines i@(If _ _ [If{}]) = multiLineIf i
stmLines (If cond [t] []) = ["if(" ++ show cond ++ ") " ] `glue` stmLines t
stmLines (If cond t []) = ["if(" ++ show cond ++ ") {"] ++ stms t ++ ["}"]
stmLines (Braces []) = []
stmLines (Braces [t]) = stmLines t
stmLines (Braces t) = ["{"] ++ stms t ++ ["}"]
stmLines (While cond t) = ["while(" ++ show cond ++ ") {"] ++ stms t ++ ["}"]
stmLines (ForIn t name e s) = ["for(" ++ showDecl t name ++ " in " ++ show e ++ ") {"] ++ stms s ++ ["}"]
stmLines (Do cond t) = ["do {"] ++ stms t ++ ["} while(" ++ show cond ++ ");"]
stmLines (If cond [t] [f]) = (["if(" ++ show cond ++ ") " ] `glue` stmLines t) ++ (["else "] `glue` stmLines f)
stmLines i@If{} = multiLineIf i

stmLines (Set Nothing l r) = (expLines l `appp` " = ") `glue` (expLines r `appp` ";")
stmLines (Set (Just tpp) l r) = (expLines l `appp` (" " ++ show tpp ++ "= ")) `glue` (expLines r `appp` ";")
stmLines (Stm Nop) = [""]
stmLines (Stm e) = appendLast ";" $ expLines e
stmLines (Return e) = ["return "] `glue` (expLines e `appp` ";")
stmLines (Throw e) = ["@throw "] `glue` (expLines e `appp` ";")
stmLines (Var tpp name Nop mods) = [(unwords . map (++ " ")) mods ++ showDecl tpp name ++ ";"]
stmLines (Var tpp name e mods) = [(unwords . map (++ " ")) mods ++ showDecl tpp name ++ " = "] `glue` (expLines e `appp` ";")
stmLines (Break) = ["break;"]
stmLines (Continue) = ["continue;"]
stmLines (Synchronized r s) = ["@synchronized(" ++ show r ++ ") {"] ++ stms s ++ ["}"]
stmLines (Try e f) = ["@try {"] ++ stms e ++ ["} @finally {"] ++ stms f ++ ["}"]

expLines :: Exp -> [String]
expLines (ExpBraces []) = []
expLines (ExpBraces [t]) = stmLines t
expLines (ExpBraces t) = ["({"] ++ stms t ++ ["})"]
expLines Self = ["self"]
expLines Super = ["super"]
expLines (Call inst name pars vargs) = ["["] `glue` (expLines inst `appp` (" " ++ kw name)) `glue` pars' `glue` (vargs'  `appp` "]")
	where 
		pars' = (mapFirst cap . glueAll " " . map (\(nm, e) -> [kw nm ++ ":"] `glue` expLines e)) pars
		vargs' = (glueAll "" . map varg') vargs
		varg' = ([", "] `glue` ) . expLines
expLines (CCall name pars) = (expLines name `appp` "(") `glue` (pars' `appp` ")")
	where 
		pars' = (glueAll ", " . map par) pars
		par ee = 
			let ls = expLines ee
			in if any (any (== ',')) ls then (["("] `glue` (ls `appp` ")")) else ls
expLines (Ref name) = [kw name]
expLines (IntConst i) = [show i]
expLines Nil = ["nil"]
expLines (BoolConst True) = ["YES"]
expLines (BoolConst False) = ["NO"]
expLines (FloatConst i) = [show i]
expLines (StringConst s) = case lines2 s of
		[] -> ["@\"\""]
		[x] -> ['@' : '"' : escape x ++ "\""]
		x:xs -> ('@' : '"' : escape x ++ "\\n\"") : map (\line -> ind $ '"' : escape line ++ "\\n\"" ) (init xs) ++ [ind $ '"' : escape (last xs) ++ "\"" | not $ null $ last xs]
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
		needb Div Mul = True
		needb Mul Plus = True
		needb Mul Minus = True
		needb Minus Minus = True
		needb _ _ = False
expLines (Dot l r) = (expLines l `appp` ".") `glue` expLines r
expLines (Arrow l r) = (expLines l `appp` "->") `glue` expLines r
expLines (PlusPlus e) = appendLast "++" (expLines e)
expLines (MinusMinus e) = appendLast "--" (expLines e)
expLines (InlineIf c t f) = ["(("] `glue` (expLines c `appp` ") ? ") `glue` (expLines t `appp` " : ") `glue` (expLines f `appp` ")") 
expLines (Index e i) = (expLines e `appp` "[") `glue` (expLines i `appp` "]")
expLines (Arr e) = ["(@[" ++ strs' ", " e ++ "])"]
expLines (EArr e) = ["{" ++ strs' ", " e ++ "}"]
expLines (EArrConst name tpp e) = ["[ " ++ name ++ "(" ++ (if null tpp then "" else tpp ++ ", ") ++ show (length e) ++ ") {" ++ strs' ", " e ++ "}]"]
expLines (Map e) = ["(@{" ++ (strs ", " . map(\(k, v) -> show k ++ " : " ++ show v) ) e ++ "})"]
expLines (ObjCConst e) = ["@" ++ show e]
expLines (Lambda pars e rtp) = ["^" ++ showRtp ++ "(" ++ strs ", " (map showPar pars) ++ ") {"] ++ stms e ++ ["}"]
	where 
		showPar(name, tpp) = showDecl tpp (kw name)
		showRtp = case rtp of
			TPBlock{} -> "id"
			_ -> show rtp
expLines (Not e) = ["!("] `glue` (expLines e `appp` ")")
expLines (Negative e) = ["-"] `glue` expLines e
expLines (Cast tpp e) =  ["((" ++ show tpp ++ ")("] `glue` (expLines e `appp` "))")
expLines (ShortCast tpp e) =  ["(" ++ show tpp ++ ")"] `glue` expLines e
expLines (ProtocolRef e) =  ["@protocol(" ++ show e ++ ")"]
expLines (GetRef e) =  ["&(" ++ show e ++ ")"]
expLines (RefUp e) =  ["*(" ++ show e ++ ")"]
expLines (Error s) = ["<#ERROR: "] `glue` (lines s `appp` "#>")
expLines Nop = []


escape :: String -> String
escape "" = ""
escape ('\t' : xs) = "\\t" ++ escape xs
escape ('\\' : xs) = "\\\\" ++ escape xs
escape ('\"' : xs) = "\\\"" ++ escape xs
escape (x : xs) = x : escape xs