module ObjD.Link (
	Sources, File(..), Class(..), Extends, Def(..), Par(..), Constructor, DataType(..), Exp(..), CImport(..),
	link, isClass, isDef, isField
)where

import           Control.Arrow
import           Control.Monad.State
import           Data.List
import qualified Data.Map            as M
import           Data.Maybe
import           Ex.String
import qualified ObjD.Struct         as D


type Sources = [File]
data File = File {fileName :: String, fileImports :: [File], fileCImports :: [CImport], fileClasses :: [Class], globalDefs :: [Def]}

data Class = Class { isStruct :: Bool, className :: String
	, classExtends :: Extends, classDefs :: [Def], classConstructor :: Constructor }
	| Stub { isStruct :: Bool, className :: String, classExtends :: Extends, classDefs :: [Def]}
isClass :: Class -> Bool
isClass (Class {}) = True
isClass _ = False

type Extends = Maybe Class
data Def = Def {defName :: String, defPars :: [Par], defType :: DataType, defBody :: Exp}
	| Field {defName :: String, isFieldMutable :: Bool, defType :: DataType, defBody :: Exp}
	| DefStub {defName :: String, defPars :: [Par], defType :: DataType}
isDef :: Def -> Bool
isDef Def{} = True
isDef _ = False
isField :: Def -> Bool
isField Field{} = True
isField _ = False

data Par = Par {parName :: String, parType :: DataType, parDef :: Exp}
type Constructor = [(Def, Exp)]

data DataType = TPInt | TPFloat | TPVoid | TPClassRef Class | TPArr DataType
instance Show DataType where
	show TPInt = "int"
	show TPFloat = "float"
	show TPVoid = "void"
	show (TPClassRef c)
		| isStruct c = className c
	 	| otherwise  = className c ++ "*"
	show (TPArr t) = "[" ++ show t ++ "]"

data Exp = Nop | IntConst Int | Braces [Exp]
	| If Exp Exp Exp
	| Self
	| NotEq Exp Exp | Eq Exp Exp
	| Dot Exp Exp
	| ParRef Par
	| Set Exp Exp
	| Call Def [(Par, Exp)]
	| Return Exp

data CImport = CImportLib String | CImportUser String

instance Show File where
	show (File name imps cimps classes gldefs) =
		"// " ++ name ++ ".od\n\n" ++
		strs' "\n" cimps ++ "\n\n" ++
		strs' "\n" imps ++ "\n\n" ++
		strs' "\n" gldefs ++
		strs' "\n\n" classes
instance Show CImport where
	show (CImportLib l) = "import <" ++ l ++ ">"
	show (CImportUser l) = "import \"" ++ l ++ "\""

instance Show Class where
	show Class {className = name, classExtends = extends, classDefs = defs, classConstructor = constr} =
		"class " ++ name ++ " (" ++ (strs ", " . map constrFld) constr ++ ") " ++ maybe "" className extends ++ " {\n" ++
			(unlines . map ind . concatMap (lines . show)) defs  ++
		"}"
		where
			constrFld (f, Nop) = defName f
			constrFld (f, e) = defName f ++ " = " ++ show e
	show Stub {className = name, classExtends = extends, classDefs = defs} = "stub " ++ name  ++ maybe "" className extends ++ " {\n" ++
		(unlines . map ind . concatMap (lines . show)) defs ++ "}"
instance Show Def where
	show Def {defName = name , defPars = [], defType = tp, defBody = e} =
		"def " ++ name ++ " : " ++ show tp ++ " = " ++ show e
	show Def {defName = name , defPars = pars, defType = tp, defBody = e} =
		"def " ++ name ++ "(" ++ strs' ", " pars ++ ")"++ " : " ++ show tp ++ " = " ++ show e
	show DefStub {defName = name , defPars = [], defType = tp} =
		"def stub " ++ name ++ " : " ++ show tp 
	show DefStub {defName = name , defPars = pars, defType = tp} =
		"def stub " ++ name ++ "(" ++ strs' ", " pars ++ ")"++ " : " ++ show tp
	show Field {defName = nm, isFieldMutable = mut, defType = tp, defBody = e } =
		(if mut then "var" else "val") ++ " " ++ nm ++ " : " ++ show tp ++ show e

instance Show Par where
	show Par {parName = name, parType = tp, parDef = Nop} = name ++ " : " ++ show tp
	show Par {parName = name, parType = tp, parDef = dd} = name ++ " : " ++ show tp ++ " = " ++ show dd

instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show Nop = ""
	show Self = "self"
	show (Return e) = "return " ++ show e
	show (NotEq l r) = showOp l "!=" r
	show (Eq l r) = showOp l "==" r
	show (Dot l r) = showOp' l "." r
	show (ParRef s) = show s
	show (Set l r) = showOp l "=" r
	show (Call f []) = defName f
	show (Call dd pars) = defName dd ++ "(" ++ strs' ", " (map showPar pars) ++ ")"
		where
			showPar (Par {parName = name}, e) = name ++ " = " ++ show e
	show (IntConst i) = show i
	
findDef :: String -> [(String, Exp)] -> [Def] -> Maybe Def
findDef name _ fdefs = find fit $ filter (\d -> defName d == name) fdefs
	where
		fit _ = True {- TODO: Finish it -}
		

idx :: (a -> k) -> a -> (k, a)
idx f a = (f a, a)

link :: D.Sources -> Sources
link src = (map snd . M.toList) fidx
	where
		fidx :: M.Map String File
		fidx = M.fromList $ map (idx fileName . file fidx) src

file :: M.Map String File -> D.File -> File
file fidx (D.File name stms) = fl
	where
		fl :: File
		fl = File {fileName = name, fileImports = files, fileCImports = cImports, 
			fileClasses =(map (cls (cidx, glidx)) . filter isCls) stms, globalDefs = gldefs}
		files = visibleFiles stms
		isCls s = D.isClass s || D.isStub s
		cidx = M.fromList $ (map (idx className) . concatMap fileClasses . (fl : ) . visibleFiles) stms
		glidx = concatMap globalDefs (fl : files)
		visibleFiles :: [D.FileStm] -> [File]
		visibleFiles = mapMaybe (getFile . D.impString) . filter D.isImport
		cImports = mapMaybe toCImport stms
		toCImport (D.Import s D.ImportTypeCUser) = Just $ CImportUser s
		toCImport (D.Import s D.ImportTypeCLib) = Just $ CImportLib s
		toCImport _ = Nothing

		getFile f = M.lookup f fidx
		gldefs = (map gldef . filter D.isStubDef) stms
		gldef D.StubDef{D.stubDefName = sn, D.stubDefPars = pars, D.stubDefRetType = tp} = 
			DefStub {defName = sn, defPars = linkDefPars cidx pars, defType = dataType cidx tp}

type ClassIndex = M.Map String Class
type DefIndex = [Def]
data Env = Env{envSelf :: Class, envIndex :: ClassIndex, envGlobalDefIndex :: DefIndex, envVals :: M.Map String EnvDecl}
data EnvDecl = EnvDeclPar Par
envAddVals :: [EnvDecl] -> Env -> Env 
envAddVals decls Env {envSelf = self, envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals} = 
	Env{envSelf = self, envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals `M.union` newVals}
	where
		newVals = M.fromList $ map (\ d -> (envDeclName d, d)) decls
envClearVals :: Env -> Env
envClearVals env
	| M.null (envVals env) = env
	| otherwise = Env{envSelf = envSelf env, envIndex = envIndex env, envGlobalDefIndex = envGlobalDefIndex env, envVals = M.empty}

envDeclName :: EnvDecl -> String
envDeclName (EnvDeclPar Par{parName = name}) = name

findTp :: String -> M.Map String a -> String -> a
findTp tp mmm name =  M.findWithDefault (error $ "No " ++ tp ++ " found " ++ name) name mmm

cls :: (ClassIndex, DefIndex) -> D.FileStm -> Class
cls (cidx, glidx) cl@D.Class{} = self
	where
		env = Env self cidx glidx M.empty
		self = Class {isStruct = False, className = D.className cl, classExtends = extends, classDefs = fields ++ defs, classConstructor = constr}
		extends = fmap (findTp "class" cidx) (D.classExtends cl)
		fields =  mapM (evalState . field) decls env
		fieldsMap = M.fromList $ map (idx defName) fields
		decls = D.classFields cl ++ (map D.stmDecl . filter D.isDecl $ D.classBody cl)
		defs = evalState (mapM def . filter D.isDef $ D.classBody cl) env
		constr = map (\f -> (findTp "field" fieldsMap (D.declName f), evalState (expr (D.declDef f)) env) ) $ D.classFields cl
cls (cidx, _) D.Stub{D.isStruct = str, D.className = name, D.classExtends = extends, D.classBody = ddefs} = 
	Stub{isStruct = str, className = name, classExtends = fmap (findTp "class" cidx) extends, classDefs = defs}
	where
		defs = (map toDef . filter D.isDef) ddefs
		toDef D.Def{D.defName = n, D.defPars = opars, D.defRetType = tp} = 
			Def{defName = n, defPars = map par opars, defBody = Nop, defType = maybe TPVoid (dataType cidx) tp}
		par D.Par {D.parName = n, D.parType = tp} = Par{parName = n, parType = dataType cidx tp, parDef = Nop}

field :: D.Decl -> State Env Def
field D.Decl {D.isDeclMutable = mut, D.declName = name, D.declDataType = tp, D.declDef = e} = do
	i <- expr e
	env <- get
	return Field {defName = name, isFieldMutable = mut, defType = getDataType env tp i, defBody = i}


def :: D.ClassStm -> State Env Def
def D.Def{D.defName = name, D.defPars = opars, D.defRetType = tp, D.defBody = body} = do
	env <- get
	let pars = linkDefPars (envIndex env) opars
		in do 
			modify $ envAddVals (map EnvDeclPar pars)
			b <- expr body
			put env
			return Def {defName = name,
				defPars = pars,
				defType = getDataType env tp b, defBody = b} 

linkDefPars :: ClassIndex -> [D.Par] -> [Par]
linkDefPars cidx = map (\D.Par { D.parName = pnm, D.parType  = ttt } -> Par pnm (dataType cidx ttt) Nop)

getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType (envIndex env)) tp

dataType :: ClassIndex -> D.DataType -> DataType
dataType cidx (D.DataType name) = case name of
	"int" -> TPInt
	"float" -> TPFloat
	_ -> TPClassRef $ findTp "class" cidx name
dataType cidx (D.DataTypeArr tp) = TPArr $ dataType cidx tp

exprDataType :: Exp -> DataType
exprDataType _ = TPInt

expr :: D.Exp -> State Env Exp
expr D.Nop = return Nop
expr (D.IntConst i) = return $ IntConst i
expr (D.Braces es) = mapM expr es >>= \v -> return $ Braces v
expr (D.If cond t f) = do
	c <- expr cond
	tt <- expr t
	ff <- expr f
	return $ If c tt ff
expr (D.Eq a b) = do
	aa <- expr a
	bb <- expr b
	return $ Eq aa bb
expr (D.NotEq a b) = do
	aa <- expr a
	bb <- expr b
	return $ NotEq aa bb
expr (D.Dot a b) = do
	aa <- expr a
	env <- get
	modify envClearVals
	bb <- expr b
	put env
	return $ Dot aa bb
expr (D.Set a b) = do
	aa <- expr a
	bb <- expr b
	return $ Set aa bb
expr D.Self = return Self
expr (D.Ref n) = do
	env <- get
	maybe (expr $ D.Call n []) (return . toRef) $ M.lookup n (envVals env)
	where
		toRef (EnvDeclPar p) = ParRef p
expr (D.Call name pars) = do
	env <- get
	rp <- mapM (\ (n, e) -> expr e >>= (\ ee -> return (n, ee))) pars
	return $
		let
			allDefs = allDefsInClass (envSelf env) ++ envGlobalDefIndex env
			allDefsInClass Class{classDefs = defs, classExtends = Nothing} = defs
			allDefsInClass Class{classDefs = defs, classExtends = Just extends} = defs ++ allDefsInClass extends
			dd = fromMaybe (error $ "Could find reference for call " ++ callStr) $ findDef name rp allDefs
			pp = map (first getPP) rp
			getPP n = fromMaybe (error $ "Could not find parameter " ++ n ++ " in def " ++ show dd) $ find (\ p -> parName p == n) $ defPars dd
			callStr = name ++ case pars of
				[] -> ""
				_  -> "(" ++ strs ", " (map ((++ ":") . fst) pars) ++ ")"
		in Call dd pp

{- expr x = error $ "No expr for " ++ show x -}

