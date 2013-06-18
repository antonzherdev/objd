module ObjD.Link (
	Sources, File(..), Class(..), Extends, Def(..), Par(..), Constructor, DataType(..), Exp(..),
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
data File = File {fileName :: String, fileImports :: [File], fileClasses :: [Class]}

data Class = Class { className :: String
	, classExtends :: Extends, classDefs :: [Def], classConstructor :: Constructor }
	| Stub { className :: String, classExtends :: Extends, classDefs :: [Def]}
isClass :: Class -> Bool
isClass (Class {}) = True
isClass _ = False

type Extends = Maybe Class
data Def = Def {defName :: String, defPars :: [Par], defType :: DataType, defBody :: Exp}
	|  Field {defName :: String, isFieldMutable :: Bool, defType :: DataType, defBody :: Exp}
isDef :: Def -> Bool
isDef Def{} = True
isDef _ = False
isField :: Def -> Bool
isField Field{} = True
isField _ = False

data Par = Par {parName :: String, parType :: DataType, parDef :: Exp}
type Constructor = [(Def, Exp)]

data DataType = TPInt | TPVoid | TPClassRef Class
instance Show DataType where
	show TPInt = "int"
	show TPVoid = "void"
	show (TPClassRef c) = className c ++ "*"

data Exp = Nop | IntConst Int | Braces [Exp]
	| If Exp Exp Exp
	| Self
	| NotEq Exp Exp | Eq Exp Exp
	| Dot Exp Exp
	| ParRef Par
	| Set Exp Exp
	| Call Def [(Par, Exp)]

instance Show File where
	show (File name imps classes) =
		"// " ++ name ++ ".id\n\n" ++
		strs' "\n" imps ++ "\n\n" ++
		strs' "\n\n" classes

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
		name ++ " : " ++ show tp ++ " = " ++ show e
	show Def {defName = name , defPars = pars, defType = tp, defBody = e} =
		name ++ "(" ++ strs' ", " pars ++ ")"++ " : " ++ show tp ++ " = " ++ show e
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
	show (NotEq l r) = showOp l "!=" r
	show (Eq l r) = showOp l "==" r
	show (Dot l r) = showOp' l "." r
	show (ParRef s) = show s
	show (Set l r) = showOp l "=" r
	show (Call f []) = defName f
	show (Call (Def {defName = n}) pars) = n ++ "(" ++ strs' ", " (map showPar pars) ++ ")"
		where
			showPar (Par {parName = name}, e) = name ++ " = " ++ show e
	show (IntConst i) = show i
	
getDef :: String -> [(String, Exp)] -> Class -> Def
getDef name pars c = fromMaybe (fromMaybe (error $ "No def for call " ++ callStr ++ " in class " ++ className c) findInParent) $
	find fit $ filter (\d -> defName d == name) (classDefs c) 
	where
		fit _ = True {- TODO: Finish it -}
		callStr = name ++ case pars of
			[] -> ""
			_  -> "(" ++ strs ", " (map ((++ ":") . fst) pars) ++ ")"
		findInParent = fmap (getDef name pars) (classExtends c)


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
		fl = File name (visibleFiles stms) $ (map (cls cidx) . filter (not . D.isImport)) stms
		cidx = M.fromList $ (map (idx className) . concatMap fileClasses . (fl : ) . visibleFiles) stms
		visibleFiles :: [D.FileStm] -> [File]
		visibleFiles = mapMaybe (getFile . D.impString) . filter D.isImport
		getFile f = M.lookup f fidx

type ClassIndex = M.Map String Class
data Env = Env{envSelf :: Class, envIndex :: ClassIndex, envVals :: M.Map String EnvDecl}
data EnvDecl = EnvDeclPar Par
envAddVals :: [EnvDecl] -> Env -> Env 
envAddVals decls Env {envSelf = self, envIndex = cidx, envVals = vals} = 
	Env{envSelf = self, envIndex = cidx, envVals = vals `M.union` newVals}
	where
		newVals = M.fromList $ map (\ d -> (envDeclName d, d)) decls
envClearVals :: Env -> Env
envClearVals env
	| M.null (envVals env) = env
	| otherwise = Env{envSelf = envSelf env, envIndex = envIndex env, envVals = M.empty}

envDeclName :: EnvDecl -> String
envDeclName (EnvDeclPar Par{parName = name}) = name

findTp :: String -> M.Map String a -> String -> a
findTp tp mmm name =  M.findWithDefault (error $ "No " ++ tp ++ " found " ++ name) name mmm

cls :: ClassIndex -> D.FileStm -> Class
cls cidx cl@D.Class{} = self
	where
		env = Env self cidx M.empty
		self = Class {className = D.className cl, classExtends = extends, classDefs = fields ++ defs, classConstructor = constr}
		extends = fmap (findTp "class" cidx) (D.classExtends cl)
		fields =  mapM (evalState . field) decls env
		fieldsMap = M.fromList $ map (idx defName) fields
		decls = D.classFields cl ++ (map D.stmDecl . filter D.isDecl $ D.classBody cl)
		defs = evalState (mapM def . filter D.isDef $ D.classBody cl) env
		constr = map (\f -> (findTp "field" fieldsMap (D.declName f), evalState (expr (D.declDef f)) env) ) $ D.classFields cl
cls cidx D.Stub{D.className = name, D.classExtends = extends, D.classBody = ddefs} = 
	Stub{className = name, classExtends = fmap (findTp "class" cidx) extends, classDefs = defs}
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
	let pars = map (\D.Par { D.parName = pnm, D.parType  = ttt } -> Par pnm (dataType (envIndex env) ttt) Nop) opars
		in do 
			modify $ envAddVals (map EnvDeclPar pars)
			b <- expr body
			put env
			return Def {defName = name,
				defPars = pars,
				defType = getDataType env tp b, defBody = b} 

getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType (envIndex env)) tp

dataType :: ClassIndex -> D.DataType -> DataType
dataType cidx (D.DataType name) = case name of
	"int" -> TPInt
	_ -> TPClassRef $ findTp "class" cidx name

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
			dd = getDef name rp (envSelf env)
			pp = map (first getPP) rp
			getPP n = fromMaybe (error $ "Could not find parameter " ++ n ++ " in def " ++ show dd) $ find (\ p -> parName p == n) $ defPars dd
		in Call dd pp

{- expr x = error $ "No expr for " ++ show x -}

