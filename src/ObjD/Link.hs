module ObjD.Link (
	Sources, File(..), Class(..), Extends, Def(..), Par(..), Constructor, DataType(..), Exp(..), CImport(..), EnumItem(..), DefMod(..),
	link, isClass, isDef, isField, isEnum, isVoid
)where

import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import           Ex.String
import qualified ObjD.Struct         as D


type Sources = [File]
data File = File {fileName :: String, fileImports :: [File], fileCImports :: [CImport], fileClasses :: [Class], globalDefs :: [Def]}

data Class = Class { isStruct :: Bool, className :: String , classExtends :: Extends, classDefs :: [Def], classConstructor :: Constructor }
	| Stub { isStruct :: Bool, className :: String, classExtends :: Extends, classDefs :: [Def]}
	| Enum { className :: String, classExtends :: Extends, classDefs :: [Def], classConstructor :: Constructor, enumItems :: [EnumItem] }
isClass :: Class -> Bool
isClass (Class {}) = True
isClass _ = False
isEnum :: Class -> Bool
isEnum (Enum {}) = True
isEnum _ = False

type Extends = Maybe Class
data Def = Def {defName :: String, defPars :: [Par], defType :: DataType, defBody :: Exp, defMods :: [DefMod]}
	| Field { defName :: String, defType :: DataType, defBody :: Exp, defMods :: [DefMod]}
	| DefStub {defName :: String, defPars :: [Par], defType :: DataType, defMods :: [DefMod]}
isDef :: Def -> Bool
isDef Def{} = True
isDef _ = False
isField :: Def -> Bool
isField Field{} = True
isField _ = False

data DefMod = DefModStatic | DefModMutable | DefModAbstract | DefModPrivate | DefModPrivateWrite deriving (Eq, Ord)

data EnumItem = EnumItem {enumFieldName :: String, enumFieldPars:: [(Def, Exp)]}

data Par = Par {parName :: String, parType :: DataType, parDef :: Exp}
type Constructor = [(Def, Exp)]

data DataType = TPInt | TPFloat | TPString | TPVoid | TPClass Class | TPStruct Class | TPEnum Class | TPArr DataType | TPBool
isVoid :: DataType -> Bool
isVoid TPVoid = True
isVoid _ = False

instance Show DataType where
	show TPInt = "int"
	show TPFloat = "float"
	show TPVoid = "void"
	show TPString = "string"
	show TPBool = "bool"
	show (TPClass c) = className c ++ "*"
	show (TPEnum c) = className c ++ "*"
	show (TPStruct c) = className c
	show (TPArr t) = "[" ++ show t ++ "]"

data Exp = Nop 
	| IntConst Int 
	| BoolConst Bool 
	| FloatConst Int Int
	| Braces [Exp]
	| If Exp Exp Exp
	| Self Class
	| NotEq Exp Exp 
	| Eq Exp Exp
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
	show cl =
		tp cl ++ " " ++ className cl ++ sConstr cl ++ maybe "" className (classExtends cl) ++ " {\n" ++
			(unlines . map ind . concatMap (lines . show)) (classDefs cl)  ++
		"}"
		where
			tp Class{} = "class"
			tp Stub{} = "stub"
			tp Enum{} = "enum"
			sConstr ccc = maybe "" (\cc -> " (" ++ (strs ", " . map constrFld) cc ++ ") ") (constr ccc)
			constr Class{classConstructor = c} = Just c
			constr Enum{classConstructor = c} = Just c
			constr _ = Nothing

			constrFld (f, Nop) = defName f
			constrFld (f, e) = defName f ++ " = " ++ show e
instance Show Def where
	show Def {defName = name , defPars = [], defType = tp, defBody = e} =
		"def " ++ name ++ " : " ++ show tp ++ " = " ++ show e
	show Def {defName = name , defPars = pars, defType = tp, defBody = e} =
		"def " ++ name ++ "(" ++ strs' ", " pars ++ ")"++ " : " ++ show tp ++ " = " ++ show e
	show DefStub {defName = name , defPars = [], defType = tp} =
		"def stub " ++ name ++ " : " ++ show tp 
	show DefStub {defName = name , defPars = pars, defType = tp} =
		"def stub " ++ name ++ "(" ++ strs' ", " pars ++ ")"++ " : " ++ show tp
	show Field {defName = nm, defMods = mods, defType = tp, defBody = e } =
		(if DefModMutable `elem` mods then "var" else "val") ++ " " ++ nm ++ " : " ++ show tp ++ show e

instance Show Par where
	show Par {parName = name, parType = tp, parDef = Nop} = name ++ " : " ++ show tp
	show Par {parName = name, parType = tp, parDef = dd} = name ++ " : " ++ show tp ++ " = " ++ show dd

instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show Nop = ""
	show (Self _) = "self"
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
	show (BoolConst i) = show i
	show (FloatConst a b) = show a ++ "." ++ show b
	
findCall :: String -> [(Maybe String, Exp)] -> [Def] -> Maybe Exp
findCall name pars fdefs = listToMaybe $ (mapMaybe fit . filter (\d -> defName d == name)) fdefs
	where
		fit :: Def -> Maybe Exp
		fit d@Field{}
			| null pars = Just $ Call d []
			| otherwise = Nothing 
		fit d = Just $ Call d $ zipWith (\dp (_, e) -> (dp, e) ) (defPars d) pars
		 {- TODO: Finish it -}
		

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
		isCls s = D.isClass s || D.isStub s || D.isEnum s
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
			DefStub {defName = sn, defPars = linkDefPars cidx pars, defType = dataType cidx tp, defMods = []}

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
envSetSelf :: Class -> Env -> Env
envSetSelf self Env {envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals} = 
	Env{envSelf = self, envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals}

envDeclName :: EnvDecl -> String
envDeclName (EnvDeclPar Par{parName = name}) = name

findTp :: String -> M.Map String a -> String -> a
findTp tp mmm name =  M.findWithDefault (error $ "No " ++ tp ++ " found " ++ name) name mmm

cls :: (ClassIndex, DefIndex) -> D.FileStm -> Class
cls (cidx, glidx) cl = self
	where
		env = Env self cidx glidx M.empty
		self = case cl of
			D.Class{} -> Class {
				isStruct = D.isStruct cl, 
				className = D.className cl, 
				classExtends = extends, 
				classDefs = fields ++ defs, 
				classConstructor = constr
			}
			D.Stub{} -> Stub{
				isStruct =  D.isStruct cl, 
				className = D.className cl, 
				classExtends = extends, 
				classDefs = defs}
			D.Enum{} -> Enum {
				className = D.className cl, 
				classExtends = extends, 
				classDefs = fields ++ defs, 
				classConstructor = constr,
				enumItems = map enumItem (D.enumItems cl)
			}
		extends = fmap (findTp "class" cidx) (D.classExtends cl)
		fields =  mapM (evalState . field) decls env
		fieldsMap = M.fromList $ map (idx defName) fields
		decls = D.classFields cl ++ (filter D.isDecl $ D.classBody cl)
		defs = evalState (mapM def . filter D.isDef $ D.classBody cl) env
		constr = map (\f -> (findTp "field" fieldsMap (D.defName f), evalState (expr False (D.defBody f)) env) ) $ D.classFields cl
		enumItem (D.EnumItem name pars) = EnumItem name enumItemPars
			where
				enumItemPars = zip (map fst constr) (map ((\e -> evalState (expr False e) env) . snd) pars)

field :: D.ClassStm -> State Env Def
field D.Decl {D.defMods = mods, D.defName = name, D.defRetType = tp, D.defBody = e} = do
	i <- expr False e
	env <- get
	return Field {defMods = translateMods mods, defName = name, defType = getDataType env tp i, defBody = i}

translateMods :: [D.DefMod] -> [DefMod]
translateMods = map m
	where 
		m D.DefModStatic = DefModStatic
		m D.DefModMutable = DefModMutable
		m D.DefModPrivate = DefModPrivate
		m D.DefModPrivateWrite = DefModPrivateWrite

def :: D.ClassStm -> State Env Def
def D.Def{D.defMods = mods, D.defName = name, D.defPars = opars, D.defRetType = tp, D.defBody = body} = do
	env <- get
	let 
		 pars = linkDefPars (envIndex env) opars
		 mods' = translateMods mods
		 needReturn (Just (D.DataType "void")) = False
		 needReturn _ = True
		 in 
		(case body of
			D.Nop -> return Def {defMods = DefModAbstract : mods' , defName = name,
					defPars = pars,
					defType = dataType (envIndex env) (fromMaybe (D.DataType "void") tp), defBody = Nop} 
			_   -> do 
				modify $ envAddVals (map EnvDeclPar pars)
				b <- expr (needReturn tp) body
				put env
				return Def {defMods = mods', defName = name,
					defPars = pars,
					defType = getDataType env tp b, defBody = b})

linkDefPars :: ClassIndex -> [D.Par] -> [Par]
linkDefPars cidx = map (\D.Par { D.parName = pnm, D.parType  = ttt } -> Par pnm (dataType cidx ttt) Nop)

getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType (envIndex env)) tp

dataType :: ClassIndex -> D.DataType -> DataType
dataType cidx (D.DataType name) = case name of
	"int" -> TPInt
	"float" -> TPFloat
	"void" -> TPVoid
	"string" -> TPString
	_ -> let c = findTp "class" cidx name
		in case c of 
			Enum{} -> TPEnum c
			_ -> if isStruct c then TPStruct c else TPClass c
dataType cidx (D.DataTypeArr tp) = TPArr $ dataType cidx tp

exprDataType :: Exp -> DataType
exprDataType (If _ t _) = exprDataType t
exprDataType (Braces []) = TPVoid
exprDataType (Braces es) = exprDataType $ last es
exprDataType (Nop) = TPVoid
exprDataType (IntConst _ ) = TPInt
exprDataType (BoolConst _ ) = TPBool
exprDataType (FloatConst _ _) = TPFloat
exprDataType (Eq _ _) = TPBool
exprDataType (NotEq _ _) = TPBool
exprDataType (Dot _ b) = exprDataType b
exprDataType (Set a _) = exprDataType a
exprDataType (Self s) = refDataType s
exprDataType (ParRef Par{parType = tp}) = tp
exprDataType (Call d _) = defType d
exprDataType (Return e) = exprDataType e

refDataType :: Class -> DataType
refDataType e@Enum{} = TPEnum e
refDataType cl 
	| isStruct cl = TPStruct cl
	| otherwise = TPClass cl

dataTypeClass :: DataType -> Class
dataTypeClass (TPClass c) = c
dataTypeClass (TPStruct c) = c
dataTypeClass (TPEnum c) = c

expr :: Bool -> D.Exp -> State Env Exp
expr r (D.If cond t f) = do
	c <- expr False cond
	tt <- expr r t
	ff <- expr r f
	return $ If c tt ff
expr r (D.Braces es) = mapM (expr r) es >>= \v -> return $ Braces v
expr True D.Nop = error "Return NOP"
expr True e = expr False e >>= \ee -> return $ Return ee
expr _ D.Nop = return Nop
expr _ (D.IntConst i) = return $ IntConst i
expr _ (D.BoolConst i) = return $ BoolConst i
expr _ (D.FloatConst a b) = return $ FloatConst a b
expr _ (D.Eq a b) = do
	aa <- expr False a
	bb <- expr False b
	return $ Eq aa bb
expr _ (D.NotEq a b) = do
	aa <- expr False a
	bb <- expr False b
	return $ NotEq aa bb
expr _ (D.Dot a b) = do
	aa <- expr False a
	env <- get
	bb <- exprCall (Just $ (dataTypeClass .exprDataType) aa)  b
	put env
	return $ Dot aa bb
expr _ (D.Set a b) = do
	aa <- expr False a
	bb <- expr False b
	return $ Set aa bb
expr _ D.Self = do
	env <- get
	return $ Self $ envSelf env
expr _ r@(D.Ref _) = exprCall Nothing r
expr _ r@(D.Call _ _) = exprCall Nothing r

exprCall :: Maybe Class -> D.Exp -> State Env Exp
exprCall c (D.Ref n) = do
	env <- get
	put $ maybe env (\ v -> envClearVals $ envSetSelf v env) c
	r <- maybe (expr False $ D.Call n []) (return . toRef) $ M.lookup n (envVals env)
	put env
	return r
	where
		toRef (EnvDeclPar p) = ParRef p
exprCall c (D.Call name pars) = do
	env <- get
	rp <- mapM (\ (n, e) -> expr False e >>= (\ ee -> return (n, ee))) pars
	return $
		let
			allDefs Nothing = allDefsInClass (envSelf env) ++ envGlobalDefIndex env
			allDefs (Just self) = allDefsInClass self
			allDefsInClass cl = classDefs cl ++ maybe [] allDefsInClass (classExtends cl)
			dd = fromMaybe (error $ "Could find reference for call " ++ callStr) $ findCall name rp (allDefs c)
			callStr = name ++ case pars of
				[] -> ""
				_  -> "(" ++ strs ", " (map ((++ ":") . fromMaybe "" . fst) pars) ++ ")"
		in dd

{- expr x = error $ "No expr for " ++ show x -}

