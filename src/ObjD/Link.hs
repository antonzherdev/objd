module ObjD.Link (
	Sources, File(..), Class(..), Extends, Def(..), Constructor, DataType(..), Exp(..), CImport(..), EnumItem(..), 
	DefMod(..), FieldAcc(..), FieldAccMod(..), MathTp(..),
	link, isClass, isDef, isField, isEnum, isVoid, isStub, isStruct, isRealClass, isTrait
)where

import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import           Ex.String
import qualified ObjD.Struct         as D


type Sources = [File]
data File = File {fileName :: String, fileImports :: [File], fileCImports :: [CImport], fileClasses :: [Class], globalDefs :: [Def]}

data Class = Class { classMods :: [ClassMod], className :: String , classExtends :: Extends, classDefs :: [Def], classConstructor :: Constructor
		,classGenetrics :: [Class] }
	| Enum { className :: String, classExtends :: Extends, classDefs :: [Def], classConstructor :: Constructor, enumItems :: [EnumItem]
		,classGenetrics :: [Class] }
	| Generic {className :: String}
isClass :: Class -> Bool
isClass (Class {}) = True
isClass _ = False
isGeneric :: Class -> Bool
isGeneric (Generic {}) = True
isGeneric _ = False
isStruct :: Class -> Bool
isStruct (Class {classMods = mods}) = ClassModStruct `elem` mods
isStruct _ = False
isTrait :: Class -> Bool
isTrait (Class {classMods = mods}) = ClassModTrait `elem` mods
isTrait _ = False
isStub :: Class -> Bool
isStub (Class {classMods = mods}) = ClassModStub `elem` mods
isStub _ = False
isRealClass :: Class -> Bool
isRealClass (Class {classMods = mods}) = ClassModStub `notElem` mods
isRealClass _ = False

isEnum :: Class -> Bool
isEnum (Enum {}) = True
isEnum _ = False

data ClassMod = ClassModStub | ClassModStruct | ClassModTrait deriving (Eq)

type Extends = Maybe Class
data Def = Def {defName :: String, defPars :: [Def], defType :: DataType, defBody :: Exp, defMods :: [DefMod]}
	| Field { defName :: String, defType :: DataType, defBody :: Exp, defMods :: [DefMod], fieldAccs :: [FieldAcc]}
	| DefStub {defName :: String, defPars :: [Def], defType :: DataType, defMods :: [DefMod]}
	| Local {defName :: String, defType :: DataType, defBody :: Exp, defMods :: [DefMod]}
isDef :: Def -> Bool
isDef Def{} = True
isDef _ = False
isField :: Def -> Bool
isField Field{} = True
isField _ = False

data DefMod = DefModStatic | DefModMutable | DefModAbstract | DefModPrivate | DefModConstructor | DefModStructConstructor deriving (Eq, Ord)

data FieldAcc = FieldAccRead [FieldAccMod] Exp | FieldAccWrite [FieldAccMod] Exp
data FieldAccMod = FieldAccModPrivate deriving (Eq)

data EnumItem = EnumItem {enumFieldName :: String, enumFieldPars:: [(Def, Exp)]}

type Constructor = [(Def, Exp)]

data DataType = TPInt | TPFloat | TPString | TPVoid | TPClass Class | TPStruct Class | TPEnum Class | TPTrait Class 
	| TPGeneric Class | TPArr DataType | TPBool | TPFun DataType DataType | TPTuple [DataType]
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
	show (TPTrait c) = className c ++ "*"
	show (TPEnum c) = className c ++ "*"
	show (TPGeneric c) = className c ++ "*"
	show (TPStruct c) = className c
	show (TPArr t) = "[" ++ show t ++ "]"
	show (TPFun s d) = show s ++ " -> " ++ show d
	show (TPTuple tps) = "(" ++ strs' ", " tps ++ ")"

data Exp = Nop 
	| IntConst Int 
	| BoolConst Bool 
	| FloatConst Int Int
	| Braces [Exp]
	| If Exp Exp Exp
	| Self Class
	| Nil
	| BoolOp BoolTp Exp Exp
	| MathOp MathTp Exp Exp
	| PlusPlus Exp
	| MinusMinus Exp
	| Dot Exp Exp
	| Set (Maybe MathTp) Exp Exp
	| Call Def [(Def, Exp)]
	| Return Exp

data CImport = CImportLib String | CImportUser String

instance Show File where
	show (File name imps cimps classes gldefs) =
		"// " ++ name ++ ".od\n" ++
		(sapp "\n\n" . strs' "\n") cimps ++
		(sapp "\n\n" . strs' "\n") imps ++ 
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
			tp c@Class{}
				| isStub c = "stub"
				| otherwise = "class"
			tp Enum{} = "enum"
			sConstr ccc = maybe "" (\cc -> " (" ++ (strs ", " . map constrFld) cc ++ ") ") (Just $ classConstructor ccc)
			constrFld (f, Nop) = defName f
			constrFld (f, e) = defName f ++ " = " ++ show e
instance Show Def where
	show d = showDef True d

showDef :: Bool -> Def -> String
showDef f Def {defName = name , defPars = [], defType = tp, defBody = e} =
		"def " ++ name ++ if f then (" : " ++ show tp ++ " = " ++ show e) else ""
showDef f Def {defName = name , defPars = pars, defType = tp, defBody = e} =
	"def " ++ name ++ "(" ++ strs' ", " pars ++ ")" ++ if f then  (" : " ++ show tp ++ " = " ++ show e) else ""
showDef _ DefStub {defName = name , defPars = [], defType = tp} =
	"def stub " ++ name ++ " : " ++ show tp 
showDef _ DefStub {defName = name , defPars = pars, defType = tp} =
	"def stub " ++ name ++ "(" ++ strs' ", " pars ++ ")"++ " : " ++ show tp
showDef f Field {defName = nm, defMods = mods, defType = tp, defBody = e } =
	(if DefModMutable `elem` mods then "var" else "val") ++ " " ++ nm ++ if f then (" : " ++ show tp ++ show e) else ""

instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show Nop = ""
	show (Self c) = "<" ++ className c ++ ">self"
	show (Return e) = "return " ++ show e
	show (Set Nothing l r) = showOp l "=" r
	show (Set (Just t) l r) = showOp l (show t ++ "=") r
	show (BoolOp t l r) = showOp l (show t) r
	show (MathOp t l r) = showOp l (show t) r
	show (PlusPlus e) = show e ++ "++"
	show (MinusMinus e) = show e ++ "--"
	show (Dot l r) = showOp' l "." r
	show (Call f []) = defRefPrep f ++ defName f
	show (Call dd pars) = defRefPrep dd ++ defName dd ++ "(" ++ strs' ", " (map showPar pars) ++ ")"
		where
			showPar (Local {defName = name}, e) = name ++ " = " ++ show e
	show (IntConst i) = show i
	show Nil = "nil"
	show (BoolConst i) = show i
	show (FloatConst a b) = show a ++ "." ++ show b

defRefPrep :: Def -> String
defRefPrep Field{} = "<F>"
defRefPrep Def{} = "<D>"
defRefPrep DefStub{} = "<S>"
defRefPrep Local{} = "<L>"
	
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
link src = map (\D.File{D.fileName = name} ->  fromMaybe (error $ "Could not find linked file " ++ name) $ M.lookup name fidx) src
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
data Env = Env{envSelf :: Class, envIndex :: ClassIndex, envGlobalDefIndex :: DefIndex, envVals :: M.Map String Def}
envAddVals :: [Def] -> Env -> Env 
envAddVals decls Env {envSelf = self, envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals} = 
	Env{envSelf = self, envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals `M.union` newVals}
	where
		newVals = M.fromList $ map (\ d -> (defName d, d)) decls

findTp :: String -> M.Map String a -> String -> a
findTp tp mmm name =  M.findWithDefault (error $ "No " ++ tp ++ " found " ++ name) name mmm

cls :: (ClassIndex, DefIndex) -> D.FileStm -> Class
cls (ocidx, glidx) cl = self
	where
		cidx = ocidx `M.union` M.fromList (map (\g -> (className g, g)) generics)
		env = Env self cidx glidx M.empty
		self = case cl of
			D.Class{} -> Class {
				classMods = map clsMod (D.classMods cl), 
				className = D.className cl, 
				classExtends = extends, 
				classDefs = fields ++ defs, 
				classConstructor = constr,
				classGenetrics = generics
			}
			D.Enum{} -> Enum {
				className = D.className cl, 
				classExtends = extends, 
				classDefs = fields ++ defs, 
				classConstructor = constr,
				enumItems = map enumItem (D.enumItems cl), 
				classGenetrics = generics
			}
		clsMod D.ClassModStruct = ClassModStruct
		clsMod D.ClassModStub = ClassModStub
		clsMod D.ClassModTrait = ClassModTrait
		extends = fmap (findTp "class" cidx) (D.classExtends cl)
		fields =  mapM (evalState . field) decls env
		fieldsMap = M.fromList $ map (idx defName) fields
		decls = D.classFields cl ++ filter D.isDecl (D.classBody cl)
		defs = map (def env) . filter D.isDef $ D.classBody cl
		constr = map (\f -> (findTp "field" fieldsMap (D.defName f), evalState (expr False (D.defBody f)) env) ) $ D.classFields cl
		generics = map generic (D.classGenerics cl)
		generic (D.ClassGeneric name) = Generic name
		enumItem (D.EnumItem name pars) = EnumItem name enumItemPars
			where
				enumItemPars = zip (map fst constr) (map ((\e -> evalState (expr False e) env) . snd) pars)

field :: D.ClassStm -> State Env Def
field D.Decl {D.defMods = mods, D.defName = name, D.defRetType = tp, D.defBody = e, D.declAccs = accs} = do
	i <- expr False e
	env <- get
	let 
		tp' = getDataType env tp i
		acc' (D.DeclAccRead accMods ex) = liftM (FieldAccRead (accMods' accMods)) (expr True ex)
		acc' (D.DeclAccWrite accMods ex) = do
			env' <- get
			modify $ envAddVals [Local name tp' Nop []]
			v <- expr False ex 
			put env'
			return $ FieldAccWrite (accMods' accMods) v
		accMods' = map accMod'
		accMod' D.DeclAccModPrivate = FieldAccModPrivate 
		in do
			accs' <- mapM acc' accs
			return Field {defMods = translateMods mods, defName = name, defType = tp', defBody = i, fieldAccs = accs'}

		

translateMods :: [D.DefMod] -> [DefMod]
translateMods = map m
	where 
		m D.DefModStatic = DefModStatic
		m D.DefModMutable = DefModMutable
		m D.DefModPrivate = DefModPrivate
		
def :: Env -> D.ClassStm -> Def
def env ccc = evalState (stateDef ccc) env
	where 
		stateDef:: D.ClassStm -> State Env Def
		stateDef D.Def{D.defMods = mods, D.defName = name, D.defPars = opars, D.defRetType = tp, D.defBody = body} = 
			let 
				 pars = linkDefPars (envIndex env) opars
				 mods' = translateMods mods
				 needReturn (Just (D.DataType "void" _)) = False
				 needReturn _ = True
				 in 
				(case body of
					D.Nop -> return Def {defMods = DefModAbstract : mods' , defName = name,
							defPars = pars,
							defType = dataType (envIndex env) (fromMaybe (D.DataType "void" []) tp), defBody = Nop} 
					_   -> do 
						modify $ envAddVals pars
						b <- expr (needReturn tp) body
						put env
						return Def {defMods = mods', defName = name,
							defPars = pars,
							defType = getDataType env tp b, defBody = b})

linkDefPars :: ClassIndex -> [D.Par] -> [Def]
linkDefPars cidx = map (\D.Par { D.parName = pnm, D.parType  = ttt } -> Local pnm (dataType cidx ttt) Nop [])

getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType (envIndex env)) tp

dataType :: ClassIndex -> D.DataType -> DataType
dataType cidx (D.DataType name _) = case name of
	"int" -> TPInt
	"float" -> TPFloat
	"void" -> TPVoid
	"string" -> TPString
	"bool" -> TPBool
	_ -> refDataType $ findTp "class" cidx name
dataType cidx (D.DataTypeArr tp) = TPArr $ dataType cidx tp
dataType cidx (D.DataTypeFun s d) = TPFun (dataType cidx s) (dataType cidx d)
dataType cidx (D.DataTypeTuple tps) = TPTuple $ map (dataType cidx) tps

exprDataType :: Exp -> DataType
exprDataType (If _ t _) = exprDataType t
exprDataType (Braces []) = TPVoid
exprDataType (Braces es) = exprDataType $ last es
exprDataType (Nop) = TPVoid
exprDataType (IntConst _ ) = TPInt
exprDataType Nil = TPVoid
exprDataType (BoolConst _ ) = TPBool
exprDataType (FloatConst _ _) = TPFloat
exprDataType (BoolOp _ _ _) = TPBool
exprDataType (MathOp _ l _) = exprDataType l
exprDataType (PlusPlus e) = exprDataType e
exprDataType (MinusMinus e) = exprDataType e
exprDataType (Dot _ b) = exprDataType b
exprDataType (Set _ a _) = exprDataType a
exprDataType (Self s) = refDataType s
exprDataType (Call d _) = defType d
exprDataType (Return e) = exprDataType e

refDataType :: Class -> DataType
refDataType e@Enum{} = TPEnum e
refDataType cl 
	| isStruct cl = TPStruct cl
	| isTrait cl = TPTrait cl
	| isGeneric cl = TPGeneric cl
	| otherwise = TPClass cl

dataTypeClass :: DataType -> Class
dataTypeClass (TPClass c) = c
dataTypeClass (TPStruct c) = c
dataTypeClass (TPEnum c) = c
dataTypeClass (TPTrait c) = c
dataTypeClass (TPGeneric c) = c

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
expr _ D.Nil = return Nil
expr _ (D.BoolConst i) = return $ BoolConst i
expr _ (D.FloatConst a b) = return $ FloatConst a b
expr _ (D.BoolOp tp a b) = do
	aa <- expr False a
	bb <- expr False b
	return $ BoolOp tp aa bb
expr _ (D.MathOp tp a b) = do
	aa <- expr False a
	bb <- expr False b
	return $ MathOp tp aa bb
expr _ (D.Dot a b) = do
	aa <- expr False a
	env <- get
	bb <- exprCall (Just $ (dataTypeClass . exprDataType) aa)  b
	put env
	return $ Dot aa bb
expr _ (D.Set tp a b) = do
	aa <- expr False a
	bb <- expr False b
	return $ Set tp aa bb
expr _ (D.PlusPlus e) = do
	aa <- expr False e
	return $ PlusPlus aa
expr _ (D.MinusMinus e) = do
	aa <- expr False e
	return $ MinusMinus aa
expr _ D.Self = do
	env <- get
	return $ Self $ envSelf env
expr _ r@(D.Ref _) = exprCall Nothing r
expr _ r@(D.Call _ _) = exprCall Nothing r

exprCall :: Maybe Class -> D.Exp -> State Env Exp
exprCall Nothing (D.Ref n) = do
	get >>= (\env -> let
			toRef p = Call p []
		in 
			maybe (exprCall Nothing $ D.Call n []) (return . toRef) $ M.lookup n (envVals env)
		)
exprCall c@(Just _) (D.Ref n) = exprCall c $ D.Call n []
		
exprCall c (D.Call name pars) = do
	env <- get
	rp <- mapM (\ (n, e) -> expr False e >>= (\ ee -> return (n, ee))) pars
	return $
		let
			allDefs :: Maybe Class -> [Def]
			allDefs Nothing = allDefsInClass (envSelf env) ++ envGlobalDefIndex env ++ classConstructors
			allDefs (Just self) = allDefsInClass self
			allDefsInClass cl = classDefs cl  ++ maybe [] allDefsInClass (classExtends cl) 
			classConstructors = (map (constructorToDef . snd) . filter (isClass . snd) . M.toList) (envIndex env)
			constructorToDef cl@Class{className = n, classConstructor = constr} = 
				Def {defName = n, defPars = map constructorParToPar constr, defType = refDataType cl, defBody = Nop, 
				defMods = [DefModStatic, if isStruct cl then DefModStructConstructor else DefModConstructor]}
			constructorParToPar (d, e) = Local (defName d) (defType d) e []
			dd = resolveDef c $ fromMaybe (error err) $ findCall name rp (allDefs c)
			resolveDef Nothing call@(Call d _)
				| DefModStatic `elem` (defMods d) = call
				| otherwise = Dot (Self (envSelf env)) call
			resolveDef _ call = call
			err = "Could find reference for call " ++ callStr ++ "\n" ++
				maybe "" (\cl -> "strict in class " ++ className cl ++ "\n") c ++
				"in defs:\n" ++
				(strs "\n" . map (ind . (showDef False))) (allDefs c)
			callStr = name ++ case pars of
				[] -> ""
				_  -> "(" ++ strs ", " (map ((++ ":") . fromMaybe "" . fst) pars) ++ ")"
		in dd

{- expr x = error $ "No expr for " ++ show x -}

