module ObjD.Link (
	Sources, File(..), Class(..), Extends, Def(..), Constructor, DataType(..), Exp(..), CImport(..), EnumItem(..), 
	DefMod(..), FieldAcc(..), FieldAccMod(..), MathTp(..),
	link, isClass, isDef, isField, isEnum, isVoid, isStub, isStruct, isRealClass, isTrait, exprDataType
)where

import 			 Control.Arrow
import           Control.Monad.State
import 			 Data.List
import qualified Data.Map            as M
import           Data.Maybe
import           Ex.String
import qualified ObjD.Struct         as D


type Sources = [File]
data File = File {fileName :: String, fileImports :: [File], fileCImports :: [CImport], fileClasses :: [Class], globalDefs :: [Def]}

data Class = Class { classMods :: [ClassMod], className :: String , classExtends :: Extends, classDefs :: [Def], classConstructor :: Constructor
		,classGenerics :: [Class] }
	| Enum { className :: String, classExtends :: Extends, classDefs :: [Def], classConstructor :: Constructor, enumItems :: [EnumItem]
		,classGenerics :: [Class] }
	| Generic {className :: String}
instance Eq Class where
	a == b = className a == className b
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
data Def = Def {defName :: String, defPars :: [Def], defType :: DataType, defBody :: Exp, defMods :: [DefMod], defGenerics :: [Class]}
	| Field { defName :: String, defType :: DataType, defBody :: Exp, defMods :: [DefMod], fieldAccs :: [FieldAcc]}
localVal :: String -> DataType -> Def
localVal name tp = Def name [] tp Nop [DefModLocal] []
isDef :: Def -> Bool
isDef Def{} = True
isDef _ = False
isField :: Def -> Bool
isField Field{} = True
isField _ = False

data DefMod = DefModStatic | DefModMutable | DefModAbstract | DefModPrivate 
	| DefModConstructor | DefModStructConstructor | DefModStub | DefModLocal | DefModEnumList deriving (Eq, Ord)

data FieldAcc = FieldAccRead [FieldAccMod] Exp | FieldAccWrite [FieldAccMod] Exp
data FieldAccMod = FieldAccModPrivate deriving (Eq)

data EnumItem = EnumItem {enumFieldName :: String, enumFieldPars:: [(Def, Exp)]}

type Constructor = [(Def, Exp)]

data DataType = TPInt | TPUInt| TPFloat | TPString | TPVoid | TPClass Class [DataType] | TPStruct Class Bool | TPEnum Class | TPTrait Class 
	| TPGeneric Class | TPArr DataType | TPBool | TPFun DataType DataType | TPTuple [DataType] | TPSelf
isVoid :: DataType -> Bool
isVoid TPVoid = True
isVoid _ = False

refDataType :: Class -> [DataType] -> DataType
refDataType e@Enum{} _ = TPEnum e
refDataType cl gens
	| isStruct cl = TPStruct cl False
	| isTrait cl = TPTrait cl
	| isGeneric cl = TPGeneric cl
	| otherwise = TPClass cl gens

dataTypeClass :: Env -> DataType -> Class
dataTypeClass _ (TPClass c _) = c
dataTypeClass _ (TPStruct c _) = c
dataTypeClass _ (TPEnum c) = c
dataTypeClass _ (TPTrait c) = c
dataTypeClass _ (TPGeneric c) = c
dataTypeClass env (TPArr _) = findTp "array class" (envIndex env) "ODArray"

dataTypeGenerics :: Env -> DataType -> [DataType]
dataTypeGenerics _ (TPClass _ g) = g
dataTypeGenerics _ (TPArr g) = [g]
dataTypeGenerics _ _ = []

setStructGenericFlag :: Bool -> DataType -> DataType
setStructGenericFlag b (TPStruct c _) = TPStruct c b
setStructGenericFlag _ t = t

dataType :: ClassIndex -> D.DataType -> DataType
dataType cidx (D.DataType name gens) = case name of
	"int" -> TPInt
	"uint" -> TPUInt
	"float" -> TPFloat
	"void" -> TPVoid
	"string" -> TPString
	"bool" -> TPBool
	"self" -> TPSelf
	_ -> refDataType (findTp "class" cidx name) (map (dataType cidx) gens)
dataType cidx (D.DataTypeArr tp) = TPArr $ dataType cidx tp
dataType cidx (D.DataTypeFun s d) = TPFun (dataType cidx s) (dataType cidx d)
dataType cidx (D.DataTypeTuple tps) = TPTuple $ map (dataType cidx) tps



instance Show DataType where
	show TPInt = "int"
	show TPUInt = "uint"
	show TPFloat = "float"
	show TPVoid = "void"
	show TPString = "string"
	show TPBool = "bool"
	show TPSelf = "self"
	show (TPClass c []) = className c
	show (TPClass c gens) = className c ++ "<" ++ strs' ", " gens ++ ">"
	show (TPTrait c) = className c ++ "*"
	show (TPEnum c) = className c ++ "*"
	show (TPGeneric c) = className c ++ "*"
	show (TPStruct c s) = className c ++ if s then "^" else "%"
	show (TPArr t) = "[" ++ show t ++ "]"
	show (TPFun s d) = show s ++ " -> " ++ show d
	show (TPTuple tps) = "(" ++ strs' ", " tps ++ ")"

data Exp = Nop 
	| IntConst Int 
	| BoolConst Bool 
	| FloatConst Int Int
	| Braces [Exp]
	| If Exp Exp Exp
	| Self DataType
	| Nil
	| BoolOp BoolTp Exp Exp
	| MathOp MathTp Exp Exp
	| PlusPlus Exp
	| MinusMinus Exp
	| Dot Exp Exp
	| Set (Maybe MathTp) Exp Exp
	| Call Def DataType [(Def, Exp)]
	| Return Exp
	| Index Exp Exp
	| Lambda [(String, DataType)] Exp DataType
	| Val Def
	| Error String D.Exp 

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
	show cl@Generic{} = "generic " ++ className cl
	show cl =
		tp cl ++ " " ++ className cl ++ sConstr cl ++ maybe "" className (classExtends cl) ++ " {\n" ++
			(unlines . map ind . concatMap (lines . show)) (classDefs cl)  ++
		"}"
		where
			tp c@Class{}
				| isStub c = "stub"
				| otherwise = "class"
			tp Enum{} = "enum"
			tp Generic{} = "generic"
			sConstr Generic{} = ""
			sConstr ccc = maybe "" (\cc -> " (" ++ (strs ", " . map constrFld) cc ++ ") ") (Just $ classConstructor ccc)
			constrFld (f, Nop) = defName f
			constrFld (f, e) = defName f ++ " = " ++ show e
instance Show Def where
	show = showDef True

showDef :: Bool -> Def -> String
showDef f Def {defName = name , defPars = [], defType = tp, defBody = e} =
		"def " ++ name ++ if f then " : " ++ show tp ++ " = " ++ show e else ""
showDef f Def {defName = name , defPars = pars, defType = tp, defBody = e} =
	"def " ++ name ++ "(" ++ strs' ", " pars ++ ")" ++ if f then  " : " ++ show tp ++ " = " ++ show e else ""
showDef f Field {defName = nm, defMods = mods, defType = tp, defBody = e } =
	(if DefModMutable `elem` mods then "var" else "val") ++ " " ++ nm ++ if f then " : " ++ show tp ++ show e else ""

instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show Nop = ""
	show (Self c) = "<" ++ show c ++ ">self"
	show (Return e) = "return " ++ show e
	show (Set Nothing l r) = showOp l "=" r
	show (Set (Just t) l r) = showOp l (show t ++ "=") r
	show (BoolOp t l r) = showOp l (show t) r
	show (MathOp t l r) = showOp l (show t) r
	show (PlusPlus e) = show e ++ "++"
	show (MinusMinus e) = show e ++ "--"
	show (Dot l r) = showOp' l "." r
	show (Call f tp []) = defRefPrep f ++ defName f ++ "<" ++ show tp ++ ">"
	show (Call dd tp pars) = defRefPrep dd ++ defName dd ++ "<" ++ show tp ++ ">" ++ "(" ++ strs' ", " (map showPar pars) ++ ")"
		where
			showPar (Def {defName = name}, e) = name ++ " = " ++ show e
	show (IntConst i) = show i
	show Nil = "nil"
	show (BoolConst i) = show i
	show (FloatConst a b) = show a ++ "." ++ show b
	show (Index e i) = show e ++ "[" ++ show i ++ "]"
	show (Lambda pars e tp) = strs' ", " (map (\(n, t) -> n ++ " : " ++ show t) pars) ++ " -> " ++ show tp ++ " = " ++ show e
	show (Val d) = show d
	show (Error s e) = s ++ " in " ++ show e

defRefPrep :: Def -> String
defRefPrep Field{} = "<F>"
defRefPrep Def{defMods = mods}
	| DefModStub `elem` mods = "<S>"
	| DefModLocal `elem` mods = "<L>"
	| otherwise = "<D>"
	
defPars' :: Def -> [Def]
defPars' Def{defType = t, defPars = []} = dataTypePars t
defPars' Def{defPars = r} = r
defPars' Field{defType = t} = dataTypePars t


dataTypePars :: DataType -> [Def]
dataTypePars (TPFun (TPTuple pars) _) = map (\t -> Def "" [] t Nop [DefModLocal] []) pars
dataTypePars (TPFun t _) = [Def "" [] t Nop [DefModLocal] []]
dataTypePars _ = []		 
		

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
		cidx = M.fromList $ (map (idx className) . concatMap fileClasses . (fl : ) . (++ kernelFiles) . visibleFiles) stms
		glidx = concatMap globalDefs (fl : files)
		visibleFiles :: [D.FileStm] -> [File]
		visibleFiles = mapMaybe (getFile . D.impString) . filter D.isImport
		kernelFiles :: [File]
		kernelFiles = map (findTp "file" fidx) ["ODArray"]
		cImports = mapMaybe toCImport stms
		toCImport (D.Import s D.ImportTypeCUser) = Just $ CImportUser s
		toCImport (D.Import s D.ImportTypeCLib) = Just $ CImportLib s
		toCImport _ = Nothing

		getFile f = M.lookup f fidx
		gldefs = (map gldef . filter D.isStubDef) stms
		gldef D.StubDef{D.stubDefName = sn, D.stubDefPars = pars, D.stubDefRetType = tp} = 
			Def {defName = sn, defPars = linkDefPars cidx pars, defType = dataType cidx tp, defMods = [DefModStub], defBody = Nop, defGenerics = []}

type ClassIndex = M.Map String Class
type DefIndex = [Def]
data Env = Env{envSelf :: DataType, envIndex :: ClassIndex, envGlobalDefIndex :: DefIndex, envVals :: [Def]}
envAddVals :: [Def] -> Env -> Env 
envAddVals newVals Env {envSelf = self, envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals} = 
	Env{envSelf = self, envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals ++ newVals}
envAddClasses :: [Class] -> Env -> Env 
envAddClasses newClasses Env {envSelf = self, envIndex = cidx, envGlobalDefIndex = glidx, envVals = vals} = 
	Env{envSelf = self, envIndex = cidx `M.union` M.fromList (map (\cc -> (className cc, cc)) newClasses), envGlobalDefIndex = glidx, envVals = vals}
findTp :: String -> M.Map String a -> String -> a
findTp tp mmm name =  M.findWithDefault (error $ "No " ++ tp ++ " found " ++ name) name mmm
envSelfClass :: Env -> Class 
envSelfClass env = dataTypeClass env $ envSelf env

cls :: (ClassIndex, DefIndex) -> D.FileStm -> Class
cls (ocidx, glidx) cl = self
	where
		cidx = ocidx `M.union` M.fromList (map (\g -> (className g, g)) generics)
		env = Env selfType cidx glidx []
		self = case cl of
			D.Class{} -> Class {
				classMods = map clsMod (D.classMods cl), 
				className = D.className cl, 
				classExtends = extends, 
				classDefs = fields ++ defs, 
				classConstructor = constr,
				classGenerics = generics
			}
			D.Enum{} -> Enum {
				className = D.className cl, 
				classExtends = extends, 
				classDefs = fields ++ defs, 
				classConstructor = constr,
				enumItems = map enumItem (D.enumItems cl), 
				classGenerics = generics
			}
		selfType = refDataType self (map TPGeneric generics)
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
		generic (D.Generic name) = Generic name
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
			modify $ envAddVals [Def name [] tp' Nop [DefModLocal] []]
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
def env ccc = evalState (stateDef ccc) env'
	where 
		env' = envAddClasses generics' env
		genericClass (D.Generic genericName) = Generic genericName
		generics' = map genericClass (D.defGenerics ccc)
		stateDef:: D.ClassStm -> State Env Def
		stateDef D.Def{D.defMods = mods, D.defName = name, D.defPars = opars, D.defRetType = tp, D.defBody = body} = 
			let 
				 pars = linkDefPars (envIndex env') opars
				 mods' = translateMods mods
				 needReturn (Just (D.DataType "void" _)) = False
				 needReturn _ = True
				 in 
				(case body of
					D.Nop -> return Def {defMods = DefModAbstract : mods' , defName = name, defGenerics = generics',
							defPars = pars,
							defType = dataType (envIndex env') (fromMaybe (D.DataType "void" []) tp), defBody = Nop} 
					_   -> do 
						modify $ envAddVals pars
						b <- expr (needReturn tp) body
						put env'
						return Def {defMods = mods', defName = name, defGenerics = generics',
							defPars = pars,
							defType = getDataType env' tp b, defBody = b})

linkDefPars :: ClassIndex -> [D.Par] -> [Def]
linkDefPars cidx = map (\D.Par { D.parName = pnm, D.parType  = ttt } -> Def pnm [] (dataType cidx ttt) Nop [DefModLocal] [])

getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType (envIndex env)) tp


exprDataType :: Exp -> DataType
exprDataType (If _ t _) = exprDataType t
exprDataType (Braces []) = TPVoid
exprDataType (Braces es) = exprDataType $ last es
exprDataType (Nop) = TPVoid
exprDataType (IntConst _ ) = TPInt
exprDataType Nil = TPVoid
exprDataType (BoolConst _ ) = TPBool
exprDataType (FloatConst _ _) = TPFloat
exprDataType (BoolOp {}) = TPBool
exprDataType (MathOp _ l _) = exprDataType l
exprDataType (PlusPlus e) = exprDataType e
exprDataType (MinusMinus e) = exprDataType e
exprDataType (Dot _ b) = exprDataType b
exprDataType (Set _ a _) = exprDataType a
exprDataType (Self s) = s
exprDataType (Call _ t _) = t
exprDataType (Return e) = exprDataType e
exprDataType (Index e _) = case exprDataType e of
	TPArr t -> t
	t -> error $ show t ++ " is not array"
exprDataType (Lambda pars _ r) = TPFun (parsTp pars) r
	where 
		parsTp :: [(String, DataType)] -> DataType
		parsTp [(_, tp)] = tp
		parsTp ps = TPTuple $ map snd ps
exprDataType (Error e m) = TPVoid

expr :: Bool -> D.Exp -> State Env Exp
expr r (D.If cond t f) = do
	c <- expr False cond
	tt <- expr r t
	ff <- expr r f
	return $ If c tt ff
expr r (D.Braces es) = do
	env <- get
	v <- mapM (expr r) es 
	put env
	return $ Braces v
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
	bb <- exprCall (Just $ exprDataType aa)  b
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
expr _ r@D.Call{} = exprCall Nothing r
expr _ (D.Index e i) = do
	e' <- expr False e
	i' <- expr False i
	return $ Index e' i'
expr _ l@(D.Lambda pars e) = if any (isJust.snd) pars then (do
	env <- get
	let pars' = map (second (dataType (envIndex env) . fromJust)) pars
	modify $ envAddVals (map (uncurry localVal) pars')
	e' <- expr True e
	put env
	let tp = exprDataType e'
	return $ Lambda pars' e' tp)
	else return $ Error "Not all types are defined in lambda" l

expr _ (D.Val name tp body mods) = do
	env <- get 
	body' <- expr False body
	let tp' = setStructGenericFlag False $ maybe (exprDataType body') (dataType $ envIndex env) tp
	let mods' = DefModLocal : [DefModMutable | D.DefModMutable `elem` mods]
	let def' = Def{defName = name, defType = tp', defMods = mods', defPars = [], defBody = body', defGenerics = []}
	modify $ envAddVals [def']
	return $ Val def'

{- expr x = error $ "No expr for " ++ show x -}

exprCall :: Maybe DataType -> D.Exp -> State Env Exp		
exprCall strictClass call@(D.Call name pars gens) = do
	env <- get
	rp <- mapM (\ (n, e) -> expr False e >>= (\ ee -> return (n, ee))) pars
	return $
		let
			self = fromMaybe (envSelf env) strictClass
			call' :: Exp
			call' = fromMaybe (Error errorString call) $ findCall (name, rp) (env, self, allDefs env strictClass)
			call'' :: Exp
			call'' = case call' of
				Call{} -> (resolveDef strictClass . correctCall) call'
				_ -> call'
				where
					resolveDef Nothing c@(Call d _ _)
						| DefModStatic `elem` defMods d = c 
						| DefModLocal `elem` defMods d = c
						| DefModStub `elem` defMods d = c
						| otherwise = Dot (Self (envSelf env)) c
					resolveDef _ c = c
			pars' :: [(Def, Exp)]
			pars' = case call' of
				(Call _ _ cpars) -> cpars
			pars'' :: [(Def, Exp)]
			pars'' = (map correctExpression pars')
			
			gens' :: M.Map String DataType
			gens' = resolveGenerics pars'

			gens'' :: M.Map String DataType
			gens'' = resolveGenerics pars''

			extendList :: Int -> [a] -> [Maybe a]
			extendList l a
				| length a == l = map Just a
				| length a > l = (map Just . take l) a
				| otherwise = map Just a ++ replicate (l - length a) Nothing 

			resolveGenerics :: [(Def, Exp)] -> M.Map String DataType
			resolveGenerics rpars = case call' of
				(Call Def{defGenerics = defGens} _ _) -> 
					M.fromList $  checkedDefGenerics ++ dclassGenerics
					where
						checkedDefGenerics :: [(String, DataType)]
						checkedDefGenerics = if length ddefGenerics /= length defGens 
							then (error $ show ddefGenerics  ++ " != " ++ show defGens) 
							else ddefGenerics
						ddefGenerics :: [(String, DataType)]
						ddefGenerics = (map determineGenericType . zip defGens . extendList (length defGens)) gens
						srcClassGenerics = classGenerics $ dataTypeClass env self
						dclassGenerics :: [(String, DataType)]
						dclassGenerics = (map extractGen . zip srcClassGenerics . extendList (length srcClassGenerics)) (dataTypeGenerics env self) 
							where 
								extractGen :: (Class, Maybe DataType) -> (String, DataType)
								extractGen(g, Just t) = (className g, t)
								extractGen(g, Nothing) = error $ "Could not find generic type for " ++ show g ++ " in self " ++ show self
							
						determineGenericType :: (Class, Maybe D.DataType) -> (String, DataType)
						determineGenericType (g, Just tp) = (className g, (setStructGenericFlag True . dataType (envIndex env)) tp)
						determineGenericType (g, _) = (className g, 
								fromMaybe (error $ "Could not determine generic type for " ++ show g ++ " in " ++ show call) $ 
								(listToMaybe . mapMaybe ( (defType *** exprDataType)>>> tryDetermine g) ) rpars
							)
							where 
								tryDetermine :: Class -> (DataType, DataType) -> Maybe DataType
								tryDetermine c (TPGeneric gg, tp) = if c == gg then Just (setStructGenericFlag True tp) else Nothing
								tryDetermine c (TPArr a, TPArr a') = tryDetermine c (a, a')
								tryDetermine c (TPFun a b, TPFun a' b') = listToMaybe $ catMaybes [tryDetermine c (a, a'), tryDetermine c (b, b')]
								tryDetermine _ _ = Nothing
			
			errorString :: String
			errorString = "Could find reference for call " ++ callStr ++ "\n" ++
				maybe "" (\cl -> "strict in class " ++ show cl ++ "\n") strictClass ++
				"in defs:\n" ++
				(strs "\n" . map (ind . showDef False)) (allDefs env strictClass)
				where callStr = name ++ case pars of
					[] -> ""
					_  -> "(" ++ strs ", " (map ((++ ":") . fromMaybe "" . fst) pars) ++ ")"

			correctCall :: Exp -> Exp
			correctCall (Call d tp _) = Call d (correctType gens'' tp) pars''

			correctExpression :: (Def, Exp) -> (Def, Exp)
			correctExpression(d@Def{defType = (TPFun _ TPGeneric{})}, Lambda lpars e dtp) = (d, Lambda lpars e (setStructGenericFlag True dtp))
			correctExpression(d@Def{defType = (TPFun stp _)}, Error _ (D.Lambda lambdaPars lambdaExpr)) = (d, Lambda lpars' expr' tp')
				where
					lpars' :: [(String, DataType)]
					lpars' = map (second (correctType gens')) $ zip (map fst lambdaPars) (stps stp)
					stps :: DataType -> [DataType]
					stps (TPTuple tps) = tps
					stps tp = [tp]
					env' = envAddVals (map (uncurry localVal) lpars') env
					expr' = evalState (expr True lambdaExpr) env'
					tp' = setStructGenericFlag True $ exprDataType expr'
			correctExpression e = e
			
			correctType :: M.Map String DataType -> DataType -> DataType
			correctType gns gg@(TPGeneric (Generic g)) = fromMaybe gg $ M.lookup g gns
			correctType gns (TPClass c g) = TPClass c (map (correctType gns) g)
			correctType gns (TPArr c) = TPArr (correctType gns c)
			correctType _ t = t
		in call''


allDefs :: Env -> Maybe DataType -> [Def]
allDefs env (Just ss) = allDefsInClass $ dataTypeClass env ss
allDefs env Nothing = envVals env ++ allDefsInClass (envSelfClass env) ++ envGlobalDefIndex env ++ classConstructors
	where
		classConstructors = (mapMaybe (constructorToDef . snd) . M.toList) (envIndex env)
		constructorToDef cl@Class{className = n, classConstructor = constr} = 
			Just Def {defName = n, defPars = map constructorParToPar constr, defType = refDataType cl [], defBody = Nop, 
				defMods = [DefModStatic, if isStruct cl then DefModStructConstructor else DefModConstructor], defGenerics = []}
		constructorToDef cl@Enum{className = n} =
			Just Def {defName = n, defPars = [], defType = TPArr $ refDataType cl [], defBody = Nop, 
				defMods = [DefModStatic, DefModEnumList], defGenerics = []}
		constructorToDef _ = Nothing
		constructorParToPar (d, e) = Def (defName d) [] (defType d) e [DefModLocal] []


allDefsInClass :: Class -> [Def]
allDefsInClass Generic{} = []
allDefsInClass cl = classDefs cl  ++ maybe [] allDefsInClass (classExtends cl) 
	
findCall :: (String, [(Maybe String, Exp)]) -> (Env, DataType, [Def]) -> Maybe Exp
findCall (name,pars) (_, selfType, fdefs) = listToMaybe $ (mapMaybe fit . filter (\d -> defName d == name)) fdefs
	where
		fit :: Def -> Maybe Exp
		fit d@Field{}
			| null pars = Just $ Call d (resolveTp d) [] 
			| otherwise = Nothing 
		fit d = Just $ Call d (resolveTp d) $  zipWith (\dp (_, e) -> (dp, e) ) (defPars' d) pars
		resolveTp d = case defType d of
			TPSelf -> selfType
			tp -> tp
			
