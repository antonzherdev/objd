module ObjD.Link (
	Sources, File(..), Class(..), Extends(..), Def(..), DataType(..), Exp(..), CImport(..), 
	DefMod(..), MathTp(..), DataTypeMod(..), ClassMod(..), Error(..),
	link, isClass, isDef, isField, isEnum, isVoid, isStub, isStruct, isRealClass, isTrait, exprDataType, isStatic, enumItems,
	classConstructor, classFields, checkErrors, dataTypeClassName, isCoreFile
)where

import 			 Control.Arrow
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Decimal
import           Data.List
import           Ex.String
import qualified ObjD.Struct         as D

detailedReferenceError :: Bool
detailedReferenceError = False

type Sources = [File]
data File = File {fileName :: String, filePackage :: [String], fileImports :: [File], fileCImports :: [CImport]
	, fileClasses :: [Class], globalDefs :: [Def]}

data Class = Class { classMods :: [ClassMod], className :: String, classExtends :: Maybe Extends
		, classDefs :: [Def], classGenerics :: [Class]}
	| Generic {className :: String, classDefs :: [Def], classExtends :: Maybe Extends}
	| ClassError {className :: String, classErrorText :: String, classDefs :: [Def], classExtends :: Maybe Extends
		, classGenerics :: [Class]}
instance Eq Class where
	a == b = className a == className b

isCoreFile :: File -> Bool
isCoreFile File{filePackage = (x : xs)} = x == "core"
isCoreFile _ = False
classError :: String -> String -> Class
classError name err = ClassError { className = name, classErrorText = err, classDefs = [], classExtends = Nothing, classGenerics = []}
classConstructor :: Class -> Maybe Def 
classConstructor Generic{} = Nothing
classConstructor cl = (listToMaybe . filter isConstructor . classDefs) cl
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
isEnum (Class {classMods = mods}) = ClassModEnum `elem` mods
isEnum _ = False
classFields :: Class -> [Def]
classFields = filter isField . classDefs

data ClassMod = ClassModStub | ClassModStruct | ClassModTrait | ClassModEnum | ClassModObject deriving (Eq)

data Extends = Extends {extendsClass :: Class, extendsGenerics :: [DataType], extendsPars :: [(Def, Exp)]}
data Def = Def {defName :: String, defPars :: [Def], defType :: DataType, defBody :: Exp, defMods :: [DefMod]
	, defGenerics :: Maybe DefGenerics}
localVal :: String -> DataType -> Def
localVal name tp = Def name [] tp Nop [DefModLocal] Nothing
isStatic :: Def -> Bool
isStatic = (DefModStatic `elem` ). defMods
isDef :: Def -> Bool
isDef = (DefModDef `elem` ) . defMods
isField :: Def -> Bool
isField = (DefModField `elem` ) . defMods
isEnumItem :: Def -> Bool
isEnumItem = (DefModEnumItem `elem` ) . defMods
isConstructor :: Def -> Bool
isConstructor = (DefModConstructor `elem` ) . defMods
enumItems :: Class -> [Def]
enumItems Class{classDefs = defs} = filter isEnumItem defs
setDefName :: String -> Def -> Def
setDefName name (Def _ pars tp body mods gens) = Def name pars tp body mods gens 

data DefMod = DefModStatic | DefModMutable | DefModAbstract | DefModPrivate  | DefModGlobalVal | DefModWeak
	| DefModConstructor | DefModStub | DefModLocal | DefModObject 
	| DefModField | DefModEnumItem | DefModDef | DefModSpecial deriving (Eq, Ord)
instance Show DefMod where
	show DefModStatic = "static"
	show DefModMutable = "var"
	show DefModAbstract = "abstract" 
	show DefModPrivate = "private"
	show DefModWeak = "weak"
	show DefModConstructor = "constructor"
	show DefModStub = "stub"
	show DefModGlobalVal = "global val"
	show DefModField = "val"
	show DefModLocal = "local"
	show DefModObject = "object"
	show DefModEnumItem = "enum"
	show DefModDef = "def"
	show DefModSpecial = "special"
data DefGenerics = DefGenerics{defGenericsClasses :: [Class], defGenericsSelfType :: DataType}

data CImport = CImportLib String | CImportUser String

instance Show File where
	show (File name _ _ _ classes _) =
		"// " ++ name ++ ".od\n" ++
		{-((`tryCon` "\n\n" ). strs' "\n") cimps ++
		((`tryCon` "\n\n") . strs' "\n") imps ++ 
		{trs' "\n" gldefs ++-}
		strs' "\n\n" classes
instance Show CImport where
	show (CImportLib l) = "import <" ++ l ++ ">"
	show (CImportUser l) = "import \"" ++ l ++ "\""

instance Show Class where
	show Generic{className = name, classExtends = ext} =  name ++ maybe "" ((" " ++ ) . show) ext
	show ClassError{className = name, classErrorText = er} = name ++ ": " ++ er
	show cl =
		tp cl ++ " " ++ className cl ++ sConstr cl ++ maybe "" (( ++ " "). show) (classExtends cl) ++ " {\n" ++
			(unlines . map ind . concatMap (lines . show)) (classDefs cl)  ++
		"}"
		where
			tp c@Class{}
				| isStub c = "stub"
				| isEnum c = "enum"
				| otherwise = "class"
			tp Generic{} = "generic"
			sConstr c = maybe "" (\cc -> "(" ++ (strs ", " . map defName . defPars) cc ++ ")") (classConstructor c)
			
instance Show Extends where
	show (Extends cls generics pars) = "extends " ++ className cls ++ "<" ++ strs' ", " generics ++ "> (" ++ 
		(strs ", " . map (\(d, e) -> show d ++ " = " ++ show e)) pars ++ ")"
instance Show Def where
	show = showDef True

showDef :: Bool -> Def -> String
showDef f Def {defName = name , defPars = [], defType = tp, defBody = e, defMods = mods, defGenerics = gens} =
	strs' " " mods ++ " " ++ name ++ maybe "" show gens ++ " : " ++ show tp ++ if f then " = " ++ show e else ""
showDef f Def {defName = name , defPars = pars, defType = tp, defBody = e, defMods = mods, defGenerics = gens} =
	strs' " " mods ++ " " ++ name ++ maybe "" show gens ++ "(" ++ strs' ", " pars ++ ")" ++ " : " ++ show tp  ++ if f then  " = " ++ show e else ""
instance Show DefGenerics where
	show (DefGenerics [] _) = ""
	show (DefGenerics tps s) = "<" ++ strs' ", " tps ++ " | self = " ++ show s ++ ">"

defRefPrep :: Def -> String
defRefPrep Def{defMods = mods}
	| DefModStub `elem` mods = "<S>"
	| DefModLocal `elem` mods = "<L>"
	| DefModField `elem` mods = "<F>"
	| DefModGlobalVal `elem` mods = "<G>"
	| otherwise = "<D>"

dataTypePars :: DataType -> [Def]
dataTypePars (TPFun (TPTuple pars) _) = map (localVal "") pars
dataTypePars (TPFun t _) = [localVal "" t]
dataTypePars _ = []		 
		

idx :: (a -> k) -> a -> (k, a)
idx f a = (f a, a)

link :: D.Sources -> Sources
link src = map (\D.File{D.fileName = name} ->  fromMaybe (error $ "Could not find linked file " ++ name) $ M.lookup name fidx) src
	where
		fidx :: M.Map String File
		fidx = M.fromList $ map (idx fileName . linkFile fidx) src

linkFile :: M.Map String File -> D.File -> File
linkFile fidx (D.File name package stms) = fl
	where
		fl :: File
		fl = File {fileName = name, fileImports = files, fileCImports = cImports, 
			fileClasses =(map (linkClass (cidx, glidx)) . filter isCls) stms, globalDefs = gldefs, filePackage = package}
		files = visibleFiles stms
		isCls s = D.isClass s || D.isStub s || D.isEnum s
		cidx = M.fromList $ (map (idx className) . concatMap fileClasses . (fl : ) . (++ kernelFiles) . visibleFiles) stms
		glidx = concatMap globalDefs (fl : files ++ kernelFiles)
		visibleFiles :: [D.FileStm] -> [File]
		visibleFiles = mapMaybe (getFile . D.impString) . filter D.isImport
		kernelFiles :: [File]
		kernelFiles = mapMaybe (idxFind fidx) ["ODArray", "ODOption", "ODMap", "ODNS", "ODEnum", "ODTuple", "ODObject"]
		cImports = mapMaybe toCImport stms
		toCImport (D.Import s D.ImportTypeCUser) = Just $ CImportUser s
		toCImport (D.Import s D.ImportTypeCLib) = Just $ CImportLib s
		toCImport _ = Nothing

		getFile f = M.lookup f fidx
		gldefs = (map gldef . filter D.isStubDef) stms
		gldef (D.StubDef d@D.Def{D.defMods = mods}) = 
			(linkDef env d){defMods = DefModStub : mapMaybe md' mods}
			where
				env = Env{envSelf = TPVoid, envIndex = cidx, envGlobalDefIndex = glidx,  envVals = []}
				md' D.DefModVal = Just DefModGlobalVal
				md' _ = Nothing

baseClassExtends :: ClassIndex -> Extends
baseClassExtends cidx = Extends (classFind cidx "ODObject") [] []

linkClass :: (ClassIndex, DefIndex) -> D.FileStm -> Class
linkClass (ocidx, glidx) cl = self
	where
		cidx = ocidx `M.union` M.fromList (map (\g -> (className g, g)) generics)
		env = Env selfType cidx glidx []
		staticEnv = Env (TPObject (refDataTypeMod self) self) ocidx glidx []
		self = case cl of
			D.Class{} -> Class {
				classMods = map clsMod (D.classMods cl), 
				className = D.className cl, 
				classExtends = if D.className cl == "ODObject" then Nothing else Just $ 
					fromMaybe (baseClassExtends cidx) extends, 
				classDefs = constr constrPars : fields ++ defs, 
				classGenerics = generics
			}
			D.Enum{} -> Class {
				classMods = [ClassModEnum], 
				className = D.className cl, 
				classExtends = Just $ Extends (classFind cidx "ODEnum") [TPClass TPMEnum [] self] 
					[(enumOrdinal, callLocalVal "ordinal" TPUInt), (enumName, callLocalVal "name" TPString)], 
				classDefs =  enumConstr: 
					snd (mapAccumL enumItem 0 (D.enumItems cl)) ++ fields ++ defs ++ [Def{
					defName = "values", defType = TPArr False (TPClass TPMEnum [] self), defBody = Nop,
					defMods = [DefModStatic], defPars = [], defGenerics = Nothing}],
				classGenerics = generics
			}
		enumOrdinal = Def "ordinal" [] TPUInt Nop [] Nothing
		enumName = Def "name" [] TPString Nop [] Nothing
		enumAdditionalDefs = [enumOrdinal, enumName]
		selfType = refDataType self (map (TPClass TPMGeneric []) generics)
		clsMod D.ClassModStruct = ClassModStruct
		clsMod D.ClassModStub = ClassModStub
		clsMod D.ClassModTrait = ClassModTrait
		extends = fmap (linkExtends cidx) (D.classExtends cl)
		
		fields =  mapM (evalState . linkField) (filter (D.isStatic) decls) staticEnv ++
			mapM (evalState . linkField) (filter (not . D.isStatic) decls) env
		fieldsMap = M.fromList $ map (idx defName) fields
		decls = D.classFields cl ++ filter D.isDecl (D.classBody cl)
		defs = map (\ def -> linkDef (envForDef def) def) . filter D.isDef $ D.classBody cl
		envForDef def = if D.isStatic def then staticEnv else env
		enumConstr = constr (enumAdditionalDefs ++ constrPars)
		constr :: [Def] -> Def
		constr pars = Def{defName = "new", defMods = [DefModStatic, DefModConstructor], defBody = Nop,
			defPars = pars, defType = selfType, defGenerics = Just $ DefGenerics generics selfType}
		constrPars = map constrPar (D.classFields cl)
		constrPar f = fromJust $ idxFind fieldsMap (D.defName f)
		generics = map (linkGeneric cidx) (D.classGenerics cl) 
		
		
		enumItem :: Int -> D.EnumItem -> (Int, Def)
		enumItem ordinal (D.EnumItem name pars) = (ordinal + 1, Def{defName = name, defMods = [DefModStatic, DefModEnumItem, DefModField], 
				defType = selfType, defGenerics = Nothing, defPars = [], 
				defBody = enumConstrCall})
			where
				enumConstrCall = evalState (exprCall Nothing enumConstrDCall) env
				enumConstrDCall = D.Call 
					(D.className cl) 
					([ (Nothing,D.IntConst ordinal), (Nothing, D.StringConst name)] ++  pars)
					[]

linkExtends :: ClassIndex -> D.Extends -> Extends
linkExtends cidx (D.Extends ecls gens) = Extends (classFind cidx ecls) (map (dataType cidx) gens) []

linkGeneric :: ClassIndex -> D.Generic -> Class
linkGeneric cidx (D.Generic name ext) = Generic name [] (Just $ maybe (baseClassExtends cidx) (linkExtends cidx) ext)

linkField :: D.ClassStm -> State Env Def
linkField D.Def {D.defMods = mods, D.defName = name, D.defRetType = tp, D.defBody = e} = do
	i <- expr e
	env <- get
	let 
		tp' = unwrapGeneric $ getDataType env tp i
		in return Def{defMods = 
			DefModField : translateMods mods, defName = name, defType = tp', 
			defBody = implicitConvertsion tp' i, defGenerics = Nothing, defPars = []}

		

translateMods :: [D.DefMod] -> [DefMod]
translateMods = mapMaybe m
	where 
		m D.DefModStatic = Just DefModStatic
		m D.DefModMutable = Just DefModMutable
		m D.DefModPrivate = Just DefModPrivate
		m D.DefModWeak = Just DefModWeak
		m _ = Nothing
		
linkDef :: Env -> D.ClassStm -> Def
linkDef env ccc = evalState (stateDef ccc) env'
	where 
		env' = envAddClasses generics' env
		generics' = map (linkGeneric $ envIndex env) (D.defGenerics ccc)

		stateDef:: D.ClassStm -> State Env Def
		stateDef D.Def{D.defMods = mods, D.defName = name, D.defPars = opars, D.defRetType = tp, D.defBody = body} = 
			let 
				 pars = linkDefPars (envIndex env') opars
				 pars' = case pars of
				 	[] -> []
				 	x@Def{defName = dn} : xs -> if dn == "self" then xs else x:xs
				 defGenerics' = Just $ DefGenerics generics' $ case pars of
				 	[] -> envSelf env
				 	Def{defName = dn, defType = dtp} : _ -> if dn == "self" then dtp else envSelf env
				 mods' = translateMods mods
				 in 
				(case body of
					D.Nop -> return Def {defMods = DefModDef : DefModAbstract : mods' , defName = name, defGenerics = defGenerics',
							defPars = pars',
							defType = dataType (envIndex env') (fromMaybe (D.DataType "void" []) tp), defBody = Nop} 
					_   -> do 
						modify $ envAddVals pars'
						b <- expr body
						put env'
						let tp' = unwrapGeneric $ getDataType env' tp b
						return Def {defMods = DefModDef : mods', defName = name, defGenerics = defGenerics',
							defPars = pars,
							defType = tp', defBody = maybeAddReturn tp' b})

linkDefPars :: ClassIndex -> [D.Par] -> [Def]
linkDefPars cidx = map (\D.Par { D.parName = pnm, D.parType  = ttt } -> localVal pnm (dataType cidx ttt))

{------------------------------------------------------------------------------------------------------------------------------ 
 - Env 
 ------------------------------------------------------------------------------------------------------------------------------}

type ClassIndex = M.Map String Class
type DefIndex = [Def]
data Env = Env{envSelf :: DataType, envIndex :: ClassIndex, envGlobalDefIndex :: DefIndex, envVals :: [Def]}
envAddVals :: [Def] -> Env -> Env 
envAddVals newVals env@Env{envVals = vals} = env{envVals = vals ++ newVals}
envAddClasses :: [Class] -> Env -> Env 
envAddClasses newClasses env@Env{envIndex = cidx} = env{
	envIndex = cidx `M.union` M.fromList (map (\cc -> (className cc, cc)) newClasses)}
envSelfClass :: Env -> Class 
envSelfClass env = dataTypeClass env $ envSelf env
idxFind :: M.Map String a -> String -> Maybe a
idxFind idxx k = M.lookup k idxx
classFind :: ClassIndex -> String -> Class
classFind cidx name = fromMaybe (classError name ("Class " ++ name ++ " not found") ) $ idxFind cidx name

{------------------------------------------------------------------------------------------------------------------------------ 
 - DataType 
 ------------------------------------------------------------------------------------------------------------------------------}

data DataType = TPInt | TPUInt| TPFloat | TPString | TPVoid 
	| TPClass {tpMod :: DataTypeMod, tpGenerics :: [DataType], tpClass :: Class}
	| TPArr Bool DataType | TPBool | TPFun DataType DataType | TPTuple [DataType] | TPSelf | TPUnknown String 
	| TPMap Bool DataType DataType
	| TPOption DataType | TPGenericWrap DataType | TPNil | TPObject {tpMod :: DataTypeMod, tpClass :: Class} | TPThrow
	| TPAnyGeneric
	deriving (Eq)
data DataTypeMod = TPMClass | TPMStruct | TPMEnum | TPMTrait | TPMGeneric deriving (Eq)
isVoid :: DataType -> Bool
isVoid TPVoid = True
isVoid _ = False
isTpFun :: DataType -> Bool
isTpFun TPFun{} = True
isTpFun _ = False

forDataType :: MonadPlus m => (DataType -> m a) -> DataType -> m a
forDataType f tp = mplus (go tp) (f tp)
	where
		go (TPClass _ gens _) = msum $ map (forDataType f) gens
		go (TPArr _ a) = forDataType f a
		go (TPFun a b) = mplus (forDataType f a) (forDataType f b)
		go (TPMap _ a b) = mplus (forDataType f a) (forDataType f b)
		go (TPGenericWrap a) = forDataType f a
		go (TPOption a) = forDataType f a
		go (TPTuple a) =  msum $ map (forDataType f) a
		go _ = mzero


mapDataType :: (DataType -> Maybe DataType) -> DataType -> DataType
mapDataType f tp = fromMaybe (go tp) (f tp)
	where
		go (TPClass mods gens cl) = TPClass mods (map (mapDataType f) gens) cl
		go (TPArr m a) = TPArr m (mapDataType f a)
		go (TPFun a b) = TPFun (mapDataType f a) (mapDataType f b)
		go (TPMap m a b) = TPMap m (mapDataType f a) (mapDataType f b)
		go (TPGenericWrap a) = TPGenericWrap (mapDataType f a)
		go (TPOption a) = TPOption (mapDataType f a)
		go (TPTuple a) = TPTuple (map (mapDataType f) a)
		go _ = tp

refDataType :: Class -> [DataType] -> DataType
refDataType cl gens = TPClass (refDataTypeMod cl) gens cl

refDataTypeMod :: Class -> DataTypeMod
refDataTypeMod cl
	| isStruct cl = TPMStruct
	| isEnum cl = TPMEnum
	| isTrait cl = TPMTrait
	| isGeneric cl = TPMGeneric
	| otherwise = TPMClass

dataTypeClass :: Env -> DataType -> Class
dataTypeClass _ (TPClass _ _ c ) = c
dataTypeClass env (TPObject _ c) = Class { classMods = [ClassModObject], className = className c, classExtends = Nothing, 
	classDefs = filter ((DefModStatic `elem`) . defMods) (allDefsInClass env c), classGenerics = []}
dataTypeClass env (TPGenericWrap c) = dataTypeClass env c
dataTypeClass env (TPArr False _) = classFind (envIndex env) "ODArray"
dataTypeClass env (TPArr True _) = classFind (envIndex env) "ODMutableArray"
dataTypeClass env (TPOption _) = classFind (envIndex env) "ODOption"
dataTypeClass env (TPMap False _ _) = classFind(envIndex env) "ODMap"
dataTypeClass env (TPMap True _ _) = classFind(envIndex env) "ODMutableMap"
dataTypeClass env (TPTuple [_, _]) = classFind (envIndex env) "CNTuple"
dataTypeClass env (TPInt) = classFind (envIndex env) "ODInt"
dataTypeClass env (TPFloat) = classFind (envIndex env) "ODFloat"
dataTypeClass env (TPUInt) = classFind (envIndex env) "ODUInt"
dataTypeClass env (TPTuple a) = classFind (envIndex env) ("CNTuple" ++ show (length a))
dataTypeClass _ x = classError (show x) ("No dataTypeClass for " ++ show x)

dataTypeClassName :: DataType -> String
dataTypeClassName (TPClass _ _ c ) = className c
dataTypeClassName (TPObject _ c) = className c
dataTypeClassName (TPGenericWrap c) = dataTypeClassName c
dataTypeClassName (TPArr False _) = "ODArray"
dataTypeClassName (TPArr True _) = "ODMutableArray"
dataTypeClassName (TPOption _) = "ODOption"
dataTypeClassName (TPMap False _ _) = "ODMap"
dataTypeClassName (TPMap True _ _) = "ODMutableMap"
dataTypeClassName (TPTuple [_, _]) = "CNTuple"
dataTypeClassName (TPTuple a) = "CNTuple" ++ show (length a)
dataTypeClassName x = error ("No dataTypeClassName for " ++ show x)

dataTypeGenerics :: Env -> DataType -> [DataType]
dataTypeGenerics _ (TPClass _ g _) = g
dataTypeGenerics _ (TPArr _ g) = [g]
dataTypeGenerics _ (TPMap _ k v) = [k, v]
dataTypeGenerics _ (TPOption v) = [v]
dataTypeGenerics _ (TPTuple a) = a
dataTypeGenerics env (TPGenericWrap g) = dataTypeGenerics env g
dataTypeGenerics _ _ = []

wrapGeneric :: DataType -> DataType
wrapGeneric TPVoid = TPVoid
wrapGeneric g@(TPClass TPMGeneric _ _) = g
wrapGeneric g@TPGenericWrap{} = g
wrapGeneric g = TPGenericWrap g
unwrapGeneric :: DataType -> DataType
unwrapGeneric (TPGenericWrap g)= g
unwrapGeneric g = g

dataType :: ClassIndex -> D.DataType -> DataType
dataType cidx (D.DataType name gens) = case name of
	"int" -> TPInt
	"uint" -> TPUInt
	"float" -> TPFloat
	"void" -> TPVoid
	"string" -> TPString
	"bool" -> TPBool
	"self" -> TPSelf
	"_" -> TPAnyGeneric
	_ -> maybe (TPUnknown $ "No class found " ++ name) (\cl -> refDataType cl (map (wrapGeneric . dataType cidx) gens)) (idxFind cidx name)
dataType cidx (D.DataTypeArr m tp) = TPArr m $ wrapGeneric $ dataType cidx tp
dataType cidx (D.DataTypeMap m k v) = TPMap m (wrapGeneric $ dataType cidx k) (wrapGeneric $ dataType cidx v)
dataType cidx (D.DataTypeFun (D.DataTypeTuple tps) d) = TPFun (TPTuple $ map (dataType cidx) tps) (dataType cidx d)
dataType cidx (D.DataTypeFun s d) = TPFun (dataType cidx s) (dataType cidx d)
dataType cidx (D.DataTypeTuple tps) = TPTuple $ map (wrapGeneric . dataType cidx) tps
dataType cidx (D.DataTypeOption t) = TPOption $ (wrapGeneric . dataType cidx) t


instance Show DataType where
	show TPInt = "int"
	show TPUInt = "uint"
	show TPFloat = "float"
	show TPVoid = "void"
	show TPString = "string"
	show TPBool = "bool"
	show TPSelf = "self"
	show TPNil = "nil"
	show TPThrow = "throw"
	show TPAnyGeneric = "_"
	show (TPUnknown s) = s
	show (TPClass t [] c) = className c ++ show t
	show (TPObject t c) = className c ++ show t ++ ".class"
	show (TPClass t gens c) = className c ++ show t ++ "<" ++ strs' ", " gens ++ ">"
	show (TPGenericWrap c) = '^' : show c
	show (TPArr m t) = "[" ++ (if m then "var " else "") ++ show t ++ "]"
	show (TPMap m k v) = "[" ++ (if m then "var " else "") ++ show k ++ " : " ++ show v ++ "]"
	show (TPFun s d) = show s ++ " -> " ++ show d
	show (TPTuple tps) = "(" ++ strs' ", " tps ++ ")"
	show (TPOption t) = show t ++ "?"
instance Show DataTypeMod where
	show TPMClass = "#C"
	show TPMStruct = "#S"
	show TPMEnum = "#E"
	show TPMTrait = "#T"
	show TPMGeneric = "#G"
	
getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType (envIndex env)) tp



{------------------------------------------------------------------------------------------------------------------------------ 
 - Expression 
 ------------------------------------------------------------------------------------------------------------------------------}
data Exp = Nop 
	| IntConst Int 
	| StringConst String 
	| BoolConst Bool 
	| FloatConst Decimal
	| Braces [Exp]
	| If Exp Exp Exp
	| While Exp Exp
	| Do Exp Exp
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
	| ExpDError String D.Exp 
	| ExpLError String Exp 
	| FirstTry D.Exp Exp 
	| Arr [Exp]
	| Map [(Exp, Exp)]
	| Tuple [Exp]
	| Opt Exp
	| None DataType
	| Throw Exp
	| Not Exp
	| Negative Exp
	| Cast DataType Exp
	| As DataType
	| Is DataType
	| Break
	
instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show (While cond e) = "while(" ++ show cond ++ ") " ++ show e
	show (Do cond e) = "do" ++ show e ++ " while(" ++ show cond ++ ")"
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
	show (Call f tp []) = defRefPrep f ++ defName f ++ "\\" ++ show tp ++ "\\"
	show (Call dd tp pars) = defRefPrep dd ++ defName dd ++ "(" ++ strs ", " (map showPar pars) ++ ")" ++ "\\" ++ show tp ++ "\\" 
		where
			showPar (Def {defName = name}, e) = name ++ " = " ++ show e
	show (IntConst i) = show i
	show (StringConst i) = show i
	show Nil = "nil"
	show (BoolConst i) = show i
	show (FloatConst i) = show i
	show (Index e i) = show e ++ "[" ++ show i ++ "]"
	show (Lambda pars e tp) = strs ", " (map (\(n, t) -> n ++ " : " ++ show t) pars) ++ " -> " ++ show tp ++ " = " ++ show e
	show (Val d) = show d
	show (ExpDError s e) = "<#" ++ show e ++ ": " ++ (strs " " . lines) s ++ "#>"
	show (ExpLError s e) = "<#" ++ show e ++ ": " ++ (strs " " . lines) s ++ "#>"
	show (Arr exps) = "["  ++ strs' ", " exps ++ "]"
	show (Map exps) = "["  ++ strs' ", " exps ++ "]"
	show (Tuple exps) = "("  ++ strs' ", " exps ++ ")"
	show (Opt e) = "opt(" ++ show e ++ ")"
	show (None tp) = "none<" ++ show tp ++ ">" 
	show (FirstTry _ e) = "First try: " ++ show e
	show (Throw e) = "throw " ++ show e
	show (Not e) = "!(" ++ show e ++ ")"
	show (Negative e) = '-' : show e
	show (Cast tp e) = show e ++ ".cast<" ++ show tp ++ ")"
	show (As tp) = "as<" ++ show tp ++ ">"
	show (Is tp) = "is<" ++ show tp ++ ">"
	show (Break) = "break"
	
callLocalVal :: String -> DataType -> Exp
callLocalVal name tp = Call (localVal name tp) tp []

maybeAddReturn :: DataType -> Exp -> Exp
maybeAddReturn TPVoid e = e
maybeAddReturn tp e = addReturn tp e

addReturn :: DataType -> Exp -> Exp
addReturn tp (If cond t f) = If cond (addReturn tp t) (addReturn tp f)
addReturn _ e@(Braces []) = ExpLError "Return empty braces" e
addReturn tp (Braces es) = Braces $ init es ++ [addReturn tp (last es)]
addReturn _ Nop = ExpLError "Return NOP" Nop
addReturn _ e@(Throw _) = e
addReturn _ e@(Return _) = e
addReturn tp e = Return $ implicitConvertsion tp e

forExp :: MonadPlus m => (Exp -> m a) -> Exp -> m a
forExp f ee = mplus (go ee) (f ee)
	where
		go (Braces es) = msum $ map (forExp f) es
		go (Arr es) = msum $ map (forExp f) es
		go (Tuple es) = msum $ map (forExp f) es
		go (Map es) = msum $ map (forExp f *** forExp f >>> uncurry mplus) es
		go (Call _ _ pars) = msum $ map (forExp f . snd) pars
		go (If cond te fe) =  mplus (forExp f cond) $ mplus (forExp f te) (forExp f fe)
		go (BoolOp _ l r) = mplus (forExp f l) (forExp f r)
		go (MathOp _ l r) = mplus (forExp f l) (forExp f r)
		go (While l r) = mplus (forExp f l) (forExp f r)
		go (Do l r) = mplus (forExp f l) (forExp f r)
		go (Set _ l r) = mplus (forExp f l) (forExp f r)
		go (Dot l r) = mplus (forExp f l) (forExp f r)
		go (Index l r) = mplus (forExp f l) (forExp f r)
		go (PlusPlus e) = forExp f e
		go (MinusMinus e) = forExp f e
		go (Return e) = forExp f e
		go (Opt e) = forExp f e
		go (Throw e) = forExp f e
		go (Not e) = forExp f e
		go (Negative e) = forExp f e
		go (Lambda _ e _) = forExp f e
		go (FirstTry _ e) = forExp f e
		go (Val d) = forExp f (defBody d)
		go _ = mzero


exprDataType :: Exp -> DataType
exprDataType (If _ _ Nop) = TPVoid
exprDataType (If _ t _) = exprDataType t
exprDataType (While _ _) = TPVoid
exprDataType (Do _ _) = TPVoid
exprDataType (Braces []) = TPVoid
exprDataType (Braces es) = exprDataType $ last es
exprDataType (Nop) = TPVoid
exprDataType (IntConst _ ) = TPInt
exprDataType (StringConst _ ) = TPString
exprDataType Nil = TPNil
exprDataType (BoolConst _ ) = TPBool
exprDataType (FloatConst _) = TPFloat
exprDataType (BoolOp {}) = TPBool
exprDataType (MathOp _ l r) = case(exprDataType l, exprDataType r) of
	(TPInt, TPFloat) -> TPFloat
	(lt, _) -> lt
exprDataType (PlusPlus e) = exprDataType e
exprDataType (MinusMinus e) = exprDataType e
exprDataType (Dot _ b) = exprDataType b
exprDataType (Set _ a _) = exprDataType a
exprDataType (Self s) = s
exprDataType (Call _ t _) = t
exprDataType (Return e) = exprDataType e
exprDataType (Index e i) = resolve $ exprDataType e 
	where  
		resolve (TPArr _ t) = t
		resolve (TPMap _ _ v) = TPOption v
		resolve (TPObject TPMEnum c) = TPClass TPMEnum [] c
		resolve (TPGenericWrap t) = resolve t
		resolve t = TPUnknown $ show t ++ " is not array " ++ show e ++ "[" ++ show i ++ "]"
exprDataType (Lambda pars _ r) = TPFun (parsTp pars) r
	where 
		parsTp :: [(String, DataType)] -> DataType
		parsTp [(_, tp)] = tp
		parsTp ps = TPTuple $ map snd ps
exprDataType e@(ExpDError _ _) = TPUnknown $ show e
exprDataType e@(ExpLError _ _) = TPUnknown $ show e
exprDataType (Arr []) = TPArr False TPVoid
exprDataType (Map []) = TPMap False TPVoid TPVoid
exprDataType (Arr exps) = TPArr False $ wrapGeneric $ exprDataType $ head exps
exprDataType (Map exps) = let (k, v) = ((exprDataType >>> wrapGeneric) *** (exprDataType >>> wrapGeneric)) $ head exps 
	in TPMap False k v
exprDataType (Tuple exps) = TPTuple $ map (wrapGeneric .exprDataType) exps
exprDataType (Val Def{defType = tp}) = tp
exprDataType (Opt v) = TPOption (exprDataType v)
exprDataType (None tp) = tp
exprDataType (FirstTry _ e) = exprDataType e
exprDataType (Throw _) = TPThrow
exprDataType (Not _) = TPBool
exprDataType (Negative e) = exprDataType e
exprDataType (Cast dtp _) = dtp
exprDataType (As dtp) = TPOption dtp
exprDataType (Is _) = TPBool
exprDataType Break = TPVoid
{- exprDataType x = error $ "No exprDataType for " ++ show x -}

expr :: D.Exp -> State Env Exp
expr (D.If cond t f) = do
	c <- expr cond
	tt <- expr t
	ff <- expr f
	return $ If c tt ff
expr (D.While cond t) = do
	c <- expr cond
	tt <- expr t
	return $ While c tt 
expr (D.Do cond t) = do
	c <- expr cond
	tt <- expr t
	return $ Do c tt 
expr (D.Braces []) = return Nop
expr (D.Braces es) = do
	env <- get
	f <- mapM expr es
	put env
	return $ Braces f
expr D.Nop = return Nop
expr (D.IntConst i) = return $ IntConst i
expr (D.StringConst i) = return $ StringConst i
expr D.Nil = return Nil
expr (D.BoolConst i) = return $ BoolConst i
expr (D.FloatConst s) = return $ FloatConst s
expr (D.BoolOp tp a b) = do
	aa <- expr a
	bb <- expr b
	return $ BoolOp tp aa bb
expr (D.MathOp tp a b) = do
	aa <- expr a
	bb <- expr b
	return $ MathOp tp aa bb
expr d@(D.Dot a b) = do
	env <- get
	aa <- case a of
		D.Call {} -> exprCall Nothing a
		_ -> expr a
	case aa of
		ExpDError s _ -> return $ ExpDError s d
		_ -> do
			bb <- exprCall (Just $ exprDataType aa)  b
			put env
			return $ Dot aa bb
expr (D.Set tp a b) = do
	aa <- expr a
	bb <- expr b
	return $ Set tp aa (implicitConvertsion (exprDataType aa) bb)
expr (D.PlusPlus e) = do
	aa <- expr e
	return $ PlusPlus aa
expr (D.MinusMinus e) = do
	aa <- expr e
	return $ MinusMinus aa
expr D.Self = do
	env <- get
	return $ Self $ envSelf env
expr r@D.Call{} = exprCall Nothing r
expr (D.Index e i) = do
	e' <- expr e
	let obf =expr $ D.Dot e (D.Call "objectFor" [(Nothing, i)] [])
	case exprDataType e' of
		TPClass{} -> obf
		(TPGenericWrap TPClass{}) -> obf
		_ -> expr i >>= \i' -> return $ Index e' i'
expr l@(D.Lambda pars e) = if all (isJust.snd) pars then (do
	env <- get
	let pars' = map (second (dataType (envIndex env) . fromJust)) pars
	modify $ envAddVals (map (uncurry localVal) pars')
	e' <- expr e
	put env
	let tp = exprDataType e'
	return $ Lambda pars' (maybeAddReturn tp e') tp)
	else return $ ExpDError "Not all types are defined in lambda" l

expr (D.Val name tp body mods) = do
	env <- get 
	body' <- expr body
	let tp' = unwrapGeneric $ maybe (exprDataType body') (dataType $ envIndex env) tp
	let mods' = DefModLocal : [DefModMutable | D.DefModMutable `elem` mods]
	let def' = Def{defName = name, defType = tp', defMods = mods', defPars = [], 
		defBody = implicitConvertsion tp' body', 
		defGenerics = Nothing}
	modify $ envAddVals [def']
	return $ Val def'
expr (D.Arr items) = do
	items' <- mapM expr items
	return $ Arr items'
expr (D.Tuple items) = do
	items' <- mapM expr items
	return $ Tuple items'
expr (D.Throw e) = do
	e' <- expr e
	return $ Throw e'
expr (D.Return e) = do
	e' <- expr e
	return $ Return e'
expr (D.Not e) = do
	e' <- expr e
	return $ Not e'
expr (D.Negative e) = do
	e' <- expr e
	return $ Negative e'
expr D.Break = return Break
{- expr x = error $ "No expr for " ++ show x -}

{------------------------------------------------------------------------------------------------------------------------------ 
 - Calling 
 ------------------------------------------------------------------------------------------------------------------------------}

type Generics = M.Map String DataType

exprCall :: Maybe DataType -> D.Exp -> State Env Exp		
exprCall (Just (TPUnknown t)) e = return $ ExpDError t e
exprCall (Just _) (D.Call "as" [] [tp]) = get >>= \env -> return $ As $ dataType (envIndex env) tp
exprCall (Just _) (D.Call "is" [] [tp]) = get >>= \env -> return $ Is $ dataType (envIndex env) tp
exprCall strictClass call@(D.Call name pars gens) = do
	env <- get
	pars' <- mapM (\ (n, e) ->  expr e >>= (\ ee -> return (n, FirstTry e ee))) pars
	return $
		let
			self = fromMaybe (envSelf env) strictClass
			call' :: Exp
			call' = fromMaybe (ExpDError errorString call) $ findCall (name, pars') (env, self, allDefs env strictClass)
			call'' :: Exp
			call'' = case call' of
				Call{} -> (resolveDef strictClass . correctCall) call'
				_ -> call'
				where
					resolveDef Nothing c@(Call d _ _)
						| DefModConstructor `elem` defMods d = c
						| DefModObject `elem` defMods d = c
						| DefModLocal `elem` defMods d = c
						| DefModStub `elem` defMods d = c
						| otherwise = Dot (Self (envSelf env)) c
					resolveDef _ c = c
			pars'' :: [(Def, Exp)]
			pars'' = case call' of
				Call _ _ r -> r
			pars''' :: [(Def, Exp)]
			pars''' = (map (correctCallPar env gens') pars'')
			
			gens' :: M.Map String DataType
			gens' = resolveGenerics pars''

			gens'' :: M.Map String DataType
			gens'' = resolveGenerics pars'''

			extendList :: Int -> [a] -> [Maybe a]
			extendList l a
				| length a == l = map Just a
				| length a > l = (map Just . take l) a
				| otherwise = map Just a ++ replicate (l - length a) Nothing 

			resolveGenerics :: [(Def, Exp)] -> M.Map String DataType
			resolveGenerics rpars = let
				ddefGenerics :: DefGenerics -> [(String, DataType)]
				ddefGenerics DefGenerics{defGenericsClasses = defGens, defGenericsSelfType = selfType} = 
					(zipWith (determineGenericType selfType) defGens . extendList (length defGens)) gens
				srcClassGenerics = classGenerics $ dataTypeClass env self
				dclassGenerics :: [(String, DataType)]
				dclassGenerics = (zipWith extractGen srcClassGenerics . extendList (length srcClassGenerics)) (dataTypeGenerics env self) 
					where 
						extractGen :: Class -> Maybe DataType -> (String, DataType)
						extractGen g (Just t) = (className g, t)
						extractGen g Nothing = (className g, TPUnknown $
							"Could not find generic type for " ++ show g ++ " in self " ++ show self ++ " for call " ++ show call')
					
				determineGenericType :: DataType -> Class -> Maybe D.DataType -> (String, DataType)
				determineGenericType _ g (Just tp) = (className g, (wrapGeneric . dataType (envIndex env)) tp)
				determineGenericType selfType g  _ = (className g, 
						fromMaybe (TPUnknown errorText) $ 
						mplus determineBySelfType determineByPars
					)
					where 
						determineByPars :: Maybe DataType
						determineByPars = (listToMaybe . mapMaybe ( (defType *** (exprDataType >>> unwrapGeneric))>>> tryDetermine g) ) rpars
						determineBySelfType :: Maybe DataType
						determineBySelfType = tryDetermine g (selfType, self)
						errorText = "Could not determine generic type for " ++ show g ++ " in " ++ show call 
						tryDetermine :: Class -> (DataType, DataType) -> Maybe DataType
						tryDetermine c (TPClass TPMGeneric _ gg, tp) = if c == gg then Just (wrapGeneric tp) else Nothing
						tryDetermine c (TPArr _ a, TPArr _ a') = tryDetermine c (a, a')
						tryDetermine c (TPOption a, TPOption a') = tryDetermine c (a, a')
						tryDetermine c (TPMap _ a b, TPMap _ a' b') = mplus (tryDetermine c (a, a')) (tryDetermine c (b, b'))
						tryDetermine c (TPTuple a, TPTuple a') = listToMaybe $ mapMaybe (tryDetermine c) (zip a a')
						tryDetermine c (TPClass _ gg _, TPClass _ gg' _) = 
							listToMaybe $ mapMaybe (tryDetermine c) (zip gg gg')
						tryDetermine c (cl@(TPClass _ _ _), TPObject m cl') = 
							tryDetermine c (cl, TPClass TPMClass [TPClass m [] cl'] (classFind (envIndex env) "ODClass"))
						tryDetermine c (TPFun a b, TPFun a' b') = listToMaybe $ catMaybes [tryDetermine c (a, a'), tryDetermine c (b, b')]
						tryDetermine c (TPGenericWrap a, TPGenericWrap b) = tryDetermine c (a, b)
						tryDetermine c (TPGenericWrap a, b) = tryDetermine c (a, b)
						tryDetermine c (a, TPGenericWrap b) = tryDetermine c (a, b)
						tryDetermine _ _ = Nothing
				in case call' of
					(Call Def{defGenerics = Just defGens} _ _) -> 
						M.fromList $ ddefGenerics defGens ++ dclassGenerics
					_ -> M.fromList dclassGenerics 
									
			errorString :: String
			errorString = "Could find reference for call " ++ callStr ++ "\n" ++
				maybe "" (\cl -> "strict in class " ++ show cl ++ "\n") strictClass ++
				(if detailedReferenceError then "in defs:\n" ++
				(strs "\n" . map (ind . showDef False)) (allDefs env strictClass)  else "")
				where callStr = name ++ case pars of
					[] -> ""
					_  -> "(" ++ strs ", " (map ((++ ":") . fromMaybe "" . fst) pars) ++ ")" 

			correctCall :: Exp -> Exp
			correctCall (Call d tp _) = Call d (replaceGenerics gens'' tp) (map doImplicitConversation pars''')
				where
					doImplicitConversation (dd, e) = (dd, implicitConvertsion (defType dd) e)
			
			
		in call''
exprCall _ err = return $ ExpDError "It is not call" err

correctCallPar :: Env -> Generics -> (Def, Exp) -> (Def, Exp)
correctCallPar env gens (d@Def{defType = (TPFun _ _)}, FirstTry _ e'@Lambda{}) = correctCallPar env gens (d, e')
correctCallPar env gens (d@Def{defType = (TPFun _ _)}, FirstTry D.Lambda{} e') = correctCallPar env gens (d, e')
correctCallPar env gens (d@Def{defType = tp@(TPFun _ _)}, FirstTry e e')
	| isTpFun (exprDataType e') = (d, e')
	| otherwise = correctCallPar env gens (d, ExpDError "" $ 
		D.Lambda (map (\(n, _) -> (n, Nothing)) $ lambdaImplicitParameters tp) e)
correctCallPar env gens (d, FirstTry _ e) = correctCallPar env gens (d, e)
correctCallPar _ _ (d@Def{defType = (TPFun _ (TPClass TPMGeneric _ _) )}, Lambda lpars e dtp) = (d, Lambda lpars e dtp)
correctCallPar env gens(d@Def{defType = (TPFun stp dtp)}, ExpDError _ (D.Lambda lambdaPars lambdaExpr)) = (d, Lambda lpars' expr' tp')
	where
		lpars' :: [(String, DataType)]
		lpars' = map (second (replaceGenerics gens)) $ zip (map fst lambdaPars) (stps stp)
		stps :: DataType -> [DataType]
		stps (TPTuple tps) = tps
		stps tp = [tp]
		env' = envAddVals (map (uncurry localVal) lpars') env
		expr' = maybeAddReturn dtp $ evalState (expr lambdaExpr) env'
		tp' = if containsGeneric dtp then wrapGeneric (exprDataType expr') else dtp
		containsGeneric = fromMaybe False . forDataType (\t -> case t of
			TPClass TPMGeneric _ _ -> Just True
			_ -> Nothing)
correctCallPar _ _ e = e

replaceGenerics :: Generics -> DataType -> DataType
replaceGenerics gns = mapDataType f
	where 
		f (TPClass TPMGeneric _ (Generic g _ _)) = M.lookup g gns
		f _ = Nothing

allDefs :: Env -> Maybe DataType -> [Def]
allDefs env (Just ss) = allDefsInClass env $ dataTypeClass env ss
allDefs env Nothing = 
	envVals env 
	++ allDefsInClass env (envSelfClass env) 
	++ allDefsInClass env (dataTypeClass env $ objTp $ envSelfClass env) 
	++ envGlobalDefIndex env 
	++ classConstructors'
	++ objects 
	where
		classConstructors = (map setName . mapMaybe (classConstructor . snd) . M.toList) (envIndex env)
		classConstructors' = filter (not . null . defPars) classConstructors
		setName d@Def{defType = tp} = setDefName (className $ dataTypeClass env tp) d
		objects = (map (objectDef . snd) . M.toList) (envIndex env)
		objTp cl = TPObject (refDataTypeMod cl) cl

objectDef :: Class -> Def
objectDef cl = Def {defName = className cl, defPars = [], defType = TPObject (refDataTypeMod cl) cl, defBody = Nop, 
				defMods = [DefModStatic, DefModObject], defGenerics = Nothing}

allDefsInClass :: Env ->  Class -> [Def]
allDefsInClass env cl = classDefs cl  ++ maybe [] (allDefsInClass env . extendsClass) (classExtends cl) 

	

findCall :: (String, [(Maybe String, Exp)]) -> (Env, DataType, [Def]) -> Maybe Exp
findCall (name,pars) (_, selfType, fdefs) = listToMaybe $ (mapMaybe fit . filter (\d -> defName d == name)) fdefs
	where
		fit :: Def -> Maybe Exp
		fit d
			| length pars == length (defPars d) = Just $ def' d
			| otherwise = case defType d of
				TPFun{} -> Just $ def' d
				_ -> Nothing

		def' d = Call d (resolveTp d) $  zipWith (\dp (_, e) -> (dp, e) ) (defPars' d) pars
		resolveTp d = case defType d of
			TPSelf -> selfType
			tp@(TPFun _ dtp)-> if length pars == length (defPars d) then tp else dtp
			tp -> tp
		defPars' :: Def -> [Def]
		defPars' Def{defType = t, defPars = []} = dataTypePars t
		defPars' Def{defPars = r} = r

			
{- Implicit conversion -}
implicitConvertsion :: DataType -> Exp -> Exp
implicitConvertsion dtp@(TPMap True _ _) (Arr []) = Cast dtp (Map [])
implicitConvertsion (TPMap False _ _) (Arr []) = Map []
implicitConvertsion _ Nop = Nop
implicitConvertsion dtp ex = let stp = exprDataType ex
	in conv stp dtp
	where 
		conv (TPGenericWrap s) d = conv s d
		conv s (TPGenericWrap d) = conv s d
		conv TPFun{} TPFun{} = ex
		conv _ f@(TPFun _ fdtp) = Lambda (lambdaImplicitParameters f) (maybeAddReturn fdtp ex) fdtp
		conv TPOption{} TPOption{} = ex
		conv TPNil (TPOption tp) = None tp
		conv _ (TPOption _) = Opt ex
		conv _ (TPClass TPMGeneric _ _) = ex
		conv (TPMap False _ _) (TPMap True _ _) = Cast dtp ex
		conv (TPArr False _) (TPArr True _) = Cast dtp ex
		conv TPInt TPString = Cast TPString ex
		conv TPUInt TPString = Cast TPString ex
		conv TPFloat TPString = Cast TPString ex
		conv sc dc@TPClass{} = if sc /= dc then classConversion dc sc ex else ex
		conv _ _ = ex

		classConversion c (TPGenericWrap g) e = classConversion c g e
		classConversion (TPClass _ _ cls@Class{classDefs = defs}) sc e =
			maybe e wrapWithApply $
			listToMaybe $ filter(\d -> 
					(defName d == "apply") 
					&& (DefModStatic `elem` defMods d) 
					&& checkApplyPars (defPars d) ) defs
			where
				checkApplyPars [Def{defType = tp}] = tp == sc
				checkApplyPars _ = False
				od = objectDef cls
				wrapWithApply apply@Def{defPars = [par]} = Dot (Call od (defType od) []) (Call apply dtp [(par, e)])
		classConversion t sc _ = ExpLError (show t ++ " from " ++ show sc) ex

lambdaImplicitParameters :: DataType -> [(String, DataType)]
lambdaImplicitParameters (TPFun TPVoid _) = []
lambdaImplicitParameters (TPFun (TPTuple stps) _) = (map(\(tp, i) -> ('_' : show i, tp)) . zipWithIndex) stps
lambdaImplicitParameters (TPFun fstp _) = [("_", fstp)]

{- Errors -}

data Error = Error String | ErrorParent String Error
instance Show Error where
	show (Error s) = s
	show (ErrorParent s e) = s ++ ": " ++ show e

checkErrors :: Sources -> [Error]
checkErrors = concatMap checkErrorsInFile

checkErrorsInFile :: File -> [Error]
checkErrorsInFile File{fileName = name, fileClasses = classes} = 
	map (ErrorParent (name ++ ".od")) $ concatMap checkErrorsInClass classes

checkErrorsInClass :: Class -> [Error]
checkErrorsInClass e@ClassError{} = [Error (show e)]
checkErrorsInClass Generic{} = []
checkErrorsInClass Class{className = name, classExtends = extends, classGenerics = gens, classDefs = defs} = 
	map (ErrorParent $"class " ++ name) (
		maybe [] checkErrorsInExtends extends
		++ concatMap checkErrorsInClass gens
		++ concatMap checkErrorsInDef defs
	)

checkErrorsInExtends :: Extends -> [Error]
checkErrorsInExtends Extends {extendsClass = cls, extendsGenerics = gens, extendsPars = pars} =
	checkErrorsInClass cls 
	++ concatMap checkErrorsInDataType gens
	++ concatMap (checkErrorsInExp . snd) pars
	++ concatMap (checkErrorsInDef . fst) pars

checkErrorsInDef :: Def -> [Error]
checkErrorsInDef Def {defName = name, defPars = pars, defType = tp, defBody = body, defGenerics = gens} =
	map (ErrorParent $"def " ++ name) (
		concatMap checkErrorsInDef pars
		++ checkErrorsInDataType tp
		++ checkErrorsInExp body
		++ maybe [] checkErrorsInDefGenerics gens
	)

checkErrorsInDefGenerics :: DefGenerics -> [Error]
checkErrorsInDefGenerics DefGenerics{defGenericsClasses = classes, defGenericsSelfType = tp} =
	concatMap checkErrorsInClass classes
	++ checkErrorsInDataType tp

checkErrorsInDataType :: DataType -> [Error]
checkErrorsInDataType = forDataType (\t -> case t of
	e@TPUnknown{} -> [Error (show e)]
	_ -> []
	)

checkErrorsInExp :: Exp -> [Error]
checkErrorsInExp = forExp f
	where 
		f (Self tp) = checkErrorsInDataType tp
		f (Call _ tp _) = checkErrorsInDataType tp
		f (Lambda pars _ ret) = checkErrorsInDataType ret ++ concatMap (checkErrorsInDataType . snd) pars
		f (Val Def{defType = tp}) = checkErrorsInDataType tp
		f (ExpDError t e) = [Error (show e ++ ": " ++ t)]
		f (ExpLError t e) = [Error (show e ++ ": " ++ t)]
		f e@FirstTry{} = [Error ("First try in out " ++ show e)]
		f (None tp) = checkErrorsInDataType tp
		f _ = []