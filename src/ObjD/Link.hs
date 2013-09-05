module ObjD.Link (
	Sources, File(..), Class(..), Extends(..), Def(..), DataType(..), Exp(..), CImport(..), 
	DefMod(..), MathTp(..), DataTypeMod(..), ClassMod(..), Error(..), ExtendsClass(..), ExtendsRef,
	link, isClass, isType, isDef, isField, isEnum, isVoid, isStub, isStruct, isRealClass, isTrait, exprDataType, isStatic, enumItems,
	classConstructor, classFields, checkErrors, dataTypeClassName, isCoreFile, unwrapGeneric, forExp, extendsRefs, extendsClassClass,
	tpGeneric, superType, wrapGeneric, isConst, int, uint, byte, ubyte, int4, uint4, float, float4, resolveTypeAlias
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
data File = File {fileName :: String, filePackage :: [String], fileImports :: [File], fileCImports :: [CImport], fileExports :: [File]
	, fileClasses :: [Class], globalDefs :: [Def]}
instance Eq File where
	File {fileName = a, filePackage = ap} == File {fileName = b, filePackage = bp} = a == b && ap == bp

{-----------------------------------------------------------------------------------------------------------------------------------------
 - CLASS 
 -----------------------------------------------------------------------------------------------------------------------------------------}
data Class = Class { classMods :: [ClassMod], className :: String, classExtends :: Extends
		, classDefs :: [Def], classGenerics :: [Class]}
	| Generic {className :: String, classDefs :: [Def], classExtends :: Extends, classGenerics :: [Class]}
	| ClassError {className :: String, classErrorText :: String, classDefs :: [Def], classExtends :: Extends
		, classGenerics :: [Class]}

data ClassMod = ClassModStub | ClassModStruct | ClassModTrait | ClassModEnum | ClassModObject | ClassModType deriving (Eq)

type ExtendsRef = (Class, [DataType])
data Extends = Extends {extendsClass :: Maybe ExtendsClass, extendsTraits :: [ExtendsRef]}
type CallPar = (Def, Exp)
data ExtendsClass = ExtendsClass ExtendsRef [CallPar]
type Generics = M.Map String DataType
type ClassRef = (Class, Generics)

instance Show Class where
	show Generic{className = name, classExtends = ext} =  name ++ " " ++ show ext
	show ClassError{className = name, classErrorText = er} = name ++ ": " ++ er
	show cl =
		tp cl ++ " " ++ className cl ++ sConstr cl ++ " " ++ show (classExtends cl) ++ " {\n" ++
			(unlines . map ind . concatMap (lines . show)) (classDefs cl)  ++
		"}"
		where
			tp Class{classMods = mods} = strs' " " mods
			tp Generic{} = "generic"
			sConstr c = maybe "" (\cc -> "(" ++ (strs ", " . map defName . defPars) cc ++ ")") (classConstructor c)
instance Show ClassMod where
	show ClassModStub = "stub"
	show ClassModStruct = "struct"
	show ClassModTrait = "trait"
	show ClassModEnum = "enum"
	show ClassModObject = "object"
			
instance Show Extends where
	show (Extends Nothing []) = ""
	show (Extends (Just (ExtendsClass clRef pars) ) traits ) = "extends " ++ showExtendsRef clRef ++ showCallPars pars 
		++ (unwords . map ( (" with " ++ ). showExtendsRef)) traits
	show (Extends Nothing traits) = "extends with " ++ (strs " with " . map showExtendsRef) traits

showExtendsRef :: ExtendsRef -> String
showExtendsRef (cl, []) = className cl
showExtendsRef (cl, gens) = className cl ++ "<" ++ strs' ", " gens ++ ">" 

instance Eq Class where
	a == b = className a == className b

isCoreFile :: File -> Bool
isCoreFile File{filePackage = (x : _)} = x == "core"
isCoreFile _ = False
classError :: String -> String -> Class
classError name err = ClassError { className = name, classErrorText = err, classDefs = [], classExtends = extendsNothing, classGenerics = []}
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
isType :: Class -> Bool
isType (Class {classMods = mods}) = ClassModType `elem` mods
isType _ = False
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

extendsNothing :: Extends
extendsNothing = Extends Nothing []

extendsRefs :: Extends -> [ExtendsRef]
extendsRefs (Extends Nothing traits) = traits
extendsRefs (Extends (Just (ExtendsClass cl _)) traits) = cl : traits

superClass :: Class -> Maybe Class
superClass = fmap extendsClassClass . extendsClass . classExtends

superClasses :: Class -> [Class]
superClasses = map fst . extendsRefs . classExtends

replaceGenerics :: Generics -> DataType -> DataType
replaceGenerics gns = mapDataType f
	where 
		f (TPClass TPMGeneric _ (Generic g _ _ _)) = M.lookup g gns
		f _ = Nothing

objectDef :: Class -> Def
objectDef cl = Def {defName = className cl, defPars = [], defType = TPObject (refDataTypeMod cl) cl, defBody = Nop, 
				defMods = [DefModStatic, DefModObject], defGenerics = Nothing}

replaceGenericsInDef :: Generics -> Def -> Def
replaceGenericsInDef gens d = d {defType = replaceGenerics gens (defType d), defPars = map (replaceGenericsInDef gens) (defPars d) }
		

allDefsInClass :: ClassRef -> [Def]
allDefsInClass (cl, gens) = 
	map (replaceGenericsInDef gens) notStaticDefs 
	++ defsInParentClass (classExtends cl) 
	where
		notStaticDefs = filter (not . isStatic) (classDefs cl) 
		defsInParentClass :: Extends -> [Def]
		defsInParentClass extends = concatMap defsInExtends $ extendsRefs extends
		defsInExtends :: ExtendsRef -> [Def]
		defsInExtends extRef@(cll, _)= map addSuperMod $ allDefsInClass (cll, (superGenerics gens extRef))
		addSuperMod d
			| DefModSuper `elem` defMods d = d
			| otherwise = d {defMods = DefModSuper : defMods d}

allDefsInObject :: ClassRef -> [Def]
allDefsInObject (cl, gens) = 
	map (replaceGenericsInDef gens) staticDefs  
	++ defsInParentObject (classExtends cl) 
	where
		staticDefs = filter isStatic (classDefs cl) 
		defsInParentObject :: Extends -> [Def]
		defsInParentObject extends = concatMap defsInExtends $ extendsRefs extends
		defsInExtends :: ExtendsRef -> [Def]
		defsInExtends extRef@(cll, _)= map addSuperMod $ allDefsInObject (cll, (superGenerics gens extRef))
		addSuperMod d
			| DefModSuper `elem` defMods d = d
			| otherwise = d {defMods = DefModSuper : defMods d}


buildGenerics :: Class -> [DataType] -> Generics
buildGenerics cl gens = M.fromList $ zip (map className $ classGenerics cl) gens

buildGenericsForSelf :: Class -> Generics
buildGenericsForSelf cl = M.fromList $ map (\g -> (className g, refDataType g []) ) $ classGenerics cl


superGenerics :: Generics -> ExtendsRef -> Generics
superGenerics gens (cl, extGens) = buildGenerics cl $ map (wrapGeneric .replaceGenerics gens) extGens

superClassRef :: ClassRef -> ExtendsRef -> ClassRef
superClassRef (_, gens) (cl, extGens) = (cl, buildGenerics cl $ map (wrapGeneric .replaceGenerics gens) extGens)


upGenericsToClass :: Class -> ClassRef -> Maybe Generics
upGenericsToClass destinationClass (cl, gens) 
	| destinationClass == cl = Just gens
	| otherwise = listToMaybe $ mapMaybe mapRef $ extendsRefs (classExtends cl)
		where
			mapRef ref@(ccl, _)= upGenericsToClass destinationClass (ccl, superGenerics gens ref)
extendsClassClass :: ExtendsClass -> Class
extendsClassClass (ExtendsClass (cl, _) _) = cl

extendsClassRef :: ExtendsClass -> ExtendsRef
extendsClassRef (ExtendsClass ref _) = ref


superType :: DataType -> Maybe DataType
superType (TPClass _ gens cl) = fmap superTypeForExtClass $ (extendsClass . classExtends) cl 
	where
		generics = buildGenerics cl gens
		superTypeForExtClass :: ExtendsClass -> DataType
		superTypeForExtClass (ExtendsClass extRef@(extCl, _) _) = 
			TPClass (refDataTypeMod extCl) (map snd $ M.toList $ superGenerics generics extRef) extCl
superType (TPObject _ cl) = fmap superTypeForExtClass $ (extendsClass . classExtends) cl 
	where
		superTypeForExtClass :: ExtendsClass -> DataType
		superTypeForExtClass (ExtendsClass (extCl, _) _) = 
			TPObject (refDataTypeMod extCl) extCl

isInstanceOf :: Class -> Class -> Bool
isInstanceOf target cl 
	| target == cl = True
	| otherwise = any (isInstanceOf target . fst) $ extendsRefs (classExtends cl)

commonClassRef :: ClassRef -> ClassRef -> [ClassRef]
commonClassRef rr1@(cl, _ ) rr2@(cl2, _) 
	| isInstanceOf cl2 cl = [rr2]
	| otherwise = concatMap (commonClassRef rr1 . superClassRef rr2) $ extendsRefs (classExtends cl)

isInstanceOfTp :: Env -> DataType -> DataType -> Bool
isInstanceOfTp env target cl 
	| target == cl = True
	| otherwise = isInstanceOf (dataTypeClass env target) (dataTypeClass env cl)

findDefWithName :: String -> Class -> Maybe Def
findDefWithName name cl = find ((name ==) . defName) $ classDefs cl
	
{-----------------------------------------------------------------------------------------------------------------------------------------
 - Def 
 -----------------------------------------------------------------------------------------------------------------------------------------}

data Def = Def {defName :: String, defPars :: [Def], defType :: DataType, defBody :: Exp, defMods :: [DefMod]
	, defGenerics :: Maybe DefGenerics}
unknownDef :: Def
unknownDef = Def "???" [] TPVoid Nop [] Nothing
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
instance Eq Def where
	a == b = defName a == defName b && length (defPars a) == length (defPars b) && all eqPar (zip (defPars a)(defPars b)) && (isStatic a == isStatic b)

eqPar :: (Def, Def) -> Bool
eqPar (x, y) = defName x == defName y

data DefMod = DefModStatic | DefModMutable | DefModAbstract | DefModPrivate | DefModProtected | DefModGlobalVal | DefModWeak
	| DefModConstructor | DefModStub | DefModLocal | DefModObject 
	| DefModField | DefModEnumItem | DefModDef | DefModSpecial | DefModStruct | DefModApplyLambda | DefModSuper | DefModInline deriving (Eq, Ord)
instance Show DefMod where
	show DefModStatic = "static"
	show DefModMutable = "var"
	show DefModAbstract = "abstract" 
	show DefModPrivate = "private"
	show DefModProtected = "protected"
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
	show DefModStruct = "struct"
	show DefModInline = "inline"
data DefGenerics = DefGenerics{defGenericsClasses :: [Class], defGenericsSelfType :: DataType}

data CImport = CImportLib String | CImportUser String

instance Show File where
	show (File name _ _ _ _ classes _) =
		"// " ++ name ++ ".od\n" ++
		{-((`tryCon` "\n\n" ). strs' "\n") cimps ++
		((`tryCon` "\n\n") . strs' "\n") imps ++ 
		{trs' "\n" gldefs ++-}
		strs' "\n\n" classes
instance Show CImport where
	show (CImportLib l) = "import <" ++ l ++ ">"
	show (CImportUser l) = "import \"" ++ l ++ "\""

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
defRefPrep Def{defMods = mods} = "<" ++  map ch mods ++ ">"
	where
		ch DefModStatic = 't'
		ch DefModMutable = 'm'
		ch DefModAbstract = 'a' 
		ch DefModPrivate = 'p'
		ch DefModProtected = 'q'
		ch DefModWeak = 'w'
		ch DefModConstructor = 'c'
		ch DefModStub = 'b'
		ch DefModGlobalVal = 'g'
		ch DefModField = 'f'
		ch DefModLocal = 'l'
		ch DefModObject = 'o'
		ch DefModEnumItem = 'e'
		ch DefModDef = 'd'
		ch DefModSpecial = 'i'
		ch DefModStruct = 's'
		ch DefModApplyLambda = 'd'
		ch DefModSuper = 'r'
		ch DefModInline = 'i'


dataTypePars :: DataType -> [Def]
dataTypePars (TPFun (TPTuple pars) _) = map (localVal "") pars
dataTypePars (TPFun t _) = [localVal "" t]
dataTypePars _ = []		 
		

idx :: (a -> k) -> a -> (k, a)
idx f a = (f a, a)

{-----------------------------------------------------------------------------------------------------------------------------------------
 - Link 
 -----------------------------------------------------------------------------------------------------------------------------------------}

link :: D.Sources -> Sources
link src = map (\D.File{D.fileName = name} ->  fromMaybe (error $ "Could not find linked file " ++ name) $ M.lookup name fidx) src
	where
		fidx :: M.Map String File
		fidx = M.fromList $ map (idx fileName . linkFile fidx) src

linkFile :: M.Map String File -> D.File -> File
linkFile fidx (D.File name package stms) = fl
	where
		fl :: File
		fl = File {fileName = name, fileImports = imports, fileCImports = cImports, fileExports = exports,
			fileClasses =(map (linkClass (cidx, glidx)) . filter isCls) stms, globalDefs = gldefs, filePackage = package}
		isCls s = D.isClass s || D.isStub s || D.isEnum s || D.isType s
		cidx = M.fromList $ (map (idx className) . concatMap fileClasses . (fl : ) . (++ kernelFiles) ) imports
		glidx = concatMap globalDefs (fl : imports ++ kernelFiles)
		
		imports = 
			let local = mapMaybe (getFile . D.impString) . filter D.isImport $ stms
			in nub $ local ++ exports ++ concatMap fileExports local
		exports = mapMaybe (getFile . D.expString) . filter D.isExport $ stms

		kernelFiles :: [File]
		kernelFiles = mapMaybe (idxFind fidx) ["ODEnum", "ODObject", "CNTuple", "CNOption", "CNList", "CNMap", "CNSeq", "CNData", "ODType", "CNLazy"]
		cImports = mapMaybe toCImport stms
		toCImport (D.Import s D.ImportTypeCUser) = Just $ CImportUser s
		toCImport (D.Import s D.ImportTypeCLib) = Just $ CImportLib s
		toCImport _ = Nothing

		getFile f = M.lookup f fidx
		gldefs = (map gldef . filter D.isStubDef) stms
		gldef (D.StubDef d@D.Def{D.defMods = mods}) = 
			(linkDef False env d){defMods = DefModStub : mapMaybe md' mods}
			where
				env = Env{envSelf = TPVoid, envIndex = cidx, envGlobalDefIndex = glidx,  envVals = []}
				md' D.DefModVal = Just DefModGlobalVal
				md' _ = Nothing

baseClassExtends :: ClassIndex -> ExtendsClass
baseClassExtends cidx = ExtendsClass (classFind cidx "ODObject", []) []

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
				classExtends = if D.className cl == "ODObject" then extendsNothing else fromMaybe (Extends (Just $ baseClassExtends cidx) []) extends, 
				classDefs = fields ++ defs ++ [constr constrPars, typeField] 
				{-++ [unapply | D.ClassModTrait `notElem` D.classMods cl && not hasUnapply]-}, 
				classGenerics = generics
			}
			D.Enum{} -> Class {
				classMods = [ClassModEnum], 
				className = D.className cl, 
				classExtends = Extends (Just $ ExtendsClass 
					(classFind cidx "ODEnum", [TPClass TPMEnum [] self])  
					[(enumOrdinal, callLocalVal "ordinal" uint), (enumName, callLocalVal "name" TPString)]) [], 
				classDefs =  enumConstr: 
					snd (mapAccumL enumItem 0 (D.enumItems cl)) ++ fields ++ defs ++ [Def{
					defName = "values", defType = TPArr 0 (TPClass TPMEnum [] self), defBody = Nop,
					defMods = [DefModStatic], defPars = [], defGenerics = Nothing}],
				classGenerics = generics
			}
			D.Type{} -> Class {
				classMods = [ClassModType, ClassModStub], 
				className = D.className cl, 
				classExtends = Extends (Just $ ExtendsClass (linkExtendsRef env (D.typeDef cl)) []) [], 
				classDefs = [constructorForType], 
				classGenerics = generics
			}
		enumOrdinal = Def "ordinal" [] uint Nop [] Nothing
		enumName = Def "name" [] TPString Nop [] Nothing
		enumAdditionalDefs = [enumOrdinal, enumName]
		selfType = refDataType self (map (TPClass TPMGeneric []) generics)
		clsMod D.ClassModStruct = ClassModStruct
		clsMod D.ClassModStub = ClassModStub
		clsMod D.ClassModTrait = ClassModTrait
		extends = fmap (linkExtends env constrPars) (D.classExtends cl) 
		selfIsStruct = case cl of
			D.Class{} -> D.ClassModStruct `elem` D.classMods cl
			_ -> False

		fields =  join $ mapM (evalState . linkField selfIsStruct) (filter (D.isStatic) decls) staticEnv ++
			mapM (evalState . linkField selfIsStruct) (filter (not . D.isStatic) decls) env
		decls = filter (not . containsInSuper) (D.classFields cl) ++ filter D.isDecl (D.classBody cl)
		containsInSuper D.Def {D.defName = name} =  case superClass self of
			Nothing -> False
			Just super -> any (\d -> DefModField `elem` defMods d && defName d == name) $ classDefs super

		defs = map (\ def -> linkDef selfIsStruct (envForDef def) def) . filter D.isDef $ D.classBody cl
		envForDef def = if D.isStatic def then staticEnv else env
		enumConstr = constr (enumAdditionalDefs ++ constrPars)
		constr :: [Def] -> Def
		constr pars = Def{defName = "apply", defMods = [DefModStatic, DefModConstructor] ++ [DefModStruct | selfIsStruct], defBody = Nop,
			defPars = pars, defType = selfType, defGenerics = Just $ DefGenerics generics selfType}
		constrPars = map constrPar (D.classFields cl)
		constrPar D.Def{D.defName = name, D.defRetType = Just tp} = localVal name $ dataType cidx tp
		generics = map (linkGeneric env) (D.classGenerics cl) 
		
		
		enumItem :: Int -> D.EnumItem -> (Int, Def)
		enumItem ordinal (D.EnumItem name pars) = (ordinal + 1, Def{defName = name, defMods = [DefModStatic, DefModEnumItem, DefModField], 
				defType = selfType, defGenerics = Nothing, defPars = [], 
				defBody = enumConstrCall})
			where
				enumConstrCall = exprCall env Nothing enumConstrDCall
				enumConstrDCall = D.Call 
					(D.className cl) 
					(Just $ [ (Nothing,D.IntConst ordinal), (Nothing, D.StringConst name)] ++  pars)
					[]
		constructorForType = parConstructor' {defType = selfType}
			where
				gens = buildGenericsForSelf self
				parGenerics = superGenerics gens (extendsClassRef parClassExtends)
				parClassExtends = fromJust $ extendsClass $ classExtends $ self
				parConstructor = fromJust $ classConstructor $ extendsClassClass $ parClassExtends
				parConstructor' = replaceGenericsInDef parGenerics parConstructor

		typeField :: Def 
		typeField = Def{defMods = [DefModField, DefModStatic, DefModSpecial] ++ [DefModStruct | selfIsStruct], defName = "type", defType = TPClass TPMType [selfType] (classFind cidx typeName), 
			defBody = Nop, 
			defGenerics = Nothing, defPars = []}
			where 
				typeName = if selfIsStruct then "PType" else "ClassType"
	
		{-unapply :: Def
		unapply = Def{defMods = [DefModDef, DefModStatic] ++ [DefModStruct | selfIsStruct], defName = "unapply", defType = unapplyTp, 
			defBody = Return True unapplyRet, defGenerics = Nothing, defPars = [par]}
			where
				par = localVal (D.className cl) selfType
				unapplyTp = TPOption $ case constrPars of
					[] -> TPNil
					[x] -> wrapGeneric $ defType x
					xs -> TPTuple $ map (wrapGeneric . defType) xs
				unapplyRet :: Exp
				unapplyRet = case constrPars of
					[] -> Some Nil
					[x] -> Some $ Dot (callRef par) (callRef x) 
					xs -> Some $ Tuple $ map (\x -> Dot (callRef par) (callRef x)) xs
		hasUnapply :: Bool
		hasUnapply = any ( ("unapply" == ). D.defName) (D.classBody cl)-}

linkExtends :: Env -> [Def] -> D.Extends -> Extends
linkExtends env constrPars (D.Extends (D.ExtendsClass eref@(_, gens) pars) withs) = 
	let 
		env' = env {envVals = constrPars, envSelf = objectType $ envSelf env}
		superCall = D.Dot D.Super $ D.Call "apply" (Just $ pars) gens
		superCall' = evalState (expr superCall) env'
		superCallPars = case superCall' of
			Dot _ (Call _ _ pars') -> pars'
			err -> [(unknownDef, err)]
	in Extends (Just $ ExtendsClass (linkExtendsRef env eref) superCallPars) $ map (linkExtendsRef env) withs

linkExtendsRef :: Env -> D.ExtendsRef -> ExtendsRef
linkExtendsRef env (ecls, gens) = (classFind (envIndex env) ecls, map (dataType (envIndex env)) gens) 

linkGeneric :: Env -> D.Generic -> Class
linkGeneric env (D.Generic name ext) = Generic name [] (maybe (Extends (Just $ baseClassExtends (envIndex env) ) [] ) (linkExtends env []) ext) []

linkField :: Bool -> D.ClassStm -> State Env [Def]
linkField str D.Def {D.defMods = mods, D.defName = name, D.defRetType = tp, D.defBody = e} = do
	i <- expr e
	env <- get
	let 
		tp' = unwrapGeneric $ getDataType env tp i
		tp'' = if str then case tp' of
				TPArr n atp -> TPEArr n $ unwrapGeneric atp 
				_ -> tp' 
			else tp'
		def = Def{defMods = 
			DefModField : translateMods mods ++ [DefModStruct | str], defName = name, defType = tp'', 
			defBody = implicitConvertsion env tp'' i, defGenerics = Nothing, defPars = []}
		isLazy = D.DefModLazy `elem` mods
		lazyClass = classFind (envIndex env) "CNLazy"
		lazyGet = fromJust $ findDefWithName "get" lazyClass
		lazyConstr = fromJust $ classConstructor lazyClass
		lazyTp = TPClass TPMClass [wrapGeneric tp''] lazyClass
		defLazy = Def{defMods = [DefModField, DefModPrivate], defName = "_lazy_" ++ name, 
			defType = lazyTp, 
			defBody = Dot (callRef (objectDef lazyClass)) (Call lazyConstr lazyTp [(head $ defPars lazyConstr, Lambda [] (Return True i) tp'')]), 
			defGenerics = Nothing, defPars = []}
		defLazyGet = Def{defMods = DefModInline : DefModDef : translateMods mods, defName = name, 
			defType = tp'', 
			defBody = Return True $ Dot (callRef defLazy) (Call lazyGet tp'' []), defGenerics = Nothing, defPars = []}
		in return $ if isLazy then [defLazyGet, defLazy] else [def]

		

translateMods :: [D.DefMod] -> [DefMod]
translateMods = mapMaybe m
	where 
		m D.DefModStatic = Just DefModStatic
		m D.DefModMutable = Just DefModMutable
		m D.DefModPrivate = Just DefModPrivate
		m D.DefModProtected = Just DefModProtected
		m D.DefModWeak = Just DefModWeak
		m _ = Nothing
		
linkDef :: Bool -> Env -> D.ClassStm -> Def
linkDef str env ccc = evalState (stateDef ccc) env'
	where 
		env' = envAddClasses generics' env
		generics' = map (linkGeneric env) (D.defGenerics ccc)
		cl = envSelfClass env
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
				 overrideDef :: Maybe Def
				 overrideDef = listToMaybe $ mapMaybe findThisDef (superClasses cl)
				 findThisDef c = find eqDef (classDefs c) 
				 eqDef d = defName d == name && length (defPars d) == length opars && all eqPar (zip (defPars d) pars)
				 needWrapRetType = fromMaybe False $ fmap (isTpGeneric . defType) overrideDef
				 in 
				(case body of
					D.Nop -> return Def {defMods = DefModDef : DefModAbstract : mods' ++ [DefModStruct | str], defName = name, defGenerics = defGenerics',
							defPars = pars',
							defType = dataType (envIndex env') (fromMaybe (D.DataType "void" []) tp), defBody = Nop} 
					_   -> do 
						modify $ envAddVals pars'
						b <- expr body
						put env'
						let tp' = unwrapGeneric $ getDataType env' tp b
						let tp'' = if needWrapRetType then wrapGeneric tp' else tp'
						return Def {defMods = DefModDef : mods' ++ [DefModStruct | str], defName = name, defGenerics = defGenerics',
							defPars = pars',
							defType = tp'', defBody = maybeAddReturn env tp'' b})

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

data DataType = TPNumber Bool Int | TPFloatNumber Int | TPString | TPVoid 
	| TPClass {tpMod :: DataTypeMod, tpGenerics :: [DataType], tpClass :: Class}
	| TPEArr Int DataType | TPAny
	| TPArr Int DataType | TPBool | TPFun DataType DataType | TPTuple [DataType] | TPSelf | TPUnknown String 
	| TPMap DataType DataType
	| TPOption DataType | TPGenericWrap DataType | TPNil | TPObject {tpMod :: DataTypeMod, tpClass :: Class} | TPThrow
	| TPAnyGeneric | TPVoidRef
	deriving (Eq)
data DataTypeMod = TPMClass | TPMStruct | TPMEnum | TPMTrait | TPMGeneric | TPMType deriving (Eq)

byte :: DataType
byte = TPNumber False 1
ubyte :: DataType
ubyte = TPNumber True 1
int4 :: DataType
int4 = TPNumber False 4
uint4 :: DataType
uint4 = TPNumber True 4
int :: DataType
int = TPNumber False 0
uint :: DataType
uint = TPNumber True 0
int8 :: DataType
int8 = TPNumber False 8
uint8 :: DataType
uint8 = TPNumber True 8
float :: DataType
float = TPFloatNumber 0
float4 :: DataType
float4 = TPFloatNumber 4
float8 :: DataType
float8 = TPFloatNumber 8

isVoid :: DataType -> Bool
isVoid TPVoid = True
isVoid _ = False
isTpFun :: DataType -> Bool
isTpFun TPFun{} = True
isTpFun _ = False
isTpGeneric :: DataType -> Bool
isTpGeneric (TPClass TPMGeneric _ _) = True
isTpGeneric _ = False

forDataType :: MonadPlus m => (DataType -> m a) -> DataType -> m a
forDataType f tp = mplus (go tp) (f tp)
	where
		go (TPClass _ gens _) = msum $ map (forDataType f) gens
		go (TPArr _ a) = forDataType f a
		go (TPEArr _ a) = forDataType f a
		go (TPFun a b) = mplus (forDataType f a) (forDataType f b)
		go (TPMap a b) = mplus (forDataType f a) (forDataType f b)
		go (TPGenericWrap a) = forDataType f a
		go (TPOption a) = forDataType f a
		go (TPTuple a) =  msum $ map (forDataType f) a
		go _ = mzero


mapDataType :: (DataType -> Maybe DataType) -> DataType -> DataType
mapDataType f tp = fromMaybe (go tp) (f tp)
	where
		go (TPClass mods gens cl) = TPClass mods (map (mapDataType f) gens) cl
		go (TPArr m a) = TPArr m (mapDataType f a)
		go (TPEArr m a) = TPEArr m (mapDataType f a)
		go (TPFun a b) = TPFun (mapDataType f a) (mapDataType f b)
		go (TPMap a b) = TPMap (mapDataType f a) (mapDataType f b)
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
	| isType cl = TPMType
	| otherwise = TPMClass

dataTypeClassRef :: Env -> DataType -> ClassRef
dataTypeClassRef env tp = let 
		cl = dataTypeClass env tp
	in (cl, buildGenerics cl $ dataTypeGenerics env tp)

dataTypeClass :: Env -> DataType -> Class
dataTypeClass _ (TPClass _ _ c ) = c
dataTypeClass _ (TPObject _ c) = Class { classMods = [ClassModObject], className = className c, classExtends = extendsNothing, 
	classDefs = allDefsInObject (c, M.empty), classGenerics = []}
dataTypeClass env (TPGenericWrap c) = dataTypeClass env c
dataTypeClass env (TPArr _ _) = classFind (envIndex env) "CNSeq"
dataTypeClass env (TPEArr _ _) = classFind (envIndex env) "CNPArray"
dataTypeClass env (TPOption _) = classFind (envIndex env) "CNOption"
dataTypeClass env (TPMap _ _) = classFind(envIndex env) "CNMap"
dataTypeClass env (TPTuple [_, _]) = classFind (envIndex env) "CNTuple"
dataTypeClass env (TPNumber False 1) = classFind (envIndex env) "ODByte"
dataTypeClass env (TPNumber True 1) = classFind (envIndex env) "ODUByte"
dataTypeClass env (TPNumber False 0) = classFind (envIndex env) "ODInt"
dataTypeClass env (TPNumber True 0) = classFind (envIndex env) "ODUInt"
dataTypeClass env (TPNumber False 4) = classFind (envIndex env) "ODInt4"
dataTypeClass env (TPNumber True 4) = classFind (envIndex env) "ODUInt4"
dataTypeClass env (TPNumber False 8) = classFind (envIndex env) "ODInt8"
dataTypeClass env (TPNumber True 8) = classFind (envIndex env) "ODUInt8"
dataTypeClass env (TPFloatNumber 4) = classFind (envIndex env) "ODFloat4"
dataTypeClass env (TPFloatNumber 8) = classFind (envIndex env) "ODFloat8"
dataTypeClass env (TPFloatNumber 0) = classFind (envIndex env) "ODFloat"
dataTypeClass env TPAny = classFind (envIndex env) "ODAny"
dataTypeClass env (TPTuple a) = classFind (envIndex env) ("CNTuple" ++ show (length a))
dataTypeClass _ (TPFun stp dtp) = Class { classMods = [], className = "", classExtends = extendsNothing, 
	classDefs = [apply], classGenerics = []}
	where
		sourceTypes = case stp of
			TPVoid -> []
			TPTuple tps -> tps
			tp -> [tp]
		apply = Def {defName = "apply", defPars = map (localVal "") sourceTypes, defType = dtp, defBody = Nop, defMods = [DefModApplyLambda], defGenerics = Nothing}
dataTypeClass _ x = classError (show x) ("No dataTypeClass for " ++ show x)

dataTypeClassName :: DataType -> String
dataTypeClassName (TPClass _ _ c ) = className c
dataTypeClassName (TPObject _ c) = className c
dataTypeClassName (TPGenericWrap c) = dataTypeClassName c
dataTypeClassName (TPArr _ _) = "CNArray"
dataTypeClassName (TPOption _) = "CNOption"
dataTypeClassName (TPMap _ _) = "CNMap"
dataTypeClassName TPAny = "ODAny"
dataTypeClassName (TPTuple [_, _]) = "CNTuple"
dataTypeClassName (TPTuple a) = "CNTuple" ++ show (length a)
dataTypeClassName x = error ("No dataTypeClassName for " ++ show x)

resolveTypeAlias :: DataType -> DataType
resolveTypeAlias tp@(TPClass TPMType _ _) = fromJust $ superType tp
resolveTypeAlias tp = tp

dataTypeGenerics :: Env -> DataType -> [DataType]
dataTypeGenerics _ (TPClass _ g _) = g
dataTypeGenerics _ (TPArr _ g) = [g]
dataTypeGenerics _ (TPEArr _ g) = [g]
dataTypeGenerics _ (TPMap k v) = [k, v]
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
	"byte" -> byte
	"ODByte" -> TPGenericWrap byte
	"ubyte" -> ubyte
	"ODUByte" -> TPGenericWrap ubyte
	"int" -> int
	"ODInt" -> TPGenericWrap int
	"uint" -> uint
	"ODUInt" -> TPGenericWrap uint
	"int4" -> int4
	"ODInt4" -> TPGenericWrap int4
	"uint4" -> uint4
	"ODUInt4" -> TPGenericWrap uint4
	"int8" -> int8
	"ODInt8" -> TPGenericWrap int8
	"uint8" -> uint8
	"ODUInt8" -> TPGenericWrap uint8
	"float4" -> float4
	"ODFloat4" -> TPGenericWrap float4
	"float8" -> float8
	"ODFloat8" -> TPGenericWrap float8
	"float" -> float
	"ODFloat" -> TPGenericWrap float
	"void" -> TPVoid
	"string" -> TPString
	"bool" -> TPBool
	"self" -> TPSelf
	"VoidRef" -> TPVoidRef
	"any" -> TPAny
	"_" -> TPAnyGeneric
	_ -> maybe (TPUnknown $ "No class found " ++ name) (\cl -> refDataType cl (map (wrapGeneric . dataType cidx) gens)) (idxFind cidx name)
dataType cidx (D.DataTypeArr m tp) = case tp' of
		TPClass TPMStruct _ _ ->arrr'
		TPNumber _ _ -> earr
		TPFloatNumber _  -> earr
		TPBool -> earr
		TPVoid -> earr
		_ -> arrr
	where
		tp' = dataType cidx tp
		arrr = TPArr m $ wrapGeneric tp'
		arrr' =  if m == 0 then arrr else earr
		earr = TPEArr m tp'
dataType cidx (D.DataTypeMap k v) = TPMap (wrapGeneric $ dataType cidx k) (wrapGeneric $ dataType cidx v)
dataType cidx (D.DataTypeFun (D.DataTypeTuple tps) d) = TPFun (TPTuple $ map (dataType cidx) tps) (dataType cidx d)
dataType cidx (D.DataTypeFun s d) = TPFun (dataType cidx s) (dataType cidx d)
dataType cidx (D.DataTypeTuple tps) = TPTuple $ map (wrapGeneric . dataType cidx) tps
dataType cidx (D.DataTypeOption t) = TPOption $ (wrapGeneric . dataType cidx) t


instance Show DataType where
	show (TPNumber False 1) = "byte"
	show (TPNumber True 1) = "ubyte"
	show (TPNumber False 4) = "int4"
	show (TPNumber True 4) = "uint4"
	show (TPNumber False 0) = "int"
	show (TPNumber False 8) = "int8"
	show (TPNumber True 0) = "uint"
	show (TPNumber True 8) = "uint8"
	show (TPFloatNumber 4) = "float4"
	show (TPFloatNumber 0) = "float"
	show (TPFloatNumber 8) = "float8"
	show TPVoid = "void"
	show TPString = "string"
	show TPBool = "bool"
	show TPSelf = "self"
	show TPNil = "nil"
	show TPThrow = "throw"
	show TPAnyGeneric = "_"
	show TPVoidRef = "VoidRef"
	show TPAny = "any"
	show (TPUnknown s) = s
	show (TPClass t [] c) = className c ++ show t
	show (TPObject t c) = className c ++ show t ++ ".class"
	show (TPClass t gens c) = className c ++ show t ++ "<" ++ strs' ", " gens ++ ">"
	show (TPGenericWrap c) = '^' : show c
	show (TPArr 0 t) = "[" ++ show t ++ "]"
	show (TPArr s r) = show r ++ "[" ++ show s ++ "]"
	show (TPEArr s r) = "*" ++ show r ++ "[" ++ show s ++ "]"
	show (TPMap k v) = "[" ++ show k ++ " : " ++ show v ++ "]"
	show (TPFun s d) = show s ++ " -> " ++ show d
	show (TPTuple tps) = "(" ++ strs' ", " tps ++ ")"
	show (TPOption t) = show t ++ "?"
	show _ =  "UnknownTP"
instance Show DataTypeMod where
	show TPMClass = "#C"
	show TPMType = "#P"
	show TPMStruct = "#S"
	show TPMEnum = "#E"
	show TPMTrait = "#T"
	show TPMGeneric = "#G"
	
getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType (envIndex env)) tp


tpGeneric :: DataType
tpGeneric = TPClass TPMGeneric [] (Generic "?" [] extendsNothing [])

objectType :: DataType -> DataType
objectType (TPClass t _ cl) = TPObject t cl
objectType e = TPUnknown $ "No object type for type " ++ show e

reduceTypes :: Env -> [DataType] -> [DataType]
reduceTypes env tps = foldl1 (\r x -> r >>= commonType env (head x) ) $ map (\r -> [r]) tps

commonType :: Env -> DataType -> DataType -> [DataType]
commonType env tp1 tp2 = fmap (\ (cl, gens) -> refDataType cl (map snd $ M.toList gens) ) $ 
	commonClassRef (dataTypeClassRef env tp1) (dataTypeClassRef env tp2)

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
	| Super DataType
	| Nil
	| BoolOp BoolTp Exp Exp
	| MathOp MathTp Exp Exp
	| PlusPlus Exp
	| MinusMinus Exp
	| Dot Exp Exp
	| Set (Maybe MathTp) Exp Exp
	| Call Def DataType [CallPar]
	| Return Bool Exp
	| Index Exp Exp
	| Lambda [(String, DataType)] Exp DataType
	| Val Def
	| ExpDError String D.Exp 
	| ExpLError String Exp 
	| ExpError String 
	| FirstTry D.Exp Exp 
	| Arr [Exp]
	| Map [(Exp, Exp)]
	| Tuple [Exp]
	| Opt Exp
	| Some Exp
	| None DataType
	| Throw Exp
	| Not Exp
	| Negative Exp
	| Cast DataType Exp
	| As DataType
	| Is DataType
	| CastDot DataType
	| Break
	| LambdaCall Exp
	| StringBuild [(String, Exp)] String
	
instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show (While cond e) = "while(" ++ show cond ++ ") " ++ show e
	show (Do cond e) = "do" ++ show e ++ " while(" ++ show cond ++ ")"
	show Nop = ""
	show (Self c) = "<" ++ show c ++ ">self"
	show (Super c) = "<" ++ show c ++ ">super"
	show (Return _ e) = "return " ++ show e
	show (Set Nothing l r) = showOp l "=" r
	show (Set (Just t) l r) = showOp l (show t ++ "=") r
	show (BoolOp t l r) = showOp l (show t) r
	show (MathOp t l r) = showOp l (show t) r
	show (PlusPlus e) = show e ++ "++"
	show (MinusMinus e) = show e ++ "--"
	show (Dot l r) = showOp' l "." r
	show (Call dd tp pars) = defRefPrep dd ++ defName dd ++ showCallPars pars ++ "\\" ++ show tp ++ "\\" 
	show (IntConst i) = show i
	show (StringConst i) = show i
	show Nil = "nil"
	show (BoolConst i) = show i
	show (FloatConst i) = show i
	show (Index e i) = show e ++ "[" ++ show i ++ "]"
	show (Lambda pars e tp) = strs ", " (map (\(n, t) -> n ++ " : " ++ show t) pars) ++ " -> " ++ show tp ++ " = " ++ show e
	show (Val d) = show d
	show (ExpDError s e) = "<#" ++ show e ++ ": " ++ (strs " " . lines) s ++ "#>"
	show (ExpError s) = "<#" ++ (strs " " . lines) s ++ "#>"
	show (ExpLError s e) = "<#" ++ show e ++ ": " ++ (strs " " . lines) s ++ "#>"
	show (Arr exps) = "["  ++ strs' ", " exps ++ "]"
	show (Map exps) = "["  ++ strs' ", " exps ++ "]"
	show (Tuple exps) = "("  ++ strs' ", " exps ++ ")"
	show (Opt e) = "opt(" ++ show e ++ ")"
	show (Some e) = "some(" ++ show e ++ ")"
	show (None tp) = "none<" ++ show tp ++ ">" 
	show (FirstTry _ e) = "First try: " ++ show e
	show (Throw e) = "throw " ++ show e
	show (Not e) = "!(" ++ show e ++ ")"
	show (Negative e) = '-' : show e
	show (Cast tp e) = show e ++ ".cast<" ++ show tp ++ ">"
	show (As tp) = "as<" ++ show tp ++ ">"
	show (Is tp) = "is<" ++ show tp ++ ">"
	show (CastDot tp) = "cast<" ++ show tp ++ ">"
	show (Break) = "break"
	show (LambdaCall e) = show e ++ "()"
	show (StringBuild pars lastS) = "\"" ++ join (map (\(prev, e) -> prev ++ "$" ++ show e) pars) ++ lastS ++ "\""

callRef :: Def -> Exp
callRef d = Call d (defType d) []

showCallPars :: [CallPar] -> String
showCallPars [] = ""
showCallPars pars = "(" ++ strs ", " (map showPar pars) ++ ")"
	where showPar (Def {defName = name}, e) = name ++ " = " ++ show e
	
callLocalVal :: String -> DataType -> Exp
callLocalVal name tp = Call (localVal name tp) tp []

maybeAddReturn :: Env -> DataType -> Exp -> Exp
maybeAddReturn _ TPVoid e = e
maybeAddReturn env tp e = addReturn env True tp e

addReturn :: Env -> Bool -> DataType -> Exp -> Exp
addReturn env hard tp (If cond t f) = If cond (addReturn env hard tp t) (addReturn env hard tp f)
addReturn _ True _ e@(Braces []) = ExpLError "Return empty braces" e
addReturn env True tp (Braces es) = Braces $ map (addReturn env False tp) (init es) ++ [addReturn env True tp (last es)]
addReturn _ True _ Nop = ExpLError "Return NOP" Nop
addReturn _ _ _ e@(Throw _) = e
addReturn env _ tp (Return _ e) = Return True $ implicitConvertsion env tp e
addReturn env True tp e = Return False $ implicitConvertsion env tp e
addReturn _ _ _ e = e

forExp :: MonadPlus m => (Exp -> m a) -> Exp -> m a
forExp f ee = mplus (go ee) (f ee)
	where
		go (Braces es) = msum $ map (forExp f) es
		go (StringBuild pars _) = msum $ map (forExp f . snd) pars
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
		go (Return _ e) = forExp f e
		go (Opt e) = forExp f e
		go (Some e) = forExp f e
		go (Throw e) = forExp f e
		go (Not e) = forExp f e
		go (Negative e) = forExp f e
		go (Lambda _ e _) = forExp f e
		go (FirstTry _ e) = forExp f e
		go (Val d) = forExp f (defBody d)
		go _ = mzero

isConst :: Exp -> Bool
isConst (IntConst _) = True 
isConst (StringConst _) = True 
isConst (BoolConst _) = True 
isConst (FloatConst _) = True 
isConst Nil = True 
isConst (Tuple exps) = all isConst exps
isConst (StringBuild exps _) = all (isConst . snd) exps
isConst (Arr exps) = all isConst exps
isConst (Map exps) = all (\(a, b) -> isConst a && isConst b) exps
isConst (Cast _ e) = isConst e
isConst (Dot l r) = isConst l && isConst r
isConst (Call Def {defMods = mods} _ pars) = 
	(DefModStruct `elem` mods  &&  DefModConstructor `elem` mods && all (isConst . snd) pars)
	|| (DefModObject `elem` mods && null pars)
isConst (As _) = True
isConst (Is _) = True
isConst (CastDot _) = True
isConst _ = False


exprDataType :: Exp -> DataType
exprDataType (If _ _ Nop) = TPVoid
exprDataType (If _ t _) = exprDataType t
exprDataType (While _ _) = TPVoid
exprDataType (Do _ _) = TPVoid
exprDataType (Braces []) = TPVoid
exprDataType (Braces es) = exprDataType $ last es
exprDataType (Nop) = TPVoid
exprDataType (IntConst _ ) = int
exprDataType (StringConst _ ) = TPString
exprDataType Nil = TPNil
exprDataType (BoolConst _ ) = TPBool
exprDataType (FloatConst _) = float
exprDataType (BoolOp {}) = TPBool
exprDataType (MathOp _ l r) = case(unwrapGeneric $ exprDataType l, unwrapGeneric $ exprDataType r) of
	(TPNumber _ _, rtp@TPFloatNumber{}) -> rtp
	(lt, _) -> lt
exprDataType (PlusPlus e) = exprDataType e
exprDataType (MinusMinus e) = exprDataType e
exprDataType (Dot _ b) = exprDataType b
exprDataType (Set _ a _) = exprDataType a
exprDataType (Self s) = s
exprDataType (Super s) = s
exprDataType (Call _ t _) = t
exprDataType (Return _ e) = exprDataType e
exprDataType (Index e i) = resolve $ exprDataType e 
	where  
		resolve (TPArr _ t) = t
		resolve (TPMap _ v) = TPOption v
		resolve (TPObject TPMEnum c) = TPClass TPMEnum [] c
		resolve (TPGenericWrap t) = resolve t
		resolve t = TPUnknown $ show t ++ " is not array " ++ show e ++ "[" ++ show i ++ "]"
exprDataType (Lambda pars _ r) = TPFun (parsTp pars) r
	where 
		parsTp :: [(String, DataType)] -> DataType
		parsTp [(_, tp)] = tp
		parsTp ps = TPTuple $ map snd ps
exprDataType e@(ExpDError _ _) = TPUnknown $ show e
exprDataType e@(ExpError _) = TPUnknown $ show e
exprDataType e@(ExpLError _ _) = TPUnknown $ show e
exprDataType (Arr []) = TPArr 0 TPVoid
exprDataType (Map []) = TPMap TPVoid TPVoid
exprDataType (Arr exps) = TPArr (length exps) $ wrapGeneric $ exprDataType $ head exps
exprDataType (Map exps) = let (k, v) = ((exprDataType >>> wrapGeneric) *** (exprDataType >>> wrapGeneric)) $ head exps 
	in TPMap k v
exprDataType (Tuple exps) = TPTuple $ map (wrapGeneric .exprDataType) exps
exprDataType (Val Def{defType = tp}) = tp
exprDataType (Opt v) = TPOption (exprDataType v)
exprDataType (Some v) = TPOption (exprDataType v)
exprDataType (None tp) = tp
exprDataType (FirstTry _ e) = exprDataType e
exprDataType (Throw _) = TPThrow
exprDataType (Not _) = TPBool
exprDataType (Negative e) = exprDataType e
exprDataType (Cast dtp _) = dtp
exprDataType (As dtp) = TPOption $ wrapGeneric dtp
exprDataType (CastDot dtp) = dtp
exprDataType (Is _) = TPBool
exprDataType Break = TPVoid
exprDataType StringBuild{} = TPString
exprDataType (LambdaCall e) = case unwrapGeneric $ exprDataType e of
	(TPFun _ d) -> d
	t -> t
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
		D.Call {} -> return $ exprCall env Nothing a
		_ -> expr a
	case aa of
		ExpDError s _ -> return $ ExpDError s d
		_ -> do
			bb <- return $ exprCall env (Just $ exprDataType aa)  b
			put env
			return $ case bb of
				Dot l r -> Dot (Dot aa l) r
				_ -> Dot aa bb
expr (D.Set tp a b) = do
	aa <- expr a
	bb <- expr b
	env <- get
	return $ Set tp aa (implicitConvertsion env (exprDataType aa) bb)
expr (D.PlusPlus e) = do
	aa <- expr e
	return $ PlusPlus aa
expr (D.MinusMinus e) = do
	aa <- expr e
	return $ MinusMinus aa
expr D.Self = do
	env <- get
	return $ Self $ envSelf env
expr D.Super = do
	env <- get
	return $ Super $ fromMaybe (error "No super data type") $ superType $ envSelf env
expr r@D.Call{} = get >>= (\env -> return $ exprCall env Nothing r)
expr (D.Index e i) = do
	e' <- expr e
	let obf = expr $ D.Dot e (D.Call "apply" (Just [(Nothing, i)]) [])
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
	return $ Lambda pars' (maybeAddReturn env tp e') tp)
	else return $ ExpDError "Not all types are defined in lambda" l

expr (D.Val name tp body mods) = do
	env <- get 
	body' <- expr body
	let tp' = unwrapGeneric $ maybe (exprDataType body') (dataType $ envIndex env) tp
	let mods' = DefModLocal : [DefModMutable | D.DefModMutable `elem` mods]
	let def' = Def{defName = name, defType = tp', defMods = mods', defPars = [], 
		defBody = implicitConvertsion env tp' body', 
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
	return $ Return True e'
expr (D.Not e) = do
	e' <- expr e
	return $ Not e'
expr (D.Negative e) = do
	e' <- expr e
	return $ Negative e'
expr D.Break = return Break
expr c@D.Case{} = linkCase c
expr s@D.StringBuild {} = do
	env <- get 
	return $ linkStringBuild env s
{- expr x = error $ "No expr for " ++ show x -}

{------------------------------------------------------------------------------------------------------------------------------ 
 - String build
 ------------------------------------------------------------------------------------------------------------------------------}
linkStringBuild :: Env -> D.Exp -> Exp
linkStringBuild _ (D.StringBuild [] lastString) =  StringConst lastString
linkStringBuild env (D.StringBuild pars lastString) = 
	let 
		processPart :: String -> (String, D.Exp) -> (String, (Exp, String))
		processPart next (prev, e) = (modPrev e prev, (compile prev e next, modNext e next))
		modPrev :: D.Exp -> String -> String
		modPrev (D.Call "when" (Just [_]) _) s = (reverse . tail . dropWhile ( /= '\n') . reverse) s
		modPrev _ s = s
		modNext :: D.Exp -> String -> String
		modNext _ s = s
		compile :: String -> D.Exp -> String -> Exp
		compile prev (D.Call "when" (Just [(_, e)]) _) _ = 
			If (evalState (expr e) env) 
				(StringConst $ '\n':(reverse . takeWhile ( /= '\n') . reverse) prev) 
				(StringConst "")
		compile _ e _ = evalState (expr e) env
		accumr :: (String, [(Exp, String)])
		accumr = mapAccumR processPart lastString pars
		accuml :: (String, [(String, Exp)])
		accuml = mapAccumL (\prev (e, next) -> (next, (prev, e)) ) (fst accumr) (snd accumr)
	in StringBuild (snd accuml) (fst accuml)



{------------------------------------------------------------------------------------------------------------------------------ 
 - Pattern matching
 ------------------------------------------------------------------------------------------------------------------------------}

data CaseEnv = CaseEvn{caseEnvEnv :: Env, caseEnvCurrentVal :: Def, caseEnvValNum :: Int, caseEnvDefs :: [Def]}
 
caseEnvIncVal :: CaseEnv -> CaseEnv
caseEnvIncVal env = env {caseEnvValNum = caseEnvValNum env + 1}
caseEnvAddDef :: Def -> CaseEnv -> CaseEnv
caseEnvAddDef d env = env {caseEnvDefs = caseEnvDefs env ++ [d]}

linkCase :: D.Exp -> State Env Exp
linkCase (D.Case mainExpr items) = do
	mainExpr' <- expr mainExpr
	let 
		_case = (localVal "__case__" (exprDataType mainExpr')) {defBody = mainExpr'}
	 	_incomplete = (localVal "__incomplete__" TPBool) {defBody = BoolConst True}
	 	_ok = (localVal "__ok__" TPBool) {defBody = BoolConst True}
	 	notOk = Set Nothing (callRef _ok) (BoolConst False)
	 	isOk = callRef _ok
		_result = (localVal "__result__" TPVoid)
		caseEnvVal env = "__case" ++ show (caseEnvValNum env) ++ "__"
		linkCaseItem :: D.CaseItem -> State Env (Exp, DataType)
		linkCaseItem (cond, e) = do
			env <- get
			let 
				(ex, caseEnv) = runState (linkCaseCond cond) $ CaseEvn env _case 1 []
				caseDefs = caseEnvDefs caseEnv
				vars = map Val caseDefs
			modify (envAddVals caseDefs)
			itemExpr <- expr e
			put env
			let
				setResultTo to = Set Nothing (callRef _result) to
				setResult = case itemExpr of
					Braces exprs -> init exprs ++ [setResultTo $ last exprs]
					ee -> [setResultTo ee]

			return $ (If (callRef _incomplete) (Braces $ 
				Val _ok : vars ++ [
					ex, 
					If isOk (Braces $ setResult ++ [Set Nothing (callRef _incomplete) (BoolConst False)])
						Nop]) Nop, exprDataType itemExpr)
		linkCaseCond :: D.CaseCondition -> State CaseEnv Exp
		linkCaseCond (D.CaseUnapply _ "" pars) = do
			caseEnv <- get
			let
				val = caseEnvCurrentVal caseEnv
				valTp = defType val
				env = caseEnvEnv caseEnv
				valCl = dataTypeClass env valTp
				constr = fromMaybe (error $ "No constructor") $ classConstructor valCl

				linkPar :: (Def, D.CaseCondition) -> State CaseEnv [Exp]
				linkPar (_, D.CaseAny) = return []
				linkPar (d@Def{defType = newTp}, D.CaseVal valName)= do
					let newVal = localVal valName newTp
					modify $ caseEnvAddDef newVal
					return [Set Nothing (callRef newVal) $Dot (callRef val) (callRef d)]
				linkPar (d@Def{defType = newTp}, cond)= do
					let newVal = localVal (caseEnvVal caseEnv) newTp
					modify caseEnvIncVal
					e <- get
					put e{caseEnvCurrentVal = newVal}
					cond' <- linkCaseCond cond
					e' <- get
					put e'{caseEnvCurrentVal = val}
					return $ (Val $ newVal{defBody = Dot (callRef val) (callRef d)}) : [cond']
			pars' <- mapM linkPar $ zip (defPars constr) pars
			return $ Braces $ join pars'

		linkCaseCond (D.CaseUnapply _ ref pars) = do
			caseEnv <- get
			let 
				val = caseEnvCurrentVal caseEnv
				env = caseEnvEnv caseEnv
				tp = dataType (envIndex env) $ D.DataType ref []
				cl = dataTypeClass env tp
				newValOpt = localVal  ("__caseOpt" ++ show (caseEnvValNum caseEnv) ++ "__") newTpOpt
				newTpOpt = maybe (TPUnknown "Not found unapply") defType unapply
				newVal = localVal  (caseEnvVal caseEnv) newTp
				newTp = case newTpOpt of
					TPOption t -> t
					TPUnknown _ -> tp
					t -> t

				allUnappies = filter ( ("unapply" == ). defName) $ filter ( (DefModStatic `elem`) .defMods) $ classDefs cl
				unapplyCall :: [Exp] -> Exp
				unapplyCall next = maybe (buildIf next) (buildCall next) unapply
				buildCall next f@Def{defType = ftp, defPars = [fpar]} = Braces $
					[Val $ newValOpt{defBody = Dot (callRef (objectDef cl)) (Call f ftp [(fpar, Cast tp $ callRef val)])},
					If (callFromValOpt "isDefined") 
						(Braces $ (Val $ newVal{defBody = callFromValOpt "get"}): next)
						notOk 
						]
				buildIf next = If (Dot (callRef val) (Is tp)) (Braces $
					(Val $  newVal{defBody = Dot (callRef val) (CastDot tp)}) : next)
					notOk
				unapply :: Maybe Def
				unapply = find (parsTypeIs (defType val)) allUnappies
				parsTypeIs dtp def = case defPars def of
					[x] -> defType x == dtp
					_ -> False
				callFromValOpt fname = Dot (callRef newValOpt) (exprCall env (Just newTpOpt) (D.Call fname Nothing []))
				
				caseEnv' = caseEnv {caseEnvCurrentVal = newVal, caseEnvValNum = caseEnvValNum caseEnv + 1}
			put caseEnv'
			tupleCond <- linkCaseCond (D.CaseUnapply Nothing "" pars)
			modify (\e -> e{caseEnvCurrentVal = val})
			return $ unapplyCall [tupleCond]

		linkCaseCond D.CaseAny = return Nop
		linkCaseCond  (D.CaseVal name) = do
			caseEnv <- get
			let 
				val = caseEnvCurrentVal caseEnv
				ret = localVal name $ defType val
				caseEnv' = caseEnvAddDef ret caseEnv
			put caseEnv'
			return $ Set Nothing (callRef ret) (callRef val)
	items' <- mapM linkCaseItem items
	env <- get
	let _result' = _result {defType = fromMaybe (TPUnknown "No common type for case") $  listToMaybe $ reduceTypes env $ map snd items'}
	return $ Braces $ [
		Val _case, Val _incomplete, Val _result'] 
		++ map fst items' 
		++ [If (callRef _incomplete) (Throw $ StringConst "Case incomplete") Nop, 
			callRef _result']


{------------------------------------------------------------------------------------------------------------------------------ 
 - Calling 
 ------------------------------------------------------------------------------------------------------------------------------}

exprCall :: Env-> Maybe DataType -> D.Exp -> Exp
exprCall _ (Just (TPUnknown t)) e = ExpDError t e
exprCall env (Just _) (D.Call "as" Nothing [tp]) = As $ dataType (envIndex env) tp
exprCall env (Just _) (D.Call "is" Nothing [tp]) = Is $ dataType (envIndex env) tp
exprCall env (Just _) (D.Call "cast" Nothing [tp]) = CastDot $ dataType (envIndex env) tp
exprCall env strictClass call@(D.Call name pars gens) = 
	case tryExprCall env strictClass call of
		err@ExpDError{} -> if isNothing pars then err else
			case tryExprCall env strictClass (D.Call name Nothing gens) of
				ExpDError es _ -> ExpLError es err
				e -> case tryExprCall env (Just $ exprDataType e) (D.Call "apply" pars gens) of
					ExpDError es _ -> ExpLError es err
					ee -> Dot e ee
		e -> e
		
exprCall _ _ err = ExpDError "It is not call" err

tryExprCall :: Env-> Maybe DataType -> D.Exp -> Exp
tryExprCall _ (Just (TPUnknown t)) e = ExpDError t e
tryExprCall env strictClass call@(D.Call name pars gens) = call'''
	where
		pars' = evalState (mapM (\ (n, e) ->  expr e >>= (\ ee -> return (n, FirstTry e ee))) (fromMaybe [] pars)) env
		self = fromMaybe (envSelf env) strictClass
		call' :: Exp
		call' = fromMaybe (ExpDError errorString call) $ findCall
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

		call''' = case pars of
			Just [] -> case exprDataType call'' of
				t@(TPFun TPVoid _) -> LambdaCall (Cast t call'')
				TPGenericWrap t@(TPFun TPVoid _) -> LambdaCall (Cast t call'')
				_ -> call''
			_ -> call''

		pars'' :: [(Def, Exp)]
		pars'' = case call' of
			Call _ _ r -> r
		pars''' :: [(Def, Exp)]
		pars''' = (map (correctCallPar env gens') pars'')
		
		gens' :: Generics
		gens' = resolveGenerics pars''

		gens'' :: Generics
		gens'' = resolveGenerics pars'''

		

		resolveGenerics :: [(Def, Exp)] -> Generics
		resolveGenerics rpars = let
			extendList :: Int -> [a] -> [Maybe a]
			extendList l a
				| length a == l = map Just a
				| length a > l = (map Just . take l) a
				| otherwise = map Just a ++ replicate (l - length a) Nothing 
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
					tryDetermine c (TPMap a b, TPMap a' b') = mplus (tryDetermine c (a, a')) (tryDetermine c (b, b'))
					tryDetermine c (TPTuple a, TPTuple a') = listToMaybe $ mapMaybe (tryDetermine c) (zip a a')
					tryDetermine c (TPClass _ gg cl, TPClass _ gg' cl') = 
						listToMaybe $ mapMaybe (tryDetermine c) (zip gg gg'')
						where
							gg'' = map snd $ M.toList $ fromMaybe M.empty $ upGenericsToClass cl (cl', buildGenerics cl' gg')
					tryDetermine c (cl@(TPClass _ _ _), TPObject m cl') = 
						tryDetermine c (cl, TPClass TPMClass [TPClass m [] cl'] (classFind (envIndex env) "ODClass"))
					tryDetermine c (TPFun a b, TPFun a' b') = listToMaybe $ catMaybes [tryDetermine c (a, a'), tryDetermine c (b, b')]
					tryDetermine c (TPGenericWrap a, TPGenericWrap b) = tryDetermine c (a, b)
					tryDetermine c (TPGenericWrap a, b) = tryDetermine c (a, b)
					tryDetermine c (a, TPGenericWrap b) = tryDetermine c (a, b)
					tryDetermine c (a, b@TPArr{}) = tryDetermine c (a, dtpw b)
					tryDetermine c (a, b@TPMap{}) = tryDetermine c (a, dtpw b)
					tryDetermine _ _ = Nothing
					dtpw tp = TPClass TPMGeneric (dataTypeGenerics env tp) (dataTypeClass env tp) 
			in case call' of
				(Call Def{defGenerics = Just defGens} _ _) -> 
					M.fromList $ ddefGenerics defGens ++ dclassGenerics
				_ -> M.fromList dclassGenerics 
								
		errorString :: String
		errorString = "Could find reference for call " ++ callStr ++ "\n" ++
			maybe "" (\cl -> "strict in class " ++ show cl ++ "\n") strictClass ++
			(if detailedReferenceError then "in defs:\n" ++
			(strs "\n" . map (ind . showDef False)) (allDefs)  else "")
			where callStr = name ++ maybe "" (\ps -> "(" ++ strs ", " (map ((++ ":") . fromMaybe "" . fst) ps) ++ ")" ) pars

		correctCall :: Exp -> Exp
		correctCall (Call d tp _) = Call d (replaceGenerics gens'' tp) (map doImplicitConversation pars''')
			where
				doImplicitConversation (dd, e) = (dd, implicitConvertsion env (replaceGenerics gens'' $ defType dd) e)

		allDefs :: [Def]
		allDefs = maybe allDefsInEnv allDefsInStrictClass strictClass
			where 
			clRef = dataTypeClassRef env (fromJust strictClass)
			allDefsInStrictClass _ = 
				allDefsInClass clRef 
				++ allDefsInObject clRef
			allDefsInEnv = envVals env 
				++ allDefsInClass (dataTypeClassRef env $ envSelf env) 
				++ allDefsInObject (dataTypeClassRef env $ envSelf env) 
				++ envGlobalDefIndex env 
				++ objects 
			objects = if isNothing pars then (map (objectDef . snd) . M.toList) (envIndex env) else []
		
		findCall :: Maybe Exp
		findCall = listToMaybe $ (mapMaybe fit . filter (\d -> defName d == name)) allDefs
			where
				fit :: Def -> Maybe Exp
				fit d
					| length pars' == length (defPars d) = 
							if checkParameters pars' (defPars d) then Just $ def' d else Nothing
					| otherwise = Nothing

				def' d = Call d (resolveTp d) $  zipWith (\dp (_, e) -> (dp, e) ) (defPars' d) pars'
				resolveTp d = case defType d of
					TPSelf -> self
					tp@(TPFun _ dtp)-> if length pars' == length (defPars d) then tp else dtp
					tp -> tp
				defPars' :: Def -> [Def]
				defPars' Def{defType = t, defPars = []} = dataTypePars t
				defPars' Def{defPars = r} = r
				checkParameters :: [(Maybe String, Exp)] -> [Def] -> Bool
				checkParameters cp dp = all (checkParameter) $ zip cp dp
				checkParameter :: ((Maybe String, Exp), Def) -> Bool
				checkParameter ((Just cn, _), Def{defName = dn}) = cn == dn
				checkParameter _ = True




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
		dtp' = replaceGenerics gens dtp
		expr' = maybeAddReturn env dtp $ implicitConvertsion env dtp' $ evalState (expr lambdaExpr) env'
		tp' = if containsGeneric dtp then wrapGeneric (exprDataType expr') else dtp
		containsGeneric = fromMaybe False . forDataType (\t -> case t of
			TPClass TPMGeneric _ _ -> Just True
			_ -> Nothing)
correctCallPar _ _ e = e


{-----------------------------------------------------------------------------------------------------------------------------------------
 - Implicit conversion
 -----------------------------------------------------------------------------------------------------------------------------------------}

implicitConvertsion :: Env -> DataType -> Exp -> Exp
implicitConvertsion _ (TPMap _ _) (Arr []) = Map []
implicitConvertsion _ _ Nop = Nop
implicitConvertsion env dtp ex = let stp = exprDataType ex
	in 
		case ex of
			Braces _ -> maybeAddReturn env dtp ex
			_ -> if stp == dtp then ex else conv stp dtp
	where 
		conv (TPGenericWrap s) d = conv s d
		conv s (TPGenericWrap d) = conv s d
		conv TPFun{} TPFun{} = ex
		conv _ f@(TPFun _ fdtp) = Lambda (lambdaImplicitParameters f) (maybeAddReturn env fdtp ex) fdtp
		conv TPFun{} _ = LambdaCall ex
		conv TPOption{} TPOption{} = ex
		conv TPNil (TPOption tp) = None tp
		conv _ (TPOption _) = Opt ex
		conv _ (TPClass TPMGeneric _ _) = ex
		conv (TPNumber _ _) TPString = Cast TPString ex
		conv (TPFloatNumber _ ) TPString = Cast TPString ex
		conv (TPNumber s1 l1) d@(TPNumber s2 l2) = if s1 /= s2 || l1 /= l2 then Cast d ex else ex
		conv (TPFloatNumber l1) d@(TPFloatNumber l2) = if l1 /= l2 then Cast d ex else ex
		conv TPFloatNumber{} d@TPNumber{} = Cast d ex
		conv TPNumber{} d@TPFloatNumber{} = Cast d ex
		conv (TPArr _ _) d@(TPEArr _ _) = Cast d ex
		conv (TPTuple _) (TPTuple dtps) = case ex of
			Tuple exps -> Tuple $ zipWith (implicitConvertsion env) dtps exps
			_ -> ex
		conv (TPArr _ _) (TPArr _ adtp) = case ex of
			Arr exps -> Arr $ map (implicitConvertsion env adtp) exps
			_ -> ex
		conv (TPArr _ _) (TPClass _ [d] Class{className = "CNPArray"}) = Cast (TPEArr 0 (unwrapGeneric d)) ex
		conv TPClass{} TPVoidRef = Cast dtp ex
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
				checkApplyPars [Def{defType = tp}] = isInstanceOfTp env tp sc
				checkApplyPars _ = False
				od = objectDef cls
				wrapWithApply apply@Def{defPars = [par]} = Dot (Call od (defType od) []) (Call apply dtp [(par, e)])
		classConversion t sc _ = ExpLError (show t ++ " from " ++ show sc) ex

lambdaImplicitParameters :: DataType -> [(String, DataType)]
lambdaImplicitParameters (TPFun TPVoid _) = []
lambdaImplicitParameters (TPFun (TPTuple stps) _) = (map(\(tp, i) -> ('_' : show i, tp)) . zipWithIndex) stps
lambdaImplicitParameters (TPFun fstp _) = [("_", fstp)]

{-----------------------------------------------------------------------------------------------------------------------------------------
 - Errors
 -----------------------------------------------------------------------------------------------------------------------------------------}

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
		checkErrorsInExtends extends
		++ concatMap checkErrorsInClass gens
		++ concatMap checkErrorsInDef defs
	)

checkErrorsInExtends :: Extends -> [Error]
checkErrorsInExtends (Extends cls traits) =
	concatMap checkErrorsInExtendsRef traits
	++ maybe [] checkErrorsInExtendsClass cls

checkErrorsInExtendsClass :: ExtendsClass -> [Error]
checkErrorsInExtendsClass (ExtendsClass ref pars) = 
	checkErrorsInExtendsRef ref
	++ concatMap (checkErrorsInExp . snd) pars

checkErrorsInExtendsRef :: ExtendsRef -> [Error]
checkErrorsInExtendsRef (_, tps) = concatMap checkErrorsInDataType tps

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
		f (ExpError t) = [Error t]
		f e@FirstTry{} = [Error ("First try in out " ++ show e)]
		f (None tp) = checkErrorsInDataType tp
		f _ = []