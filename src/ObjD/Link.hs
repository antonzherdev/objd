module ObjD.Link (
	Sources, File(..), Class(..), Extends(..), Def(..), DataType(..), Exp(..), CImport(..), 
	DefMod(..), MathTp(..), DataTypeMod(..), ClassMod(..), Error(..), ExtendsClass(..), ExtendsRef, CallPar, Package(..),
	WrapReason(..), Annotation(..), Lang(..),
	Import(..), link, isClass, isType, isDef, isField, isEnum, isVoid, isStub, isStruct, isRealClass, isTrait, exprDataType, isStatic, enumItems,
	classConstructor, classFields, checkErrors, dataTypeClassName, dataTypeClassNameWithPrefix,
	isCoreFile, unwrapGeneric, forExp, extendsRefs, extendsClassClass, isConstructor,
	tpGeneric, superType, wrapGeneric, isConst, int, uint, byte, ubyte, int4, uint4, float, float4, resolveTypeAlias,
	classDefs, classGenerics, classExtends, classMods, classFile, classPackage, isGeneric, isNop, classNameWithPrefix,
	fileNameWithPrefix, classDefsWithTraits, classInitDef, classContainsInit, isPure, isError, isTpClass, isTpEnum, isTpStruct, isTpTrait,
	isAbstract, isFinal, isCaseClass, classFieldsForEquals, needHashForClass, needIsEqualForClass, isGenericWrap, mapExp, isInline,
	containsAnnotationWithClassName, findAnnotationWithClassName, annotationClass, isTpBaseClass, isBaseClass, traitExtendsRefs, mainExtendsRef,
	isSpecial
)where

import 			 Control.Arrow
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Decimal
import           Data.List
import           Ex.String
import qualified ObjD.Struct         as D
import           Data.Char
--import Debug.Trace

detailedReferenceError :: Bool
detailedReferenceError = False

data Lang = ObjC | Java deriving (Eq)

type Sources = [File]
data Package = Package {packageName :: [String], packageObject :: Maybe Class, packagePrefix :: String}
instance Eq Package where
	a == b = packageName a == packageName b

data File = File {fileName :: String, filePackage :: Package, fileImports :: [Import], fileClasses :: [Class]}
data Import = ImportClass {importClass :: Class} | ImportObjectDefs {importClass :: Class} deriving (Eq)
instance Eq File where
	File {fileName = a, filePackage = p} == File {fileName = b, filePackage = bp} = a == b && p == bp
coreFakeFile :: File
coreFakeFile = File "fake.od" (Package ["core"] Nothing "") [] []
fileNameWithPrefix :: File -> String
fileNameWithPrefix f = packagePrefix (filePackage f) ++ fileName f

{-----------------------------------------------------------------------------------------------------------------------------------------
 - CLASS 
 -----------------------------------------------------------------------------------------------------------------------------------------}
data Class = Class {_classFile :: File, _classPackage :: Package, className :: String
	, _classGenerics :: [Class], _classExtends :: Extends, _classMods :: [ClassMod], _classDefs :: [Def]
	, _classImports :: [Import], classAnnotations :: [Annotation]}
	| Generic {className :: String, _classExtendsRef :: [ExtendsRef]}
	| ClassError {className :: String, classErrorText :: String}
classFile :: Class -> Maybe File
classFile Class{_classFile = file} = Just file
classFile _ = Nothing
classPackage :: Class -> Package
classPackage Class{_classPackage = pack} = pack
classPackage _ = Package [] Nothing ""
classGenerics :: Class -> [Class]
classGenerics Class{_classGenerics = r} = r
classGenerics _ = []
classExtends :: Class -> Extends
classExtends Class{_classExtends = r} = r
classExtends Generic{_classExtendsRef = r} = Extends Nothing r
classExtends _ = extendsNothing
classDefs :: Class -> [Def]
classDefs Class{_classDefs = r} = r
classDefs _ = []
classStaticDefs :: Class -> [Def]
classStaticDefs = filter (isStatic) . classDefs
classMods :: Class -> [ClassMod]
classMods Class{_classMods = r} = r
classMods _ = []
classImports :: Class -> [Import]
classImports Class{_classImports = r} = r
classImports _ = []
classNameWithPrefix :: Class -> String
classNameWithPrefix cl = packagePrefix (classPackage cl) ++ cap (className cl)
classFieldsForEquals :: Class -> [Def]
classFieldsForEquals cl = maybe [] defPars $ classConstructor cl
needIsEqualForClass :: Class -> Bool
needIsEqualForClass cl = ClassModCase `elem` classMods cl || any ( ("isEqual" == ). defName) (classDefs cl)
needHashForClass :: Class -> Bool
needHashForClass cl = ClassModCase `elem` classMods cl && (not $ any ( ("hash" == ). defName) (classDefs cl))
fullClassName :: Class -> String
fullClassName cl = strs "." (packageName $ classPackage cl) ++ case className cl of
	"" -> ""
	nm -> "." ++ nm

data ClassMod = ClassModStub | ClassModStruct | ClassModTrait | ClassModEnum | ClassModObject | ClassModType | 
	ClassModAbstract | ClassModFinal | ClassModCase | ClassModPackageObject deriving (Eq)

type ExtendsRef = (Class, [DataType])
data Extends = Extends {extendsClass :: Maybe ExtendsClass, extendsTraits :: [ExtendsRef]}
data ExtendsClass = ExtendsClass ExtendsRef [CallPar]
type Generics = M.Map String DataType
type ClassRef = (Class, Generics)

instance Show Class where
	show (Generic name ext) = name ++ extString
		where
			extString = case filter ( (/= "Object") . className . fst) ext of
				[] -> ""
				genExtendsRefs -> " extends " ++ strs " with " (map showExtendsRef genExtendsRefs)
	show (ClassError name er) = name ++ ": " ++ er
	show cl =
		tp cl ++ " " ++ className cl ++ sConstr cl ++ " " ++ show (classExtends cl) ++ " {\n" ++
			(unlines . map ind . concatMap (lines . show)) (classDefs cl)  ++
		"}"
		where
			tp Class{} = strs' " " $ classMods cl
			tp Generic{} = "generic"
			sConstr c = maybe "" (\cc -> "(" ++ (strs ", " . map defName . defPars) cc ++ ")") (classConstructor c)
instance Show ClassMod where
	show ClassModStub = "stub"
	show ClassModStruct = "struct"
	show ClassModTrait = "trait"
	show ClassModEnum = "enum"
	show ClassModObject = "object"
	show ClassModType = "type"
	show ClassModAbstract = "abstract"
	show ClassModFinal = "final"
	show ClassModCase = "case"
	show ClassModPackageObject = "package"
			
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
isCoreFile File{filePackage = Package (x : _) _ _} = x == "core"
isCoreFile _ = False
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
isStruct = (ClassModStruct `elem` ) . classMods
isType :: Class -> Bool
isType = (ClassModType `elem` ) . classMods
isTrait :: Class -> Bool
isTrait = (ClassModTrait `elem` ) . classMods
isStub :: Class -> Bool
isStub = (ClassModStub `elem` ) . classMods
isFinal :: Class -> Bool
isFinal = (ClassModFinal `elem` ) . classMods
isCaseClass :: Class -> Bool
isCaseClass = (ClassModCase `elem` ) . classMods
isAbstract :: Class -> Bool
isAbstract = (ClassModAbstract `elem` ) . classMods
isBaseClass :: Class -> Bool
isBaseClass Class{className = "Object"} = True
isBaseClass _ = False

isRealClass :: Class -> Bool
isRealClass = (ClassModStub `notElem` ) . classMods
isEnum :: Class -> Bool
isEnum = (ClassModEnum `elem` ) . classMods
classFields :: Class -> [Def]
classFields = filter isField . classDefs

classPackageName :: Class -> [String]
classPackageName = packageName . classPackage


data Annotation = Annotation Def [CallPar]

annotationClass :: Annotation -> Class
annotationClass (Annotation Def{defType = TPClass _ _ cl} _) = cl

findAnnotationWithClassName :: String -> [Annotation] -> Maybe Annotation
findAnnotationWithClassName nm = find ((== nm) . fullClassName . annotationClass)

containsAnnotationWithClassName :: String -> [Annotation] -> Bool
containsAnnotationWithClassName nm a = isJust $ findAnnotationWithClassName nm a


{-----------------------------------------------------------------------------------------------------------------------------------------
 - Extends 
 -----------------------------------------------------------------------------------------------------------------------------------------}

extendsNothing :: Extends
extendsNothing = Extends Nothing []

extendsRefs :: Extends -> [ExtendsRef]
extendsRefs (Extends Nothing traits) = traits
extendsRefs (Extends (Just (ExtendsClass cl _)) traits) = cl : traits

mainExtendsRef :: Extends -> Maybe ExtendsRef
mainExtendsRef (Extends (Just (ExtendsClass cl _)) _) = Just cl
mainExtendsRef _ = Nothing

traitExtendsRefs :: Extends -> [ExtendsRef]
traitExtendsRefs (Extends _ traits) = traits

superClass :: Class -> Maybe Class
superClass = fmap extendsClassClass . extendsClass . classExtends

superClasses :: Class -> [Class]
superClasses = map fst . extendsRefs . classExtends

replaceGenerics :: Bool -> Generics -> DataType -> DataType
replaceGenerics blk gns tp = mapDataType f tp
	where 
		f (TPClass TPMGeneric _ (Generic g _)) = fmap wrapBlock $ M.lookup g gns
		f w@(TPGenericWrap reasons _) = if WrapReasonBlock `elem` reasons then Just w else Nothing
		f _ = Nothing
		wrapBlock w@(TPGenericWrap reasons e) 
			| blk = if WrapReasonBlock `elem` reasons then w else TPGenericWrap (WrapReasonBlock:reasons) e
		wrapBlock e 
			| blk = TPGenericWrap [WrapReasonBlock] e
		wrapBlock e = e
unblockGenerics :: DataType -> DataType
unblockGenerics tp = mapDataType f tp
	where
		f (TPGenericWrap [WrapReasonBlock] e) = Just (mapDataType f e)
		f (TPGenericWrap reasons e) = if WrapReasonBlock `elem` reasons then Just (TPGenericWrap (filter (/= WrapReasonBlock) reasons) (mapDataType f e)) else Nothing
		f _ = Nothing 

objectDef :: Class -> Def
objectDef cl = Def {defName = className cl, defPars = [], defType = TPObject (refDataTypeMod cl) cl, defBody = Nop, 
				defMods = [DefModStatic, DefModObject], defGenerics = Nothing}

replaceGenericsInDef :: Generics -> Def -> Def
replaceGenericsInDef gens d = d {defType = replaceGenerics False gens (defType d), defPars = map (replaceGenericsInDef gens) (defPars d) }

allDefsInClass :: ClassRef -> [Def]
-- allDefsInClass (cl, gens) | trace ("allDefsInClass " ++ className cl ++ ": " ++ show gens) False = undefined
allDefsInClass (cl, gens) = 
	map (replaceGenericsInDef gens) notStaticDefs 
	++ allDefsInParentClass gens cl
	where
		notStaticDefs = filter (not . isStatic) (classDefs cl) 

allDefsInParentClass :: Generics -> Class -> [Def]
-- allDefsInParentClass gens cl | trace ("allDefsInParentClass " ++ className cl ++ ": " ++ show gens) False = undefined
allDefsInParentClass gens cl = concatMap defsInExtends $ extendsRefs $ classExtends cl
	where
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
buildGenerics cl gens = M.fromList $ zip (map className $ classGenerics cl) $ map wrapGeneric gens

buildGenericsForSelf :: Class -> Generics
buildGenericsForSelf cl = M.fromList $ map (\g -> (className g, refDataType g []) ) $ classGenerics cl


superGenericsList :: Generics -> ExtendsRef -> [DataType]
superGenericsList gens (_, extGens) = map (wrapGeneric . replaceGenerics True gens) extGens

superGenerics :: Generics -> ExtendsRef -> Generics
superGenerics gens (cl, extGens) = buildGenerics cl $ map (wrapGeneric . replaceGenerics True gens) extGens

upGenericsToClass :: Class -> (Class, [DataType]) -> Maybe [DataType]
upGenericsToClass destinationClass (cl, gens) 
	| destinationClass == cl = Just gens
	| otherwise = listToMaybe $ mapMaybe mapRef $ extendsRefs (classExtends cl)
		where
			mapRef ref@(ccl, _)= upGenericsToClass destinationClass (ccl, superGenericsList (buildGenerics cl gens) ref)
extendsClassClass :: ExtendsClass -> Class
extendsClassClass (ExtendsClass (cl, _) _) = cl

extendsClassRef :: ExtendsClass -> ExtendsRef
extendsClassRef (ExtendsClass ref _) = ref

superDataType :: Env -> DataType -> ExtendsRef -> DataType
superDataType _ (TPClass _ gens cl) extRef@(extCl, _) = 
	TPClass (refDataTypeMod extCl) (superGenericsList (buildGenerics cl gens) extRef) extCl
superDataType _ (TPObject _ _) (extCl, _) = TPObject (refDataTypeMod extCl) extCl
superDataType env tp extRef@(extCl, _) = TPClass (refDataTypeMod extCl) 
	(superGenericsList (buildGenerics (dataTypeClass env tp) (dataTypeGenerics env tp)) extRef)  extCl
-- superDataType _ _ = TPVoid

superDataTypes :: Env -> DataType -> [DataType]
superDataTypes env tp = map (superDataType env tp) $ extendsRefs $ classExtends $ dataTypeClass env tp

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
superType _ = Nothing

isInstanceOf :: Class -> Class -> Bool
isInstanceOf cl Generic{_classExtendsRef = extends} = 
	all (( cl `isInstanceOf` ) . fst ) extends
isInstanceOf cl target
	| target == cl = True
	| otherwise = any (\extendsRef -> fst extendsRef `isInstanceOf` target) $ extendsRefs (classExtends cl)

isInstanceOfTp :: Env -> DataType -> DataType -> Bool
isInstanceOfTp _ cl target
	| target == cl = True
isInstanceOfTp _ TPUnset{} _ = True
isInstanceOfTp _ TPNumber{} TPNumber{} = True
isInstanceOfTp _ TPNumber{} TPFloatNumber{} = True
isInstanceOfTp _ TPFloatNumber{} TPNumber{} = True
isInstanceOfTp _ TPFloatNumber{} TPFloatNumber{} = True
isInstanceOfTp env (TPSelf l) r = isInstanceOfTp env (refDataType l []) r
isInstanceOfTp env l (TPSelf r) = isInstanceOfTp env l (refDataType r [])
isInstanceOfTp env l (TPGenericWrap _ r)  = isInstanceOfTp env l r
isInstanceOfTp env (TPGenericWrap _ l) r = isInstanceOfTp env l r
isInstanceOfTp env l@(TPClass TPMType _ _) r = isInstanceOfTp env (fromJust $ superType l) r
isInstanceOfTp env l r@(TPClass TPMType _ _) = isInstanceOfTp env l (fromJust $ superType r)
isInstanceOfTp _ _ TPAny = True
isInstanceOfTp _ _ TPThrow = True
isInstanceOfTp _ TPThrow _ = True
isInstanceOfTp _ TPNil (TPOption _ _) = True
isInstanceOfTp _ TPNil TPVoid = True
isInstanceOfTp _ TPAnyGeneric _ = True
isInstanceOfTp _ _ TPAnyGeneric = True
isInstanceOfTp _ _ TPUnknown{} = True
isInstanceOfTp env (TPOption _ a) (TPOption _ b) 
	| a == b = True
	| otherwise = isInstanceOfTp env a b
isInstanceOfTp env a (TPOption _ b)  = isInstanceOfTp env a b
isInstanceOfTp env (TPOption True a) b  = isInstanceOfTp env a b
isInstanceOfTp _ TPVoid (TPClass TPMGeneric _ _) = True
isInstanceOfTp _ TPNil (TPClass TPMGeneric _ _) = True
isInstanceOfTp env cl (TPClass TPMGeneric _ t) = dataTypeClass env cl `isInstanceOf` t
isInstanceOfTp _ (TPClass _ _ _) (TPClass _ _ Class{className = "Object"}) = True
isInstanceOfTp env cl target
	-- | trace (show cl ++ " isInstanceOfTp " ++ show target ++ " / " ++ className (dataTypeClass env cl) ++ " isInstanceOf " ++ className (dataTypeClass env target)) False = undefined
	| dataTypeClass env cl == dataTypeClass env target = 
		all (\(clg, tg) -> isInstanceOfTp env clg tg ) $ zip (dataTypeGenerics env cl) (dataTypeGenerics env target)
	-- | otherwise =  dataTypeClass env cl `isInstanceOf` dataTypeClass env target
	| otherwise = any (\tp -> isInstanceOfTp env tp target) $ superDataTypes env cl
		

isInstanceOfCheck :: Env -> DataType -> DataType -> Bool
isInstanceOfCheck env l r = isInstanceOfTp env l r

baseDataType :: Env -> DataType
baseDataType env = TPClass TPMClass [] $ classFind (envIndex env) "Object"

commonSuperDataType :: Env -> DataType -> DataType -> [DataType]
-- commonSuperDataType _ a b | trace ("commonSuperDataType: " ++ show a ++ " and " ++ show b) False = undefined
commonSuperDataType _ a b 
	| a == b = [a]
commonSuperDataType env (TPGenericWrap _ a) b = commonSuperDataType env a b
commonSuperDataType env a (TPGenericWrap _ b) = commonSuperDataType env a b
commonSuperDataType env TPNil a = commonSuperDataType env a TPNil
commonSuperDataType _ TPAny a = [a]
commonSuperDataType _ TPThrow a = [a]
commonSuperDataType _ a@(TPOption _ _) TPNil = [a]
commonSuperDataType _ TPVoid TPNil = [TPVoid]
commonSuperDataType _ a TPNil = [option False a]
commonSuperDataType _ a TPAny = [a]
commonSuperDataType _ a TPThrow = [a]
commonSuperDataType _ TPNumber{} f@TPFloatNumber{} = [f]
commonSuperDataType _ f@TPFloatNumber{} TPNumber{} = [f]
commonSuperDataType _ (TPNumber as an) (TPNumber bs bn) = [TPNumber (as || bs) (max an bn)]
commonSuperDataType _ (TPFloatNumber an) (TPFloatNumber bn) = [TPFloatNumber (max an bn)]
commonSuperDataType env (TPOption ca a) (TPOption cb b) = map (option (ca && cb)) $ commonSuperDataType env a b
commonSuperDataType env a (TPOption c b) = map (option c) $ commonSuperDataType env a b
commonSuperDataType env (TPOption c a) b = map (option c) $ commonSuperDataType env a b
commonSuperDataType env _ TPVoid = [baseDataType env]
commonSuperDataType env TPVoid _ = [baseDataType env]
commonSuperDataType env a b 
	| a == b = [a]
	| dataTypeClass env a == dataTypeClass env b = 
		[mapDataTypeGenerics (map (\(ag, bg) -> wrapGeneric $ head $ commonSuperDataType env ag bg) . zip (dataTypeGenerics env a) ) b]
	| isInstanceOfTp env a b = [b]
	| isInstanceOfTp env b a = [a]
	| otherwise = 
		let 
			commons = nub $ concatMap (uncurry $ commonSuperDataType env) $ [(a', b) |a' <- superDataTypes env a] ++ [(a, b') |b' <- superDataTypes env b]
			removeCommonCommons [] = []
			removeCommonCommons (x:xs)
				| any (\xx -> isInstanceOfTp env xx x) xs = removeCommonCommons xs
				| otherwise = x:removeCommonCommons xs
		in removeCommonCommons commons


firstCommonSuperDataType :: Env -> DataType -> DataType -> DataType
firstCommonSuperDataType env a b = case commonSuperDataType env a b of
	[] -> TPUnknown $ "No common super data type for " ++ show a ++ " and " ++ show b
	x:_ -> x

reduceDataTypes :: Env -> [DataType] -> DataType
reduceDataTypes env tps = foldl1 (firstCommonSuperDataType env) tps		


classContainsInit :: Class -> Bool 
classContainsInit cl = isJust (classInitDef cl) || any classContainsInit (superClasses cl) 

classInitDef :: Class -> Maybe Def
classInitDef cl = find (\d -> "init" == defName d && null (defPars d)) $ classDefs cl

findDefWithName :: String -> Class -> Maybe Def
findDefWithName name cl = find ((name ==) . defName) $ classDefs cl

findValWithName :: String -> Class -> Maybe Def
findValWithName name cl = find (\d -> name == defName d && DefModField `elem` defMods d && null (defPars d)) $ classDefs cl

classDefsWithTraits :: Class -> [Def]
classDefsWithTraits cl = classDefs cl ++ notOverloadedTraitDefs
	where	
		notOverloadedTraitDefs = filter (\def -> not $ any (== def) notAbstractClassDefs) notAbstractTraitDefs
		--notOverloadedTraitDefs = notAbstractTraitDefs
		notAbstractTraitDefs = notAbstractDefs True
		notAbstractClassDefs = notAbstractDefs False
		notAbstractDefs trait = (filter ( (DefModAbstract `notElem`). defMods) . map fst . filter (\t -> trait == snd t)) allDefsWithLine
		allDefsWithLine :: [(Def, Bool)]
		allDefsWithLine = allDefsWithLine' False True cl
		allDefsWithLine' :: Bool -> Bool -> Class -> [(Def, Bool)] -- (Def, traitLine - True/classLine - False)
		allDefsWithLine' currentLine traitLine cll = 
			map (\def -> (def, currentLine)) (classDefs cll) 
			++ concatMap nextRec ((extendsRefs . classExtends) cll)
			where
				nextRec :: ExtendsRef -> [(Def, Bool)]
				nextRec (nextClass, _) = 
					let line = traitLine && isTrait nextClass
					in allDefsWithLine' line line nextClass
{-----------------------------------------------------------------------------------------------------------------------------------------
 - Def 
 -----------------------------------------------------------------------------------------------------------------------------------------}

data Def = Def {defName :: String, defPars :: [Def], defType :: DataType, defBody :: Exp, defMods :: [DefMod]
	, defGenerics :: Maybe DefGenerics}
unknownDef :: Def
unknownDef = Def "???" [] TPVoid Nop [] Nothing
localVal :: String -> DataType -> Def
localVal name tp = Def name [] tp Nop [DefModLocal] Nothing
localValE :: String -> DataType -> Exp -> Def
localValE name tp e = Def name [] tp e [DefModLocal] Nothing
tmpVal :: Env -> String -> DataType -> Exp -> Def
tmpVal env sf tp e = Def ("__tmp" ++ envVarSuffix env ++ sf) [] tp e [DefModLocal] Nothing 
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
isSpecial :: Def -> Bool
isSpecial = (DefModSpecial `elem` ) . defMods
isInline :: Def -> Bool
isInline = (DefModInline `elem` ) . defMods
isPure :: Def -> Bool
isPure = (DefModPure `elem` ) . defMods
enumItems :: Class -> [Def]
enumItems = filter isEnumItem . classDefs
instance Eq Def where
	a == b = defName a == defName b && length (defPars a) == length (defPars b) && all eqPar (zip (defPars a)(defPars b)) && (isStatic a == isStatic b)

eqPar :: (Def, Def) -> Bool
eqPar (x, y) = defName x == defName y

data DefMod = DefModStatic | DefModMutable | DefModAbstract | DefModPrivate | DefModPublic | DefModProtected | DefModGlobalVal | DefModWeak
	| DefModConstructor | DefModStub | DefModLocal | DefModObject 
	| DefModField | DefModEnumItem | DefModDef | DefModSpecial | DefModStruct | DefModApplyLambda | DefModSuper | DefModInline 
	| DefModPure | DefModFinal | DefModOverride | DefModError String | DefModConstructorField
	deriving (Eq, Ord)
instance Show DefMod where
	show DefModStatic = "static"
	show DefModMutable = "var"
	show DefModAbstract = "abstract" 
	show DefModPrivate = "private"
	show DefModPublic = "public"
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
	show DefModConstructorField = ""
	show DefModSuper = "super"
	show DefModApplyLambda = "applyLambda"
	show DefModPure = "pure"
	show DefModFinal = "final"
	show DefModOverride = "override"
	show (DefModError s) = "Error: " ++ s
data DefGenerics = DefGenerics{defGenericsClasses :: [Class], defGenericsSelfType :: DataType}

data CImport = CImportLib String | CImportUser String

instance Show File where
	show f =
		"// " ++ fileName f ++ ".od\n" ++
		{-((`tryCon` "\n\n" ). strs' "\n") cimps ++
		((`tryCon` "\n\n") . strs' "\n") imps ++ 
		{trs' "\n" gldefs ++-}
		strs' "\n\n" (fileClasses f)
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
		ch DefModPublic = 'I'
		ch DefModFinal = 'f'
		ch DefModOverride = 'o'
		ch DefModProtected = 'q'
		ch DefModWeak = 'w'
		ch DefModPure = 'u'
		ch DefModConstructor = 'c'
		ch DefModStub = 'b'
		ch DefModGlobalVal = 'g'
		ch DefModField = 'e'
		ch DefModLocal = 'l'
		ch DefModObject = 'o'
		ch DefModEnumItem = 'e'
		ch DefModDef = 'd'
		ch DefModSpecial = 'i'
		ch DefModStruct = 's'
		ch DefModApplyLambda = 'd'
		ch DefModSuper = 'r'
		ch DefModInline = 'i'
		ch DefModConstructorField = 'U'
		ch (DefModError _) = 'E'
		

dataTypePars :: DataType -> [Def]
dataTypePars (TPFun (TPTuple pars) _) = map (localVal "") pars
dataTypePars (TPFun t _) = [localVal "" t]
dataTypePars _ = []		 
		

idx :: (a -> k) -> a -> (k, a)
idx f a = (f a, a)

literalDefName :: String -> String
literalDefName "" = ""
literalDefName nam = nmRec False nam
	where
		mbCap True s = cap s
		mbCap False s = s
		nmm = M.fromList [('+', "add"), ('-', "sub"), ('*', "mul"), ('/', "div")]
		nmRec ncap (x:xs) = maybe ( (if ncap then  toUpper x else x ): nmRec False xs) (\s -> mbCap ncap s ++ nmRec True xs) $ M.lookup x nmm
		nmRec _ _ = ""

{-----------------------------------------------------------------------------------------------------------------------------------------
 - Link 
 -----------------------------------------------------------------------------------------------------------------------------------------}

link :: Lang -> D.Sources -> Sources
link lang src = files
	where
		files = map (linkFile lang files) src

linkFile :: Lang -> [File] -> D.File -> File
linkFile lang files (D.File name package stms) = fl
	where
		fl :: File
		fl = File {fileName = name, fileImports = thisFileImports,
			fileClasses = classes, filePackage = package'}
		stms' = filter (not . containsOtherLangAnnotation) stms
		containsOtherLangAnnotation stm = any isOtherLangAnnotation $ D.stmAnnotations stm
		otherLangs = ["ObjD" | lang /= ObjC] ++ ["Java" | lang /= Java]
		isOtherLangAnnotation (D.Annotation nm [] []) = nm `elem` otherLangs
		isOtherLangAnnotation _ = False

		classes = (map linkCl . filter isCls) stms'
		linkCl cl = linkClass (cidx cl, glidx cl, fl, package', clImports cl) cl
		isCls s = D.isClass s || D.isStub s || D.isEnum s || D.isType s
		cidx cl =  M.fromList . map (idx className) $ classes ++ importClasses cl ++ (concatMap fileClasses $ kernelFiles ++ packageFiles)
		glidx cl = importObjectDefs cl
		package' = case package of
			[] -> error $ "Empty package for file " ++ name
			_ -> Package package packObj packPref

		packPref :: String
		packPref = fromMaybe "" $ do 
			o <- packObj
			p <- findValWithName "prefix" o
			(extractStringConst . defBody) p
		packObj :: Maybe Class
		packObj = find (\cl -> "" == className cl && ClassModObject `elem` classMods cl && ClassModPackageObject `elem` classMods cl) 
			. concatMap fileClasses 
			. filter ((== package) . packageName . filePackage) $ files

		importClasses cl = mapMaybe impcl (imports cl)
			where
				impcl (ImportClass c)= Just c
				impcl _ = Nothing
		importObjectDefs cl = mapMaybe impcl (imports cl)
			where
				impcl (ImportObjectDefs c)= Just c
				impcl _ = Nothing
		imports :: D.FileStm -> [Import]
		imports cl = nub (clImports cl ++ thisFileImports ++ packObjImports)

		packObjImports = maybe [] (\o -> ImportObjectDefs o : classImports o) packObj

		thisFileImports :: [Import]
		thisFileImports = concatMap processImport . filter D.isImport $ stms'
			where
				processImport (D.Import imp _) = linkImport allFiles imp

		packageFiles = filter (\f -> f /= fl && package == (packageName . filePackage) f) files
		
		allFiles = filter (/= fl) $ files
		
		kernelFiles :: [File]
		kernelFiles = filter ((== "core") . head . packageName . filePackage ) files 
		
		clImports :: D.FileStm -> [Import]
		clImports cl = concatMap (\(D.ClassImport inn) -> linkImport files inn) . filter D.isClassImport $ classBody cl
			where
				classBody D.Class{} = D.classBody cl
				classBody D.Enum{} = D.classBody cl
				classBody _ = []
	

linkImport :: [File] -> [String] -> [Import]
linkImport files name
	| last name == "_" = let s = init name
		in (map ImportClass . filter (startsWith s . classPackageName)) allClasses 
				++ (map ImportObjectDefs . packObject) s
	| otherwise = map ImportClass $ classesWithName name
	where
		allClasses = concatMap fileClasses files
		packObject imp = filter (\c -> ClassModPackageObject `elem` classMods c && classPackageName c == imp) allClasses
		classesWithName imp = filter (\c -> className c == last imp && classPackageName c == init imp) allClasses

baseClassExtends :: ClassIndex -> ExtendsClass
baseClassExtends cidx = ExtendsClass (classFind cidx "Object", []) []

linkClass :: (ClassIndex, ObjectIndex, File, Package, [Import]) -> D.FileStm -> Class
-- linkClass (_, _, _, _, _) D.Class{D.className = cls} | trace ("Class " ++ cls) False = undefined
linkClass (ocidx, glidx, file, package, clImports) cl = self
	where
		cidx = ocidx `M.union` M.fromList (map (\g -> (className g, g)) generics)
		env = Env selfType cidx glidx [] False TPVoid ""
		staticEnv = Env (TPObject (refDataTypeMod self) self) ocidx glidx [] False TPVoid ""
		isObject = case cl of
			D.Class{} -> D.ClassModObject `elem` D.classMods cl
			_ -> False
		self = case cl of
			D.Class{} -> Class {
				_classFile = file,
				_classPackage = package,
				_classMods = concatMap clsMod (D.classMods cl), 
				className = D.className cl, 
				_classExtends = if D.className cl == "Object" then extendsNothing else fromMaybe (Extends (Just $ baseClassExtends cidx) []) extends, 
				_classDefs = 
					if isObject then fields ++ defs ++ [typeField | not isSeltTrait] 
					else fields ++ defs ++ constr constrPars ++ [typeField | not isSeltTrait] 
				{-++ [unapply | D.ClassModTrait `notElem` D.classMods cl && not hasUnapply]-}, 
				_classGenerics = generics,
				_classImports = clImports,
				classAnnotations = annotations
			}
			D.Enum{} -> Class {
				_classFile = file,
				_classPackage = package,
				_classMods = [ClassModEnum], 
				className = D.className cl, 
				_classExtends = Extends (Just $ ExtendsClass 
					(classFind cidx "Enum", [TPClass TPMEnum [] self])  
					[(enumOrdinal, callLocalVal "ordinal" uint), (enumName, callLocalVal "name" TPString)]) [], 
				_classDefs =  enumConstr ++
					snd (mapAccumL enumItem 0 (D.enumItems cl)) ++ fields ++ defs ++ [Def{
					defName = "values", defType = TPArr 0 (TPClass TPMEnum [] self), defBody = Nop,
					defMods = [DefModStatic], defPars = [], defGenerics = Nothing}],
				_classGenerics = generics,
				_classImports = clImports,
				classAnnotations = annotations
			}
			D.Type{} -> Class {
				_classFile = file,
				_classPackage = package,
				_classMods = [ClassModType, ClassModStub], 
				className = D.className cl, 
				_classExtends = Extends (Just $ ExtendsClass (linkExtendsRef env (D.typeDef cl)) []) [], 
				_classDefs = [constructorForType], 
				_classGenerics = generics,
				_classImports = [],
				classAnnotations = annotations
			}
		isSeltTrait = D.ClassModTrait `elem` D.classMods cl
		annotations = map (linkAnnotations env) $ D.stmAnnotations cl
		enumOrdinal = Def "ordinal" [] uint Nop [] Nothing
		enumName = Def "name" [] TPString Nop [] Nothing
		enumAdditionalDefs = [(enumOrdinal, Nothing), (enumName, Nothing)]
		selfType = refDataType self (map (TPClass TPMGeneric []) generics)
		clsMod D.ClassModStruct = [ClassModStruct]
		clsMod D.ClassModStub = [ClassModStub]
		clsMod D.ClassModTrait =  [ClassModTrait]
		clsMod D.ClassModObject = [ClassModObject]
		clsMod D.ClassModAbstract = [ClassModAbstract]
		clsMod D.ClassModFinal = [ClassModFinal]
		clsMod D.ClassModPackageObject = [ClassModPackageObject]
		clsMod D.ClassModCase = [ClassModFinal, ClassModCase]
		-- clsMod _ = []
		extends = fmap (linkExtends env (map fst constrPars)) (D.classExtends cl) 
		selfIsStruct = case cl of
			D.Class{} -> D.ClassModStruct `elem` D.classMods cl
			_ -> False
		isStaticDecl d = isObject || D.isStatic d
		fields :: [Def]
		fields =  concatMap (linkField staticEnv (isObject, selfIsStruct)) (filter (isStaticDecl) decls)  ++
			concatMap (linkField (envAddVals (map fst constrPars) env) (isObject, selfIsStruct)) (filter (not . isStaticDecl) decls)
		decls = (map (\d -> d{D.defBody = D.Nop}) . filter (\f -> D.isDecl f)) (D.classFields cl) 
			++ filter D.isDecl (D.classBody cl)
		defs = concatMap (\ def -> 
			linkDef (envForDef def) def ([DefModStruct | selfIsStruct] ++ [DefModStatic | isObject] ++ [DefModStub | D.ClassModStub `elem` D.classMods cl])) 
			. filter D.isDef $ D.classBody cl
		envForDef def = if isStaticDecl def then staticEnv else env
		enumConstr = constr (enumAdditionalDefs ++ constrPars)
		constr :: [(Def, Maybe Exp)] -> [Def]
		constr pars = let
			mainDef = Def{defName = "apply", defMods = [DefModStatic, DefModConstructor, DefModPublic] ++ [DefModStruct | selfIsStruct], defBody = Nop,
				defPars = map fst pars, defType = selfType, defGenerics = Just $ DefGenerics generics selfType}
			in resolveDefPar env mainDef pars 
		constrPars :: [(Def, Maybe Exp)]
		constrPars = map constrPar (D.classFields cl)
		constrPar :: D.ClassStm -> (Def, Maybe Exp)
		constrPar D.Def{D.defName = name, D.defRetType = Just tp, D.defBody = b} = let
			tp' = dataType env tp
			env' = envAddVals (map fst constrPars) env
			in (Def name [] (dataType env tp) Nop [DefModLocal, DefModWeak] Nothing,
				case b of
					D.Nop -> Nothing
					_ -> Just $ exprTo env' tp' b)
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
		typeField = Def{defMods = [DefModField, DefModStatic, DefModSpecial] ++ [DefModStruct | selfIsStruct], defName = "type", 
			defType = TPClass TPMClass [mapDataTypeGenerics (map (\_ -> TPAnyGeneric)) selfType] (classFind cidx typeName), 
			defBody = Nop, 
			defGenerics = Nothing, defPars = []}
			where 
				typeName = if selfIsStruct then "PType" else "ClassType"
linkExtends :: Env -> [Def] -> D.Extends -> Extends
linkExtends env constrPars (D.Extends (D.ExtendsClass eref@(_, gens) pars) withs) = 
	let 
		env' = env {envVals = constrPars, envSelf = objectType $ envSelf env}
		superCall = D.Dot D.Super $ D.Call "apply" (Just $ pars) gens
		superCall' = expr env' superCall
		superCallPars = case superCall' of
			Dot _ (Call _ _ pars' _) -> pars'
			err -> [(unknownDef, err)]
		mainExt = linkExtendsRef env eref
		withs' = map (linkExtendsRef env) withs
		isMainTrait = isTrait (fst mainExt)
	in Extends (if isMainTrait then Nothing else Just $ ExtendsClass mainExt superCallPars) (if isMainTrait then mainExt:withs' else withs')

linkAnnotations :: Env -> D.Annotation -> Annotation
linkAnnotations env (D.Annotation nm pars tps) = case expr env (D.Call nm (Just pars) tps) of
	Call d _ pars' _ -> Annotation d pars'
	Dot _ (Call d _ pars' _ ) -> Annotation d pars'

linkExtendsRef :: Env -> D.ExtendsRef -> ExtendsRef
linkExtendsRef env (ecls, gens) = (classFind (envIndex env) ecls, map (dataType env) gens) 

linkGeneric :: Env -> D.Generic -> Class
linkGeneric env (D.Generic name ext) = Generic{className = name,
	_classExtendsRef = (classFind (envIndex env) "Object", []) : map (linkExtendsRef env) genExtendsRefs}
	where 
		genExtendsRefs = case ext of
			Nothing -> []
			Just (D.Extends (D.ExtendsClass firstExtends []) nextExtends) -> firstExtends : nextExtends

linkField :: Env -> (Bool, Bool) -> D.ClassStm -> [Def]
--linkField _ _ D.Def{D.defName = nm} | trace ("Field " ++ nm) False = undefined
linkField env (obj, _isStruct) dd@D.Def{D.defMods = mods, D.defName = name, D.defRetType = tp, D.defBody = e} = 
	let 
		i = exprToSome env e
		i' = implicitConvertsion env tp'' i
		tp' = unwrapGeneric $ getDataType env tp i
		tp'' = if _isStruct then case tp' of
				TPArr n atp -> TPEArr n $ unwrapGeneric atp 
				_ -> tp' 
			else tp'
	
		gtp = wrapGeneric tp''
		def = Def{defMods = 
			DefModField : translateMods mods ++ [DefModStruct | _isStruct] ++ [DefModStatic | obj] ++ checkOverrideMods mods (findOverridenDef env dd), 
			defName = name, defType = tp'', 
			defBody = i', defGenerics = Nothing, defPars = []}
		isLazy = D.DefModLazy `elem` mods
		lazyClass = classFind (envIndex env) "Lazy"
		lazyGet = fromJust $ findDefWithName "get" lazyClass
		lazyConstr = fromJust $ classConstructor lazyClass
		lazyTp = TPClass TPMClass [gtp] lazyClass
		defLazy = Def{defMods = [DefModField, DefModPrivate] ++ [DefModStatic | D.DefModStatic `elem` mods || obj], defName = "_lazy_" ++ name, 
			defType = lazyTp, 
			defBody = Dot (callRef (objectDef lazyClass)) $ Call lazyConstr lazyTp [(head $ defPars lazyConstr, Lambda [] (Return True (Weak i')) gtp)] [], 
			defGenerics = Nothing, defPars = []}
		defLazyGet = Def{defMods = DefModDef : translateMods mods ++ [DefModStatic | obj], defName = name, 
			defType = tp'', 
			defBody = Return True $ Dot (callRef defLazy) (Call lazyGet gtp [] []), defGenerics = Nothing, defPars = []}
		in if isLazy then [defLazyGet, defLazy] else [def]

findOverridenDef :: Env -> D.ClassStm -> Maybe Def
findOverridenDef env D.Def{D.defName = name, D.defPars = opars} = find eqDef $ allDefsInParentClass (buildGenerics cl $ dataTypeGenerics env $ envSelf env) cl
	where
		cl = envSelfClass env
		eqDef d = defName d == name && length (defPars d) == length opars' && all eqParameterNames (zip (defPars d) opars')
		eqParameterNames (Def{defName = l}, D.Par{D.parName = r}) = l == r
		filterSelfPar (D.Par{D.parName = "self"}:xs) = xs
		filterSelfPar xs = xs
		opars' = filterSelfPar opars

checkOverrideMods :: [D.DefMod] -> Maybe Def -> [DefMod]
checkOverrideMods mods (Just o) =
	[DefModError "No override modifier" |  D.DefModOverride `notElem` mods] ++
	[DefModError "Override final def" | DefModFinal `elem` defMods o] 
checkOverrideMods mods Nothing = [DefModError "Override nothing" |  D.DefModOverride `elem` mods]		

translateMods :: [D.DefMod] -> [DefMod]
translateMods = fx . mapMaybe m
	where 
		fx ms = if DefModPrivate `notElem` ms && DefModProtected `notElem` ms then DefModPublic : ms else ms
		m D.DefModStatic = Just DefModStatic
		m D.DefModMutable = Just DefModMutable
		m D.DefModPrivate = Just DefModPrivate
		m D.DefModProtected = Just DefModProtected
		m D.DefModWeak = Just DefModWeak
		m D.DefModPure = Just DefModPure
		m D.DefModFinal = Just DefModFinal
		m D.DefModOverride = Just DefModOverride
		m D.DefModConstructorField = Just DefModConstructorField
		m D.DefModInline = Just DefModInline
		m _ = Nothing
		
linkDef :: Env -> D.ClassStm -> [DefMod] -> [Def]
-- linkDef env D.Def{D.defName = name} _ | trace ("Def " ++ show (envSelf env) ++ "." ++ name) False = undefined
linkDef env dd@D.Def{D.defMods = mods, D.defName = name, D.defPars = opars, D.defRetType = tp, D.defBody = body, D.defGenerics = generics} additionalMods = 
	resolveDefPar env' mainDef pars''
	where 
		env' = addEnvInit $ envAddClasses generics' env
		generics' = map (linkGeneric env) generics
		isInit = name == "init" && null opars
		addEnvInit e = if isInit then e {envInit = True} else e
		
		pars :: [(Def, Maybe Exp)]
		pars = linkDefPars env'' opars
		
		overrideDef = findOverridenDef env dd	
		pars'' :: [(Def, Maybe Exp)]
		pars'' = case overrideDef of
			Nothing -> filterSelfPar pars
			Just od -> map checkParTypeAndWrapIfNeeded $ zip (filterSelfPar pars) (defPars od)
			where
				filterSelfPar ((Def{defName = "self"}, _):xs) = xs
				filterSelfPar d = d

		checkParTypeAndWrapIfNeeded :: ((Def, Maybe Exp), Def) -> (Def, Maybe Exp)
		checkParTypeAndWrapIfNeeded (p@(thisPar@Def{defType = thisTp}, de), Def{defType = superTp})
			| isInstanceOfTp env thisTp superTp = case (thisTp, superTp) of
				(TPGenericWrap{}, TPGenericWrap{}) -> p
				(_, TPGenericWrap{}) -> (thisPar{defType = wrapGeneric thisTp}, de)
				_ -> p
			| otherwise = (thisPar{defType = TPUnknown $ "Incorrect override: " ++ show thisTp ++ " is not instance of " ++ show superTp }, de)
		defGenerics' = Just $ DefGenerics generics' $ case pars of
		 	[] -> envSelf env
		 	(Def{defName = dn, defType = dtp}, _) : _ -> if dn == "self" then dtp else envSelf env
		mods' = translateMods mods ++ if isInit then [] else checkOverrideMods mods overrideDef 
		

		overrideTp = fmap defType overrideDef
		needWrapRetType = maybe False isTpGeneric overrideTp
		isTpGeneric (TPClass TPMGeneric _ _) = True
		isTpGeneric (TPGenericWrap _ _) = True
		isTpGeneric _ = False

		mapOverrideType rtp = 
			let rtp' = if needWrapRetType then wrapGeneric rtp else rtp
		 	in case overrideTp of
		 		Just otp -> 
		 			if isInstanceOfTp env' rtp' otp then (if isJust tp || (null opars && name == "apply") then rtp' else otp) else
		 				TPUnknown $ "Could not choose correct datatype for override " ++ show rtp' ++ " is not instance of " ++ show otp
		 		_ -> rtp'
		parDefs = map fst pars''
		env'' = envAddVals parDefs env'
		isSelfStub = case envSelf env of
			TPClass _ _ cl -> isStub cl
			TPObject _ cl -> isStub cl
		mainDef = (case body of
			D.Nop -> Def {defMods = DefModDef : mods' ++ [DefModAbstract | not isSelfStub] ++ additionalMods, 
					defName = name, defGenerics = defGenerics',
					defPars = parDefs,
					defType = dataType env' (fromMaybe (D.DataType "void" []) tp), defBody = Nop} 
			_   -> 
				let 
					b = case tp of
						Just (D.DataType "void" []) -> expr env'' body
						_ -> exprToSome env'' body
					tp' = unblockGenerics $ unwrapGeneric $ getDataType env' tp b
					tp'' = mapOverrideType tp'
					superInitDef :: Maybe Def
					superInitDef = join $ fmap classInitDef $ superClass (envSelfClass env)
					callSuperInit :: Exp
					callSuperInit = Dot (Super $ fromJust $ superType $ envSelf env) $ call (fromJust superInitDef) []
					addSuperInit e
						| isInit && isJust superInitDef = case e of
							Braces es -> Braces $ callSuperInit : es
							_ -> Braces [callSuperInit, e]
						| otherwise = e
				in Def {defMods = DefModDef : mods' ++ additionalMods, defName = name, defGenerics = defGenerics',
					defPars = parDefs,
					defType = tp'', defBody = addSuperInit $ maybeAddReturn env tp'' b})

resolveDefPar :: Env -> Def -> [(Def, Maybe Exp)] -> [Def]
--resolveDefPar _ Def{defName = dn} _ | trace ("resolveDefPar: " ++ dn) False = undefined
resolveDefPar env mainDef parameters = parRec True parameters []
	where
		parRec :: Bool -> [(Def, Maybe Exp)] -> [(Def, Maybe Exp)]  -> [Def]
		--parRec _ ((Def{defName = name}):_) _ | trace ("parRec: " ++ name) False = undefined
		--parRec _ [] _ | trace ("parRec: []") False = undefined
		parRec True [] _ = [mainDef]
		parRec False [] recPars = [makeDef $ reverse recPars]
		parRec isMainDef ((par, defaultExp):xs) recPars = 
			parRec isMainDef xs ((par, Nothing):recPars) 
			++ case defaultExp of 
				Nothing -> []
				_ -> parRec False xs ((par, defaultExp):recPars)
		makeDef :: [(Def, Maybe Exp)] -> Def
		makeDef pars =  
			let 
				callMainDef = call mainDef (map expCallPar pars)
				callMainDef' = maybeAddReturn env (defType mainDef) $ 
					if DefModConstructor `elem` defMods mainDef then callMainDef 
					else Dot (Self (envSelf env)) $ callMainDef
				in mainDef {defMods = (map (\m -> if m == DefModConstructor then DefModDef else m) . filter (\m -> m /= DefModAbstract) ) (defMods mainDef),
					defPars = (map fst . filter ( isNothing . snd)) pars,
					defBody = callMainDef'}  
		expCallPar :: (Def, Maybe Exp) -> Exp
		expCallPar (d, Nothing) = callRef d
		expCallPar (_, Just b) = b

linkDefPars :: Env -> [D.Par] -> [(Def, Maybe Exp)]
--linkDefPars _ _ | trace ("linkDefPars: ") False = undefined
linkDefPars env pars = let 
	pars' = map linkPar pars 
	--env' = envAddVals (map fst pars') env
	parMod D.ParModWeak = DefModWeak
	linkPar D.Par {D.parMods = mods, D.parName = pnm, D.parType  = ttt, D.parDefault = pd} = let 
		tp = fmap (dataType env) ttt
		tp' = fromMaybe (exprDataType defaultExp) tp
		defaultExp = if isJust tp then exprTo env (fromJust tp) pd else exprToSome env pd
		in (Def {
			defName = pnm, defPars = [], 
			defType = tp', 
			defBody = Nop, 
			defMods = DefModLocal : map parMod mods, defGenerics = Nothing}, 
			case pd of
				D.Nop -> Nothing
				_ -> Just $ defaultExp)
	in pars'

{------------------------------------------------------------------------------------------------------------------------------ 
 - Env 
 ------------------------------------------------------------------------------------------------------------------------------}

type ClassIndex = M.Map String Class
type ObjectIndex = [Class]
data Env = Env{envSelf :: DataType, envIndex :: ClassIndex, envObjectIndex :: ObjectIndex, envVals :: [Def], envInit :: Bool, envTp :: DataType, envVarSuffix :: String}

envChangeDefTp :: Env -> Def -> DataType -> Env
envChangeDefTp env@Env{envVals = vals} d tp = env{envVals = d{defType = tp} : filter (/= d) vals}
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
classFind cidx name = fromMaybe (ClassError name ("Class " ++ name ++ " not found") ) $ idxFind cidx name
{------------------------------------------------------------------------------------------------------------------------------ 
 - DataType 
 ------------------------------------------------------------------------------------------------------------------------------}

data DataType = TPNumber Bool Int | TPFloatNumber Int | TPString | TPVoid 
	| TPClass {tpMod :: DataTypeMod, tpGenerics :: [DataType], tpClass :: Class}
	| TPEArr Int DataType | TPAny | TPChar
	| TPArr Int DataType | TPBool | TPFun DataType DataType | TPTuple [DataType] | TPSelf Class | TPUnknown String 
	| TPMap DataType DataType
	| TPOption Bool DataType -- Currently in option state. If already checked than True
	| TPGenericWrap [WrapReason] DataType | TPNil | TPObject {tpMod :: DataTypeMod, tpClass :: Class} | TPThrow
	| TPAnyGeneric | TPUnset | TPPointer DataType
	deriving (Eq)
data WrapReason = WrapReasonUp | WrapReasonBlock deriving(Eq) 
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
isTpOption :: DataType -> Bool
isTpOption TPOption{} = True
isTpOption _ = False
isTpClass :: DataType -> Bool
isTpClass (TPClass TPMClass _ _) = True
isTpClass _ = False
isTpEnum :: DataType -> Bool
isTpEnum (TPClass TPMEnum _ _) = True
isTpEnum _ = False
isTpTrait :: DataType -> Bool
isTpTrait (TPClass TPMTrait _ _) = True
isTpTrait _ = False
isTpStruct :: DataType -> Bool
isTpStruct (TPClass TPMStruct _ _) = True
isTpStruct _ = False
isTpBaseClass :: DataType -> Bool
isTpBaseClass (TPClass TPMClass _ Class{className = "Object"}) = True
isTpBaseClass _ = False

unoptionIfChecked :: DataType -> DataType
unoptionIfChecked (TPGenericWrap _ (TPOption True d)) = d
unoptionIfChecked (TPOption True d) = d
unoptionIfChecked d = d

unoptionHard :: DataType -> DataType
unoptionHard TPVoid = TPVoid
unoptionHard (TPOption _ o) = o
unoptionHard (TPGenericWrap _ (TPOption _ o)) = o
unoptionHard tp = TPUnknown $ show tp ++ " is not option"

option :: Bool -> DataType -> DataType
option _ TPVoid = TPVoid
option ch o@(TPOption och tp)
	| ch == och = o
	| otherwise = TPOption (och && ch) tp
option ch (TPGenericWrap w tp) = TPGenericWrap w $ option ch tp
option ch tp = TPOption ch $ wrapGeneric tp

forDataType :: MonadPlus m => (DataType -> m a) -> DataType -> m a
forDataType f tp = mplus (go tp) (f tp)
	where
		go (TPClass _ gens _) = msum $ map (forDataType f) gens
		go (TPArr _ a) = forDataType f a
		go (TPEArr _ a) = forDataType f a
		go (TPFun a b) = mplus (forDataType f a) (forDataType f b)
		go (TPMap a b) = mplus (forDataType f a) (forDataType f b)
		go (TPGenericWrap _ a) = forDataType f a
		go (TPOption _ a) = forDataType f a
		go (TPPointer a) = forDataType f a
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
		go (TPGenericWrap t a) = TPGenericWrap t (mapDataType f a)
		go (TPPointer a) = TPPointer (mapDataType f a)
		go (TPOption ch a) = TPOption ch (mapDataType f a)
		go (TPTuple a) = TPTuple (map (mapDataType f) a)
		go _ = tp

mapDataTypeGenerics :: ([DataType] -> [DataType]) -> DataType -> DataType
mapDataTypeGenerics f (TPGenericWrap reasons tp) = TPGenericWrap reasons $ mapDataTypeGenerics f tp
mapDataTypeGenerics f (TPClass mods gens cl) = TPClass mods (f gens) cl
mapDataTypeGenerics f (TPTuple gens) = TPTuple (f gens)
mapDataTypeGenerics f (TPOption ch gen) = TPOption ch (head $ f [gen])
mapDataTypeGenerics f (TPPointer gen) = TPPointer (head $ f [gen])
mapDataTypeGenerics f (TPEArr count gen) = TPEArr count (head $ f [gen])
mapDataTypeGenerics f (TPArr count gen) = TPArr count (head $ f [gen])
mapDataTypeGenerics f (TPFun s d) = 
	let s':d':_ = f [s, d] 
	in TPFun s' d'
mapDataTypeGenerics f (TPMap k v) = 
	let k':v':_ = f [k, v] 
	in TPMap k' v'
mapDataTypeGenerics _ tp = tp



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
dataTypeClass env (TPObject _ c) = Class { _classMods = [ClassModObject], className = className c, 
	_classExtends = Extends (if className c == "Object" then Nothing else Just $ baseClassExtends (envIndex env)) [], 
	_classDefs = allDefsInObject (c, M.empty), _classGenerics = [], _classImports = [],
	_classFile = fromMaybe (error $ "No class file for class " ++ className c) $ classFile c,
	_classPackage = classPackage c, classAnnotations = []}
dataTypeClass env (TPGenericWrap _ c) = dataTypeClass env c
dataTypeClass _ (TPSelf c) = c
dataTypeClass env (TPArr _ _) = classFind (envIndex env) "ImArray"
dataTypeClass env (TPEArr _ _) = classFind (envIndex env) "PArray"
dataTypeClass env (TPMap _ _) = classFind(envIndex env) "ImMap"
dataTypeClass env (TPTuple [_, _]) = classFind (envIndex env) "Tuple"
dataTypeClass env (TPNumber False 1) = classFind (envIndex env) "Byte"
dataTypeClass env (TPNumber True 1) = classFind (envIndex env) "UByte"
dataTypeClass env (TPNumber False 0) = classFind (envIndex env) "Int"
dataTypeClass env (TPNumber True 0) = classFind (envIndex env) "UInt"
dataTypeClass env (TPNumber False 4) = classFind (envIndex env) "Int4"
dataTypeClass env (TPNumber True 4) = classFind (envIndex env) "UInt4"
dataTypeClass env (TPNumber False 8) = classFind (envIndex env) "Int8"
dataTypeClass env (TPNumber True 8) = classFind (envIndex env) "UInt8"
dataTypeClass env (TPFloatNumber 4) = classFind (envIndex env) "Float4"
dataTypeClass env (TPFloatNumber 8) = classFind (envIndex env) "Float8"
dataTypeClass env (TPFloatNumber 0) = classFind (envIndex env) "Float"
dataTypeClass env TPChar = classFind (envIndex env) "Char"
dataTypeClass env TPString = classFind (envIndex env) "String"
dataTypeClass env TPAny = classFind (envIndex env) "Any"
dataTypeClass env TPBool = classFind (envIndex env) "Bool"
dataTypeClass env (TPPointer _) = classFind (envIndex env) "Pointer"
dataTypeClass env (TPTuple a) = classFind (envIndex env) ("Tuple" ++ show (length a))
dataTypeClass env f@TPFun{} = Class { _classMods = [], className = "", _classExtends =  Extends (Just $ baseClassExtends (envIndex env)) [],
	_classPackage = Package ["core"] Nothing "", _classFile = coreFakeFile, 
	_classDefs = [applyLambdaDef f], _classGenerics = [], _classImports = [], classAnnotations = []}
	where
		
dataTypeClass _ x = ClassError (show x) ("No dataTypeClass for " ++ show x)

applyLambdaDef :: DataType -> Def
applyLambdaDef (TPFun stp dtp) = Def {defName = "apply", defPars = map (localVal "") sourceTypes, defType = dtp, defBody = Nop, defMods = [DefModApplyLambda], defGenerics = Nothing}
	where
		sourceTypes = case stp of
			TPVoid -> []
			TPTuple tps -> tps
			tp -> [tp]

dataTypeClassName :: DataType -> String
dataTypeClassName (TPClass _ _ c ) = className c
dataTypeClassName (TPObject _ c) = className c
dataTypeClassName (TPGenericWrap _ c) = dataTypeClassName c
dataTypeClassName (TPArr _ _) = "Array"
dataTypeClassName (TPMap _ _) = "Map"
dataTypeClassName TPAny = "Any"
dataTypeClassName (TPPointer _) = "Pointer"
dataTypeClassName (TPTuple [_, _]) = "Tuple"
dataTypeClassName (TPTuple a) = "Tuple" ++ show (length a)
dataTypeClassName x = error ("No dataTypeClassName for " ++ show x)


dataTypeClassNameWithPrefix :: DataType -> String
dataTypeClassNameWithPrefix (TPClass _ _ c ) = classNameWithPrefix c
dataTypeClassNameWithPrefix (TPObject _ c) = classNameWithPrefix c
dataTypeClassNameWithPrefix (TPGenericWrap _ c) = dataTypeClassNameWithPrefix c
dataTypeClassNameWithPrefix (TPArr _ _) = "CNArray"
dataTypeClassNameWithPrefix (TPMap _ _) = "CNMap"
dataTypeClassNameWithPrefix TPAny = "ODAny"
dataTypeClassNameWithPrefix (TPTuple [_, _]) = "CNTuple"
dataTypeClassNameWithPrefix (TPTuple a) = "CNTuple" ++ show (length a)
dataTypeClassNameWithPrefix x = error ("No dataTypeClassNameWithPrefix for " ++ show x)

resolveTypeAlias :: DataType -> DataType
resolveTypeAlias tp@(TPClass TPMType _ c) = fromMaybe (TPUnknown $ "No super type for type " ++ className c) $ superType tp
resolveTypeAlias tp = tp

dataTypeGenerics :: Env -> DataType -> [DataType]
dataTypeGenerics _ (TPClass _ g _) = g
dataTypeGenerics _ (TPArr _ g) = [g]
dataTypeGenerics _ (TPEArr _ g) = [g]
dataTypeGenerics _ (TPFun s d) = [s, d]
dataTypeGenerics _ (TPMap k v) = [k, v]
dataTypeGenerics _ (TPOption _ v) = [v]
dataTypeGenerics _ (TPTuple a) = a
dataTypeGenerics _ (TPPointer a) = [a]
dataTypeGenerics env (TPGenericWrap _ g) = dataTypeGenerics env g
dataTypeGenerics _ _ = []

isGenericWrap :: DataType -> Bool
isGenericWrap TPGenericWrap{} = True
isGenericWrap _ = False
wrapGeneric :: DataType -> DataType
wrapGeneric g@(TPClass TPMGeneric _ _) = g
wrapGeneric g@TPGenericWrap{} = g
wrapGeneric g = TPGenericWrap [WrapReasonUp] g
unwrapGeneric :: DataType -> DataType
unwrapGeneric (TPGenericWrap _ g)= g
unwrapGeneric g = g

dataType :: Env -> D.DataType -> DataType
dataType env (D.DataType name gens) = case name of
	"byte" -> byte
	"Byte" -> wrapGeneric byte
	"ubyte" -> ubyte
	"UByte" -> wrapGeneric ubyte
	"int" -> int
	"Int" -> wrapGeneric int
	"uint" -> uint
	"UInt" -> wrapGeneric uint
	"int4" -> int4
	"Int4" -> wrapGeneric int4
	"uint4" -> uint4
	"UInt4" -> wrapGeneric uint4
	"int8" -> int8
	"Int8" -> wrapGeneric int8
	"uint8" -> uint8
	"UInt8" -> wrapGeneric uint8
	"float4" -> float4
	"Float4" -> wrapGeneric float4
	"float8" -> float8
	"Float8" -> wrapGeneric float8
	"float" -> float
	"Float" -> wrapGeneric float
	"char" -> TPChar
	"Char" -> wrapGeneric TPChar
	"void" -> TPVoid
	"string" -> TPString
	"bool" -> TPBool
	"self" -> TPSelf $ dataTypeClass env $ envSelf env
	"any" -> TPAny
	"Pointer" -> TPPointer $ case gens of
		[tp] -> dataType env tp
		_ -> TPUnknown $ "Incorrect generics for pointer: " ++ show gens
	"_" -> TPAnyGeneric
	_ -> maybe (TPUnknown $ "No class found " ++ name) (\cl -> refDataType cl (map (wrapGeneric . dataType env) gens)) (idxFind (envIndex env) name)
dataType env (D.DataTypeArr m tp) = case tp' of
		TPClass TPMStruct _ _ -> arrr'
		TPNumber _ _ -> arrr'
		TPChar -> arrr'
		TPFloatNumber _  -> arrr'
		TPBool -> arrr'
		TPVoid -> earr
		_ -> arrr
	where
		tp' = dataType env tp
		arrr = TPArr m $ wrapGeneric tp'
		arrr' =  if m == 0 then arrr else earr
		earr = TPEArr m tp'
dataType env (D.DataTypeMap k v) = TPMap (wrapGeneric $ dataType env k) (wrapGeneric $ dataType env v)
dataType env (D.DataTypeFun (D.DataTypeTuple []) d) = TPFun TPVoid (dataType env d)
dataType env (D.DataTypeFun (D.DataTypeTuple [s]) d) = TPFun (dataType env s) (dataType env d)
dataType env (D.DataTypeFun (D.DataTypeTuple tps) d) = TPFun (TPTuple $ map (dataType env) tps) (dataType env d)
dataType env (D.DataTypeFun s d) = TPFun (dataType env s) (dataType env d)
dataType env (D.DataTypeTuple tps) = TPTuple $ map (wrapGeneric . dataType env) tps
dataType env (D.DataTypeOption t) = TPOption False $ (wrapGeneric . dataType env) t


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
	show TPChar = "char"
	show TPBool = "bool"
	show (TPSelf cl) = "self<" ++ className cl ++ ">"
	show TPNil = "nil"
	show TPThrow = "throw"
	show TPAnyGeneric = "_"
	show TPAny = "any"
	show (TPUnknown s) = s
	show (TPClass t [] c) = className c ++ show t
	show (TPObject t c) = className c ++ show t ++ ".class"
	show (TPClass t gens c) = className c ++ show t ++ "<" ++ strs' ", " gens ++ ">"
	show (TPGenericWrap [WrapReasonUp] c) = '^' : show c
	show (TPGenericWrap [WrapReasonBlock] c) = '§' : show c ++ "§"
	show (TPGenericWrap _ c) = "§^" ++ show c ++ "§"
	show (TPArr 0 t) = "[" ++ show t ++ "]"
	show (TPArr s r) = show r ++ "[" ++ show s ++ "]"
	show (TPEArr s r) = "*" ++ show r ++ "[" ++ show s ++ "]"
	show (TPMap k v) = "[" ++ show k ++ " : " ++ show v ++ "]"
	show (TPFun s d) = show s ++ " -> " ++ show d
	show (TPTuple tps) = "(" ++ strs' ", " tps ++ ")"
	show (TPOption False t) = "(" ++ show t ++ ")?"
	show (TPOption True t) = "(" ++ show t ++ ")¿"
	show TPUnset = "_unset"
	show (TPPointer e) = show e ++ "*"
	show _ =  "UnknownTP"
instance Show DataTypeMod where
	show TPMClass = "#C"
	show TPMType = "#P"
	show TPMStruct = "#S"
	show TPMEnum = "#E"
	show TPMTrait = "#T"
	show TPMGeneric = "#G"
	
getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType env) tp


tpGeneric :: DataType
tpGeneric = TPClass TPMGeneric [] (Generic "?" [])

objectType :: DataType -> DataType
objectType (TPClass t _ cl) = TPObject t cl
objectType e = TPUnknown $ "No object type for type " ++ show e

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
	| Weak Exp
	| Synchronized Exp Exp
	| Do Exp Exp
	| Self DataType
	| Super DataType
	| Nil
	| Null DataType
	| BoolOp BoolTp Exp Exp
	| MathOp MathTp Exp Exp
	| PlusPlus Exp
	| MinusMinus Exp
	| Dot Exp Exp
	| Arrow Exp Exp
	| NullDot Exp Exp
	| NonOpt Bool Exp Exp -- Need to check. The second is expanded expression
	| Set (Maybe MathTp) Exp Exp
	| Call Def DataType [CallPar] [DataType]
	| Return Bool Exp -- Required return
	| Index Exp Exp
	| Lambda [(String, DataType)] Exp DataType
	| Val Bool Def -- Separate declaraion and initialization if true
	| ExpDError String D.Exp 
	| ExpLError String Exp 
	| ExpError String 
	| FirstTry D.Exp Exp 
	| Arr [Exp]
	| Map [(Exp, Exp)]
	| Tuple [Exp]
	| Some Bool Exp -- Return checked or unchecked option
	| None DataType
	| Throw Exp
	| Not Exp
	| Negative Exp
	| Cast DataType Exp
	| As DataType
	| Is DataType
	| CastDot DataType
	| Try Exp Exp
	| Break
	| Continue
	| LambdaCall Exp
	| Deferencing Exp
	| StringBuild [(String, Exp)] String
	deriving(Eq)
type CallPar = (Def, Exp)	

instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show (While cond e) = "while(" ++ show cond ++ ") " ++ show e
	show (Weak e) = "weak " ++ show e
	show (Synchronized cond e) = "synchronized(" ++ show cond ++ ") " ++ show e
	show (Do cond e) = "do" ++ show e ++ " while(" ++ show cond ++ ")"
	show Nop = ""
	show (Self c) = "<" ++ show c ++ ">self"
	show (Super c) = "<" ++ show c ++ ">super"
	show (Return _ e) = "return " ++ show e
	show (Set Nothing l r) = "(" ++ showOp l "=" r ++ ")"
	show (Set (Just t) l r) = "(" ++ showOp l (show t ++ "=") r ++ ")"
	show (BoolOp t l r) = "(" ++ showOp l (show t) r ++ ")"
	show (MathOp t l r) = "(" ++ showOp l (show t) r ++ ")"
	show (PlusPlus e) = show e ++ "++"
	show (MinusMinus e) = show e ++ "--"
	show (Dot l r) = showOp' l "." r
	show (Arrow l r) = showOp' l "->" r
	show (NullDot l r) = showOp' l "?." r
	show (Call dd tp pars _) = defRefPrep dd ++ defName dd ++ showCallPars pars ++ "\\" ++ show tp ++ "\\" 
	show (IntConst i) = show i
	show (StringConst i) = show i
	show Nil = "nil"
	show (BoolConst i) = show i
	show (FloatConst i) = show i
	show (Index e i) = show e ++ "[" ++ show i ++ "]"
	show (Lambda pars e tp) = strs ", " (map (\(n, t) -> n ++ " : " ++ show t) pars) ++ " -> " ++ show tp ++ " = " ++ show e
	show (Val False d) = show d
	show (Val True d) = "val " ++ defName d ++ " : " ++ show (defType d) ++ "\n" ++ show (defBody d)
	show (ExpDError s e) = "<#" ++ show e ++ ": " ++ (strs " " . lines) s ++ "#>"
	show (ExpError s) = "<#" ++ (strs " " . lines) s ++ "#>"
	show (ExpLError s e) = "<#" ++ show e ++ ": " ++ (strs " " . lines) s ++ "#>"
	show (Arr exps) = "["  ++ strs' ", " exps ++ "]"
	show (Map exps) = "["  ++ strs' ", " exps ++ "]"
	show (Tuple exps) = "("  ++ strs' ", " exps ++ ")"
	show ee@(Some _ e) = "some(" ++ show e ++ ")\\" ++ show (exprDataType ee) ++ "\\"
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
	show (Continue) = "continue"
	show (Null tp) = "null<" ++ show tp ++ ">"
	show (NonOpt _ e _) = show e ++ ".get"
	show (LambdaCall e) = show e ++ "()"
	show (Deferencing e) = "*(" ++ show e ++ ")"
	show (Try e f) = "try " ++ show e ++ "\nfinally " ++ show f
	show (StringBuild pars lastS) = "\"" ++ join (map (\(prev, e) -> prev ++ "$" ++ show e) pars) ++ lastS ++ "\""

extractStringConst :: Exp -> Maybe String
extractStringConst (StringConst s) = Just s
extractStringConst _ = Nothing

callRef :: Def -> Exp
callRef d = Call d (defType d) [] []

call :: Def -> [Exp] -> Exp
call d pars = Call d (defType d) (zip (defPars d) pars) []

showCallPars :: [CallPar] -> String
showCallPars [] = ""
showCallPars pars = "(" ++ strs ", " (map showPar pars) ++ ")"
	where showPar (Def {defName = name}, e) = name ++ " = " ++ show e
	
callLocalVal :: String -> DataType -> Exp
callLocalVal name tp = Call (localVal name tp) tp [] []

maybeAddReturn :: Env -> DataType -> Exp -> Exp
--maybeAddReturn _ tp _ | trace ("r " ++ show tp) False = undefined
maybeAddReturn _ TPVoid e = e
maybeAddReturn env tp e  = let
	mbNil = case unwrapGeneric $ exprDataType e of
		TPVoid -> case e of
			Braces es -> Braces (es ++ [Return False Nil]) 
			_ -> Braces (e : [Return False Nil]) 
		_ -> addReturn env True tp e
	in case unwrapGeneric tp of
		TPClass TPMGeneric _ _ -> mbNil 
		TPVoid -> mbNil 
		TPUnknown{} -> mbNil 
		_ -> addReturn env True tp e

addReturn :: Env -> Bool -> DataType -> Exp -> Exp 
addReturn env hard tp ee = addReturnBy defBy hard ee
	where
		defBy h e = Return h $ implicitConvertsion env tp e


addReturnBy :: (Bool -> Exp -> Exp) -> Bool -> Exp -> Exp
addReturnBy by hard (Weak e) = Weak (addReturnBy by hard e)
addReturnBy by hard (If cond t f) = If cond (addReturnBy by hard t) (addReturnBy by hard f)
addReturnBy by hard (Synchronized r b) = Synchronized r (addReturnBy by hard b)
addReturnBy by hard (Try e f) = Try (addReturnBy by hard e) f
addReturnBy _ True e@(Braces []) = ExpLError "Return empty braces" e
addReturnBy by hard (Braces es) = Braces $ map (addReturnBy by False) (init es) ++ [addReturnBy by hard (last es)]
addReturnBy _ True Nop = ExpLError "Return NOP" Nop
addReturnBy _ _ e@(Throw _) = e
addReturnBy by _ (Return _ e) = by True e
addReturnBy by True e = by False e
addReturnBy _ _ e = e

forExp :: MonadPlus m => (Exp -> m a) -> Exp -> m a
forExp f ee = mplus (go ee) (f ee)
	where
		go (Braces es) = msum $ map (forExp f) es
		go (StringBuild pars _) = msum $ map (forExp f . snd) pars
		go (Arr es) = msum $ map (forExp f) es
		go (Tuple es) = msum $ map (forExp f) es
		go (Map es) = msum $ map (forExp f *** forExp f >>> uncurry mplus) es
		go (Call _ _ pars _) = msum $ map (forExp f . snd) pars
		go (If cond te fe) =  mplus (forExp f cond) $ mplus (forExp f te) (forExp f fe)
		go (BoolOp _ l r) = mplus (forExp f l) (forExp f r)
		go (MathOp _ l r) = mplus (forExp f l) (forExp f r)
		go (While l r) = mplus (forExp f l) (forExp f r)
		go (Try l r) = mplus (forExp f l) (forExp f r)
		go (Weak e) =forExp f e
		go (Synchronized l r) = mplus (forExp f l) (forExp f r)
		go (Do l r) = mplus (forExp f l) (forExp f r)
		go (Set _ l r) = mplus (forExp f l) (forExp f r)
		go (Dot l r) = mplus (forExp f l) (forExp f r)
		go (Arrow l r) = mplus (forExp f l) (forExp f r)
		go (NullDot l r) = mplus (forExp f l) (forExp f r)
		go (Index l r) = mplus (forExp f l) (forExp f r)
		go (PlusPlus e) = forExp f e
		go (MinusMinus e) = forExp f e
		go (Return _ e) = forExp f e
		go (Cast _ e) = forExp f e
		go (Some _ e) = forExp f e
		go (Throw e) = forExp f e
		go (Not e) = forExp f e
		go (Negative e) = forExp f e
		go (Lambda _ e _) = forExp f e
		go (LambdaCall e) = forExp f e
		go (FirstTry _ e) = forExp f e
		go (NonOpt _ e u) = mplus (forExp f e) (forExp f u)
		go (Deferencing e) = forExp f e
		go (Val _ d) = forExp f (defBody d)
		go _ = mzero

mapExp :: (Exp -> Maybe Exp) -> Exp -> Exp
mapExp f ee = fromMaybe (go ee) (f ee)
	where
		go (Braces es) = Braces $ map (mapExp f) es
		go (StringBuild pars p) = StringBuild (map (second (mapExp f)) pars) p
		go (Arr es) = Arr $ map (mapExp f) es
		go (Tuple es) = Tuple $ map (mapExp f) es
		go (Map es) = Map $ map (mapExp f *** mapExp f) es
		go (Call p1 p2 pars gens) = Call p1 p2 (map (second $ mapExp f) pars) gens
		go (If cond te fe) =  If (mapExp f cond) (mapExp f te) (mapExp f fe)
		go (BoolOp tp l r) = BoolOp tp (mapExp f l) (mapExp f r)
		go (MathOp tp l r) = MathOp tp (mapExp f l) (mapExp f r)
		go (While l r) = While (mapExp f l) (mapExp f r)
		go (Try l r) = Try (mapExp f l) (mapExp f r)
		go (Weak e) = Weak $ mapExp f e
		go (Synchronized l r) = Synchronized (mapExp f l) (mapExp f r)
		go (Do l r) = Do (mapExp f l) (mapExp f r)
		go (Set t l r) = Set t (mapExp f l) (mapExp f r)
		go (Dot l r) = Dot (mapExp f l) (mapExp f r)
		go (Arrow l r) = Arrow (mapExp f l) (mapExp f r)
		go (NullDot l r) = NullDot (mapExp f l) (mapExp f r)
		go (Index l r) = Index (mapExp f l) (mapExp f r)
		go (PlusPlus e) = PlusPlus $ mapExp f e
		go (MinusMinus e) = MinusMinus $ mapExp f e
		go (Return p e) = Return p $ mapExp f e
		go (Cast t e) = Cast t $ mapExp f e
		go (Some ch e) = Some ch $ mapExp f e
		go (Throw e) = Throw $ mapExp f e
		go (Not e) = Not $ mapExp f e
		go (Negative e) = Negative $ mapExp f e
		go (Lambda p1 e p2) = Lambda p1 (mapExp f e) p2
		go (FirstTry p e) = FirstTry p $ mapExp f e
		go (NonOpt c e u) = NonOpt c (mapExp f e) (mapExp f u)
		go (Deferencing e) = Deferencing $ mapExp f e
		go (LambdaCall e) = LambdaCall $ mapExp f e
		go (Val e d) = Val e d{defBody = mapExp f (defBody d)}
		go e = e

isNop :: Exp -> Bool
isNop Nop = True
isNop _ = False

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
isConst (Arrow l r) = isConst l && isConst r
isConst (NullDot l r) = isConst l && isConst r
isConst (Call Def {defMods = mods, defBody = b} _ pars _) = 
	(DefModStruct `elem` mods  &&  DefModConstructor `elem` mods && all (isConst . snd) pars)
	|| (DefModObject `elem` mods && null pars)
	|| (DefModStub `elem` mods  &&  DefModGlobalVal `elem` mods)
	|| (DefModStatic `elem` mods  &&  DefModField `elem` mods && isNop b)
isConst (As _) = True
isConst (Is _) = True
isConst (CastDot _) = True
isConst (NonOpt _ e _) = isConst e
isConst Nop = True
isConst _ = False

isSimpleExpression :: Exp -> Bool
isSimpleExpression (If _ l r) = isSimpleExpression l && isSimpleExpression r
isSimpleExpression (Braces _) = False
isSimpleExpression (Throw _) = False
isSimpleExpression _ = True

isElementaryExpression :: Exp -> Bool
isElementaryExpression (IntConst _) = True
isElementaryExpression (FloatConst _) = True
isElementaryExpression (BoolConst _) = True
isElementaryExpression (Call Def{defMods = mods} _ [] _) = DefModDef `notElem` mods
isElementaryExpression (Dot (Self _) (Call Def{defMods = mods} _ [] _)) = DefModDef `notElem` mods
isElementaryExpression (Self _) = True
isElementaryExpression _ = False


isError :: Exp -> Bool
isError ExpDError{} = True
isError ExpLError{} = True
isError ExpError{} = True
isError _ = False


exprDataType :: Exp -> DataType
exprDataType (If _ _ Nop) = TPVoid
exprDataType (If _ t _) = exprDataType t
exprDataType (While _ _) = TPVoid
exprDataType (Try e _) = exprDataType e
exprDataType (Weak e) = exprDataType e
exprDataType (Synchronized _ r) = exprDataType r
exprDataType (Do _ _) = TPVoid
exprDataType (Braces []) = TPVoid
exprDataType (Braces es) = exprDataType $ last es
exprDataType (Nop) = TPVoid
exprDataType (IntConst _ ) = int
exprDataType (StringConst _ ) = TPString
exprDataType Nil = TPNil
exprDataType (NonOpt _ e _) = unwrapGeneric $ unoptionHard $ exprDataType e
exprDataType (BoolConst _ ) = TPBool
exprDataType (FloatConst _) = float
exprDataType (BoolOp {}) = TPBool
exprDataType (MathOp _ l r) = case(unwrapGeneric $ exprDataType l, unwrapGeneric $ exprDataType r) of
	(TPNumber _ _, rtp@TPFloatNumber{}) -> rtp
	(lt, _) -> lt
exprDataType (PlusPlus e) = exprDataType e
exprDataType (MinusMinus e) = exprDataType e
exprDataType (NullDot _ b) = option False $ exprDataType b 
exprDataType (Dot _ b) = exprDataType b
exprDataType (Arrow _ b) = exprDataType b
exprDataType Set{} = TPVoid
exprDataType (Self s) = s
exprDataType (Super s) = s
exprDataType (Call _ t _ _) = t
exprDataType (Return _ e) = exprDataType e
exprDataType (Index e i) = resolve $ exprDataType e 
	where  
		resolve (TPArr _ t) = t
		resolve (TPEArr _ t) = t
		resolve (TPMap _ v) = v
		resolve (TPObject TPMEnum c) = TPClass TPMEnum [] c
		resolve (TPGenericWrap _ t) = resolve t
		resolve t = TPUnknown $ show t ++ " is not array " ++ show e ++ "[" ++ show i ++ "]"
exprDataType (Lambda pars _ r) = TPFun (parsTp pars) r
	where 
		parsTp :: [(String, DataType)] -> DataType
		parsTp [] = TPVoid
		parsTp [(_, tp)] = tp
		parsTp ps = TPTuple $ map snd ps
exprDataType e@(ExpDError _ _) = TPUnknown $ show e
exprDataType e@(ExpError _) = TPUnknown $ show e
exprDataType e@(ExpLError _ _) = TPUnknown $ show e
exprDataType (Arr []) = TPArr 0 TPUnset
exprDataType (Map []) = TPMap TPUnset TPUnset
exprDataType (Arr exps) = TPArr (length exps) $ wrapGeneric $ exprDataType $ head exps
exprDataType (Map exps) = let (k, v) = ((exprDataType >>> wrapGeneric) *** (exprDataType >>> wrapGeneric)) $ head exps 
	in TPMap k v
exprDataType (Tuple exps) = TPTuple $ map (wrapGeneric .exprDataType) exps
exprDataType (Val _ Def{defType = tp}) = tp
exprDataType (Some ch v) = option ch $ exprDataType v
exprDataType (None tp) = option False tp
exprDataType (FirstTry _ e) = exprDataType e
exprDataType (Throw _) = TPThrow
exprDataType (Not _) = TPBool
exprDataType (Negative e) = exprDataType e
exprDataType (Deferencing e) = case exprDataType e of
	TPPointer r -> unwrapGeneric r
	TPGenericWrap _ (TPPointer r) -> unwrapGeneric r
	r -> TPUnknown $ show r ++ " is not a reference"
exprDataType (Cast dtp _) = dtp
exprDataType (As dtp) = option False $ wrapGeneric dtp
exprDataType (CastDot dtp) = dtp
exprDataType (Is _) = TPBool
exprDataType Break = TPVoid
exprDataType (Null tp) = TPPointer tp
exprDataType StringBuild{} = TPString
exprDataType (LambdaCall e) = case unwrapGeneric $ exprDataType e of
	(TPOption True (TPGenericWrap _ (TPFun _ d))) -> d
	(TPOption True (TPFun _ d)) -> d
	(TPFun _ d) -> d
	t -> t
{- exprDataType x = error $ "No exprDataType for " ++ show x -}

maybeCast :: DataType -> Exp -> Exp
maybeCast _ e@Throw{} = e
maybeCast _ e@(Braces []) = e
maybeCast tp (If c t f) = If c (maybeCast tp t) (maybeCast tp f) 
maybeCast tp (Braces x) = Braces $ (init x) ++ [maybeCast tp $ last x]
maybeCast TPNil e = e
maybeCast (TPGenericWrap _ TPNil) e = e
maybeCast _ Nil = Nil
maybeCast (TPOption ch t) e = 
	let tp = unwrapGeneric $ exprDataType e
	in case tp of
		TPOption _ r -> if unwrapGeneric r == unwrapGeneric t then e else Cast tp e
		_ -> Some ch e
maybeCast tp e 
	| unwrapGeneric tp == unwrapGeneric (exprDataType e) = e
	| otherwise = Cast tp e

exprTo :: Env -> DataType -> D.Exp -> Exp
exprTo env tp e = implicitConvertsion env tp $ expr env{envTp = tp} e

exprToSome :: Env ->D.Exp -> Exp
exprToSome env e =  expr env{envTp = baseDataType env} e


expr :: Env -> D.Exp -> Exp
expr env (D.If cond t D.Nop) = let
	cond' = exprTo env TPBool cond 
	env' = optChecking env cond'
	in If (exprTo env TPBool cond) (expr (fst env') t) Nop
expr env (D.If cond t f)
	| envTp env == TPVoid = let
		cond' = exprTo env TPBool cond 
		env' = optChecking env cond'
		in If (exprTo env TPBool cond) (expr (fst env') t) (expr (snd env') f)
	| otherwise = let 
		cond' = exprTo env TPBool cond 
		env' = optChecking env cond'
		t' = expr (fst env') t
		f' = expr (snd env') f
		dt = exprDataType t'
		df = exprDataType f'
		retTp = firstCommonSuperDataType env dt df
	in If cond' (maybeCast retTp t') (maybeCast retTp f')
expr env (D.While cond t) = let
	cond' = exprTo env TPBool cond 
	env' = fst $ optChecking env{envTp = TPVoid} cond'
	in While cond' (expr env' t)
expr env (D.Synchronized cond t) = Synchronized (exprToSome env cond) (expr env t)
expr env (D.Try e f) = Try (expr env e) (expr env f)
expr env (D.Do cond t) = Do (exprTo env TPBool cond) (expr env{envTp = TPVoid} t)
expr env (D.Weak e) = insertWeak (expr env e)
expr _ (D.Braces []) = Nop
expr env (D.Braces es) = Braces $ bracesRec env 0 es
	where
		bracesRec :: Env -> Int -> [D.Exp] -> [Exp]
		bracesRec _  _ [] = []
		bracesRec env' n [x] = [expr env'{envVarSuffix = envVarSuffix env ++ ('_' : show n)} x]
		bracesRec env' n (x:xs) = let 
				x' = expr env'{envVarSuffix = envVarSuffix env ++ ('_' : show n), envTp = TPVoid} x
				env'' = case x' of
					(Val _ d) -> (envAddVals [d] env')
					(Set _ (Call d _ [] _) r) -> case unwrapGeneric (defType d) of
						TPOption cl tpl -> case unwrapGeneric (exprDataType r) of
							TPOption cr _ -> if cr == cl then env' else envChangeDefTp env' d (TPOption cr tpl)
							_ -> env'
						_ -> env'
					_ -> env'
			in x':(bracesRec env'' (n + 1) xs)
expr _ D.Nop = Nop
expr _ (D.IntConst i) = IntConst i
expr _ (D.StringConst i) = StringConst i
expr _ D.Nil = Nil
expr _ (D.BoolConst i) = BoolConst i
expr _ (D.FloatConst s) = FloatConst s
expr env (D.BoolOp tp a b)
	| tp == Or || tp == And = let
		a' = exprTo env TPBool a
		env' = optChecking env a'
		env'' = if tp == Or then snd env' else fst env'
	in BoolOp tp a' (exprTo env'' TPBool b)
expr env (D.BoolOp tp a b) 
	| tp == Eq || tp == NotEq = let
		a' = exprToSome env a
		b' = exprToSome env b
		atp = unwrapGeneric $ exprDataType a'
		btp = unwrapGeneric $ exprDataType b'
	in case (a', b') of
		(Nil, _) -> compareWithNil env tp (b', btp)
		(_, Nil) -> compareWithNil env tp (a', atp)
		_ -> case (atp, btp) of
			(TPOption False _, TPOption False _) -> compareOptions env tp (a', atp) (b', btp)
			(TPOption False _, _) -> compareOptionWithNonOption env tp (a', atp) (b', btp)
			(_, TPOption False _) -> compareOptionWithNonOption env tp (b', btp) (a', atp)
			_ -> BoolOp tp  a' b'
expr env (D.BoolOp tp a b) = BoolOp tp (exprToSome env a) (exprToSome env b)
expr env (D.MathOp tp a b) = 
	let 
		aa = exprToSome env a
		ltp = exprDataType aa
		math = MathOp tp aa (exprToSome env b)
		cll = D.Call (literalDefName $ show tp) (Just [(Nothing, b)]) []
		callOp =  Dot aa $ exprCall env (Just ltp) $ cll
	in case unwrapGeneric ltp of
		TPNumber{} -> math
		TPFloatNumber{} -> math
		TPString{} -> math
		TPPointer{} -> math
		_ -> callOp 
expr env d@(D.Dot a b) = let
	aa = case a of
		D.Call {} -> exprCall env Nothing a
		_ -> exprToSome env a
	aTp' = case exprDataType aa of
		TPOption True t -> t
		TPPointer t -> t
		TPGenericWrap _ (TPOption True t) -> t
		t -> t
	aTp = case exprDataType aa of
		TPOption True t -> t
		TPGenericWrap _ (TPOption True t) -> t
		t -> t
	bb = exprCall env (Just aTp')  b
	def = case aa of
			ExpDError s _ -> ExpDError s d
			_ -> case bb of
				Dot l r -> maybeInlineCall env $ Dot (Dot aa l) r
				_ -> maybeInlineCall env $ Dot aa bb
	in case unwrapGeneric aTp of
		TPOption{} -> linkOptionCall env (a, aa) b
		TPPointer{} -> fromMaybe (Arrow aa bb) $ linkPointerCall env (a, aa) b
		TPObject _ Class{className = "Pointer"} -> fromMaybe def $ linkPointerStatic env b
		_ -> def
expr env d@(D.NullDot _ _) = linkNullDot env d
expr env (D.Set tp a b) = 
	let 
		aa = exprToSome env a
		ltp = case aa of
			NullDot _ r -> exprDataType r
			_ -> exprDataType aa
		simpleSet = if isNothing tp then set Nothing aa math
			else case unwrapGeneric ltp of
				TPNumber{} -> set tp aa math
				TPFloatNumber{} -> set tp aa math
				TPString{} -> set tp aa math
				TPPointer{} -> set tp aa math
				_ -> set Nothing aa callOp

		set tp' (NullDot dl dr) r = 
			let 
				dltp = exprDataType dl
				tmp = tmpVal env "" dltp dl
				ref = if isElementaryExpression dl then dl else callRef tmp
				f = If (BoolOp NotEq ref (None dltp) ) (Set tp' (Dot (nonOpt env False ref) dr) r) Nop
			in  if isElementaryExpression dl then f else Braces [declareVal env tmp, f]
		set tp' l r = if isSimpleExpression r then Set tp' l r else multilineSet env tp' l r

		rtp = case unwrapGeneric ltp of
			 t@TPPointer{} -> if isJust tp then int else t
			 t -> t
		math = exprTo env rtp b 
		callOp = Dot aa $ exprCall env (Just ltp) $ D.Call (literalDefName $ show $ fromJust tp) (Just [(Nothing, b)]) []
		lcall = case aa of
				Dot _ c@(Call {}) -> Just c
				Arrow _ c@(Call {}) -> Just c
				NullDot _ c@(Call {}) -> Just c
				c@Call {} -> Just c
				_ -> Nothing
		isSelfSet = case aa of 
			Dot Self{} _ -> True
			_ -> False
		bToProcCall = if isJust tp then D.MathOp (fromJust tp) a b else b
		callSet ldef ref = exprCall env (Just $ unoptionIfChecked $ exprDataType ref) $ D.Call "set" (Just [(Just $ defName ldef, bToProcCall)]) []
	in if isNothing lcall then Set tp (ExpLError "Left is not def" aa) $ expr env b
		else case fromJust lcall of
			Call ldef _ [] _ -> 
				if DefModMutable `elem` defMods ldef then simpleSet 
				else if envInit env && isSelfSet then simpleSet
				else case aa of
					Dot ref _ ->  Dot ref $ callSet ldef ref
					NullDot ref _ ->  NullDot ref $ callSet ldef ref
					_ -> exprCall env Nothing $ D.Call "set" (Just [(Just $ defName ldef, bToProcCall)]) []
			_ -> Set tp (ExpLError "Unassinable left" aa) (expr env b)
expr env (D.PlusPlus e) = PlusPlus (exprToSome env e)
expr env (D.MinusMinus e) = MinusMinus (exprToSome env e)
expr env D.Self = Self $ envSelf env
expr env D.Super = Super $ fromMaybe (error $ "No super data type for " ++ show (envSelf env)) $ superType $ envSelf env
expr env r@D.Call{} = maybeInlineCall env $ exprCall env Nothing r
expr env (D.Index e i) = let
	e' = exprToSome env e
	obf = expr env $ D.Dot e (D.Call "apply" (Just [(Nothing, i)]) [])
	in case exprDataType e' of
		TPClass{} -> obf
		(TPGenericWrap _ TPClass{}) -> obf
		_ -> Index e' $ expr env i 
expr env l@(D.Lambda pars e) = 
	if all (isJust.snd) pars then 
		let 
			pars' = map (second (dataType env . fromJust)) pars
			env' =  envAddVals (map (uncurry localVal) pars') env
			mapEnvTp = case envTp env' of
				TPFun _ d -> d
				d -> d
			e' = expr env'{envTp = mapEnvTp} e
			tp = exprDataType e'
		in Lambda pars' (maybeAddReturn env tp e') tp
	else ExpDError "Not all types are defined in lambda" l

expr env (D.Val name tp body mods) = let
	tp' = fmap (dataType env) tp
	body' = expr env{envTp = fromMaybe (baseDataType env) tp'} body
	tp'' = unwrapGeneric $ fromMaybe (exprDataType body') tp'
	body'' = if isJust tp then implicitConvertsion env tp'' body' else body'
	mods' = DefModLocal : [DefModMutable | D.DefModMutable `elem` mods] ++ [DefModWeak | D.DefModWeak `elem` mods] 
	def' = Def{defName = name, defType = tp'', defMods = mods', defPars = [], 
		defBody = if isTpOption tp'' then body'' else case body'' of
			Nop -> ExpError $ name ++ ": no initialiazation value for non-option"
			_ -> body'', 
		defGenerics = Nothing}
	in declareVal env def'
expr _ (D.Arr []) = Arr []
expr env (D.Arr [e]) = Arr [expr env e]
expr env (D.Arr exprs) = 
	let 
		exprs' = map (expr env) exprs
		tps = map exprDataType exprs'
		commonTp = reduceDataTypes env tps 
	in Arr $ map (\(e, tp) -> if tp == commonTp then e else maybeCast commonTp e) (zip exprs' tps)
expr env (D.Tuple items) = Tuple $ map (expr env) items
expr env (D.Throw e) = Throw (expr env e)
expr env (D.Return e) = Return  True (expr env e)
expr env (D.Not e) = Not (expr env e)
expr env (D.Negative e) = Negative (expr env e)
expr _ D.Break = Break
expr env c@D.Case{} = linkCase env c
expr env s@D.StringBuild {} = linkStringBuild env s
expr env ex@D.FuncOp{} = linkFuncOp env ex
-- expr x = error $ "No expr for " ++ show x

declareVal :: Env -> Def -> Exp
declareVal env d 
	| isSimpleExpression (defBody d) = Val False d
	| otherwise = Val True mappedDef
	where
		mappedDef = d{defBody =  multilineSet env Nothing (callRef mappedDef) (defBody d)}
multilineSet :: Env -> Maybe MathTp -> Exp -> Exp -> Exp
multilineSet env tp l r = mapExp replaceReturn $ addReturn env True (exprDataType r) r
	where
		replaceReturn (Return _ e) = Just $ Set tp l e
		replaceReturn _ = Nothing


{------------------------------------------------------------------------------------------------------------------------------ 
 - Options
 ------------------------------------------------------------------------------------------------------------------------------}

nonOpt :: Env -> Bool -> Exp -> Exp
nonOpt _ False e = NonOpt False e e
nonOpt env True e = let
	tp = exprDataType e
	tp' = unoptionHard $ tp
	rt w = If (BoolOp Eq w (None tp')) (Throw $ StringConst "Not null") w
	val = tmpVal env "" tp e
	in NonOpt True e $ if isElementaryExpression e then rt e else Braces [Val False val, rt (callRef val)]

linkOptionCall :: Env -> (D.Exp, Exp) -> D.Exp -> Exp
linkOptionCall env (_, leftExp) (D.Call "get" Nothing []) = nonOpt env True $ leftExp
linkOptionCall env (_ ,leftExp) e@(D.Call "cast" Nothing [tp]) = case tp of
	D.DataTypeOption{} -> Cast (dataType env tp) leftExp 
	_ -> ExpDError ("Cast option to non-option: " ++ show tp) e
linkOptionCall env (l, _) (D.Call "getOr" (Just [(_, alt)]) []) = 
	let
		l' = expr env{envVarSuffix = envVarSuffix env ++ "_e1"} l
		tp = unoptionHard $ exprDataType l'
		alt'' = linkOptionAlt env alt tp
	in linkOrElse env (tp, False) (l, l') alt''
linkOptionCall env (l, _) (D.Call "or" (Just [(_, alt)]) []) = 
	let
		l' = expr env{envVarSuffix = envVarSuffix env ++ "_e1"} l
		tp = unoptionHard $ exprDataType l'
		alt'' = linkOptionAlt env alt (option False tp)
	in linkOrElse env (tp, True) (l, l') alt''
linkOptionCall env (_, l') (D.Call fname (Just [(_, e)]) []) 
	| fname == "map" || fname == "for" || fname == "flatMap" = let
		aTp = unoptionHard $ exprDataType l'
		tmp :: Def
		tmp = localValE mapVarName (option True aTp) l'
		mapVarName = case e of
			D.Lambda [(name, _)] _ -> name 
			_ -> "_"
		mapExpr = case e of
			D.Lambda _ le -> le
			_ -> e
		mapExpr' = expr (envAddVals [tmp] env) mapExpr
		bTp = case fname of
			"for" -> TPVoid
			"map" -> exprDataType mapExpr'
			"flatMap" -> unoptionHard $ exprDataType mapExpr'
	in	Braces [
			declareVal env tmp, 
			(case unwrapGeneric $ envTp env of
				TPVoid -> If (BoolOp NotEq (callRef tmp) (None aTp) ) 
					mapExpr' Nop
				_ -> If (BoolOp NotEq (callRef tmp) (None aTp) ) 
					(implicitConvertsion env (option False bTp) mapExpr')
					(None $ wrapGeneric $ bTp))
		]			
linkOptionCall _ _ e = ExpDError ("Unknown option operation: " ++ show e) e

linkOptionAlt :: Env -> D.Exp -> DataType -> Exp
linkOptionAlt env alt tp = implicitConvertsion env tp $ expr env{envVarSuffix = envVarSuffix env ++ "_e2"} alt'
	where alt' = case alt of
			(D.Lambda [] a) -> a
			_ -> alt

linkOrElse :: Env -> (DataType, Bool) -> (D.Exp, Exp) -> Exp -> Exp
linkOrElse env _ ((D.NullDot dl dr), (NullDot dl' dr')) alt =
	let
		l'' = Dot (nonOpt env False dl') dr'
		dltp = exprDataType dl'
		tmp = tmpVal env "" dltp dl'
	in if isElementaryExpression dl' then If (BoolOp NotEq dl' (None dltp) ) (expr env (D.Dot (D.Dot dl (D.Call "get" Nothing [])) dr)) alt
		else Braces[
			declareVal env tmp,
			If (BoolOp NotEq (callRef tmp) (None dltp)) l'' alt
		]
linkOrElse env (tp, isOptionAlt) (_, l') alt = 
	let 
		tmp = tmpVal env "" (option False tp) $ implicitConvertsion env (option False tp) l'
	in Braces[
			declareVal env tmp,
			If (BoolOp NotEq (callRef tmp) (None tp)) ((if isOptionAlt then id else nonOpt env False) $ callRef tmp) alt
		]

linkNullDot :: Env -> D.Exp -> Exp
linkNullDot env d@(D.NullDot a b) = let
	aa = case a of
		D.Call {} -> exprCall env Nothing a
		_ -> exprToSome env a
	aTp = exprDataType aa
	bb = case aTp of
		TPOption _ tp -> exprCall env (Just tp) b
		TPGenericWrap _ (TPOption _ tp) -> exprCall env (Just tp) b
		tp -> ExpDError ("Null safe operation for the non-nullable datatype " ++ show tp) b			
	in case aa of
		ExpDError s _ -> ExpDError s d
		_ -> case bb of
			Dot l r -> NullDot (NullDot aa l) r
			_ -> NullDot aa bb

optChecking :: Env -> Exp -> (Env, Env)
optChecking env e =  rec (env, env) e
	where
		mapEnvDef en d = case unwrapGeneric $ defType d of
			TPOption False tp -> envChangeDefTp en d (TPOption True tp)
			_ -> en
		mapEnv en (Call d _ [] _) = mapEnvDef en d
		mapEnv en (Dot (Self _) (Call d _ [] _)) = mapEnvDef en d
		mapEnv en _ = en
		rec :: (Env, Env) -> Exp -> (Env, Env)
		rec en (BoolOp And a b) = rec (rec en a) b
		rec (l, r) (BoolOp Eq ee (None _)) = (l, mapEnv r ee)
		rec (l, r) (BoolOp NotEq ee (None _)) = (mapEnv l ee, r)
		rec (l, r) (BoolOp Eq (None _) ee) = (l, mapEnv r ee)
		rec (l, r) (BoolOp NotEq (None _) ee) = (mapEnv l ee, r)
		rec en _ = en

compareWithNil :: Env -> BoolTp -> (Exp, DataType) -> Exp 
compareWithNil _ btp (e, etp) = case etp of
	TPOption _ tp -> BoolOp btp e (None tp)
	_ -> ExpLError "Non-option compares with nil" e

compareOptions :: Env -> BoolTp -> (Exp, DataType) -> (Exp, DataType) -> Exp
compareOptions env btp (l, ltp) (r, rtp) = let
	comp l' r' = case btp of
		Eq -> BoolOp Or 
			(BoolOp ExactEq l' r') 
			(BoolOp And 
				(BoolOp And (BoolOp NotEq l' (None $ unoptionHard ltp)) (BoolOp NotEq r' (None $ unoptionHard rtp))) 
				(BoolOp Eq l' r') )
		NotEq -> BoolOp And 
			(BoolOp ExactNotEq l' r') 
			(BoolOp Or 
				(BoolOp Or (BoolOp Eq l' (None $ unoptionHard ltp)) (BoolOp Eq r' (None $ unoptionHard rtp))) 
				(BoolOp NotEq l' r') )
	isSimpleL = isElementaryExpression l
	isSimpleR = isElementaryExpression r
	lval = tmpVal env "_l" ltp l
	rval = tmpVal env "_r" rtp r
	ml = Braces $
		[Val False lval | not isSimpleL]
		++ [Val False rval | not isSimpleR]
		++ [comp (if isSimpleL then l else callRef lval) (if isSimpleR then r else callRef rval)]
	in if isSimpleL && isSimpleR then comp l r else ml

compareOptionWithNonOption :: Env -> BoolTp -> (Exp, DataType) -> (Exp, DataType) -> Exp
compareOptionWithNonOption env btp (opt, optTp) (nonopt, _) = let 
	comp opt' nonopt' = case btp of
		Eq -> BoolOp And (BoolOp NotEq opt' (None $ unoptionHard optTp)) (BoolOp Eq opt' nonopt') 
		NotEq -> BoolOp Or (BoolOp Eq opt' (None $ unoptionHard optTp)) (BoolOp NotEq opt' nonopt') 
	isSimpleOpt = isElementaryExpression opt
	val = tmpVal env "" optTp opt
	ml = Braces [Val False val,
		comp (callRef val) nonopt]
	in if isSimpleOpt then comp opt nonopt else ml


{------------------------------------------------------------------------------------------------------------------------------ 
 - Pointer
 ------------------------------------------------------------------------------------------------------------------------------}

linkPointerCall :: Env -> (D.Exp, Exp) -> D.Exp -> Maybe Exp
linkPointerCall env (_, leftExp) (D.Call "cast" Nothing [tp]) = Just $ Cast (case dataType env tp of
	p@TPPointer{} -> p
	p -> TPUnknown $ "Cast Pointer to non-pointer: " ++ show p) leftExp
linkPointerCall _ (_, leftExp) (D.Call "get" Nothing _) = Just $ Deferencing leftExp
linkPointerCall env (_, leftExp) (D.Call "get" (Just [(_, rel)]) _) = Just $ Deferencing $ MathOp Plus leftExp (expr env rel)
linkPointerCall env (_, leftExp) (D.Call "set" (Just [(_, value)]) _) = Just $ Set Nothing (Deferencing $ leftExp) (expr env value)
linkPointerCall env (_, leftExp) (D.Call "set" (Just [(_, rel), (_, value)]) _) = 
	Just $ Set Nothing (Deferencing $ MathOp Plus leftExp (expr env rel)) (expr env value)
linkPointerCall env (_, leftExp) c@(D.Call "free" Nothing _) = Just $ Dot leftExp $ exprCall env (Just $ exprDataType leftExp) c
linkPointerCall env (_, leftExp) c@(D.Call "copy" (Just [_]) _) = Just $ Dot leftExp $ exprCall env (Just $ exprDataType leftExp) c
linkPointerCall _ _ _ = Nothing

linkPointerStatic :: Env -> D.Exp -> Maybe Exp
linkPointerStatic env (D.Call "null" Nothing [tp]) = Just $ Null $ dataType env tp
linkPointerStatic _ _ = Nothing

{------------------------------------------------------------------------------------------------------------------------------ 
 - Functional Compositions >> *|* **
 ------------------------------------------------------------------------------------------------------------------------------}
linkFuncOp :: Env -> D.Exp -> Exp
linkFuncOp env ex@(D.FuncOp tp l r)  = 
	let 
		l' = expr env l
		r' = expr env r
		ltp = exprDataType l'
		lInputType = case ltp of 
			TPFun ret _ -> Right ret
			_ -> Left $ "Left is not function but " ++ show ltp ++ " in " ++ show l'
		lOutputType = case ltp of 
			TPFun _ ret -> Right ret
			_ -> Left $ "Left is not function but " ++ show ltp ++ " in " ++ show l'
		rInputTypeShouldBe = case tp of
			D.FuncOpBind -> lOutputType >>= \t -> case t of
				TPOption _ o -> return $ unwrapGeneric o
				_ -> return $ t
			D.FuncOpClone -> lInputType
		r'' = case exprDataType r' of
			TPFun{} -> r'
			_ -> case rInputTypeShouldBe of
				Left _ -> r'
				Right ritp -> 
					let 
						rr = expr (envAddVals [localVal "_" ritp] env) r
						etp = exprDataType rr
					in Lambda [("_", ritp)] (maybeAddReturn env etp rr) etp 
		ldef = localVal "__l" (exprDataType l')
		rdef = localVal "__r" (exprDataType r'')
		rtp = exprDataType r''
		rInputType = case rtp of 
			TPFun ret _ -> Right ret
			_ -> Left $ "Right is not function but " ++ show rtp ++ " in " ++ show r''
		rOutputType = case rtp of 
			TPFun _ ret -> Right ret
			_ -> Left $ "Right is not function but " ++ show rtp ++ " in " ++ show r''
		f p = do
			lInputType
			return $ Dot (callRef ldef) $ call (applyLambdaDef ltp) [p]
		g p = do 
			rInputType
			return $ Dot (callRef rdef) $ call (applyLambdaDef rtp) [p]
		compile :: Either String Exp
		compile = do
			li <- lInputType 
			lo <- lOutputType 
			ri <- rInputType
			ro <- rOutputType
			let
				lambda o c = Lambda [("_", li)] (maybeAddReturn env o c) o
				bind :: Either String Exp
				bind = do
					ff <- f $ callRef $ localVal "_" li
					let 		
						dotCall = do
							c <- g ff
							return $ lambda ro c
						optClass = dataTypeClass env lo 
						mapDef = maybe (Left "map in option didn't find") Right $ find ( (== "map") . defName) $ classDefs optClass 
						forDef = maybe (Left "for in option didn't find") Right $ find ( (== "for") . defName) $ classDefs optClass 
						optCall = do
							m <- if ro == TPVoid then forDef else mapDef
							gg <- g $ callRef $ localVal "_" $ wrapGeneric ri
							let c = Dot ff $ call m [Lambda 
								[("_", wrapGeneric ri)] 
								(maybeAddReturn env (wrapGeneric ro) gg)
								(wrapGeneric ro)]
							return $ lambda (if ro == TPVoid then TPVoid else TPOption False $ wrapGeneric ro) c
					case (lo, ri) of
						(TPOption _ _, TPOption _ _) -> dotCall
						(TPOption _ _, _) -> optCall
						_ -> dotCall
				clone :: Either String Exp
				clone = do
					ff <- f $ callRef $ localVal "_" li
					gg <- g $ callRef $ localVal "_" li
					case (lo, ro) of
						(TPVoid, TPVoid) -> return $ lambda TPVoid $ Braces [ff, gg]
						(TPVoid, _) -> return $ lambda ro $ Braces [ff, maybeAddReturn env ro gg]
						(_, TPVoid) -> return $ lambda lo $ Braces [gg, maybeAddReturn env lo ff]
						_ -> return $ lambda (TPTuple [lo, ro]) $ Tuple [ff, gg]
					
			case tp of
				D.FuncOpBind -> bind
				D.FuncOpClone -> clone
	in case compile of
		Left err -> ExpDError err ex
		Right e -> 
			Braces [
				declareVal env ldef{defBody = implicitConvertsion env ltp l'},
				declareVal env rdef{defBody = implicitConvertsion env rtp r''},
				e
			]
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
		modPrev (D.Call "when" (Just [_]) _) "" = ""
		modPrev (D.Call "when" (Just [_]) _) s = (edrp . dropWhile ( /= '\n') . reverse) s
			where
				edrp "" = ""
				edrp ssss = reverse $ tail ssss
		modPrev _ s = s
		modNext :: D.Exp -> String -> String
		modNext _ s = s
		compile :: String -> D.Exp -> String -> Exp
		compile prev (D.Call "when" (Just [(_, e)]) _) _ = 
			If (expr env e) 
				(StringConst $ '\n':(reverse . takeWhile ( /= '\n') . reverse) prev) 
				(StringConst "")
		compile _ e _ = expr env e
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

linkCase :: Env -> D.Exp -> Exp
linkCase env (D.Case mainExpr items) = 
	let 
		mainExpr' = expr env mainExpr
		_case = (localVal "__case__" (exprDataType mainExpr')) {defBody = mainExpr'}
	 	_incomplete = (localVal "__incomplete__" TPBool) {defBody = BoolConst True}
	 	_ok = (localVal "__ok__" TPBool) {defBody = BoolConst True}
	 	notOk = Set Nothing (callRef _ok) (BoolConst False)
	 	isOk = callRef _ok
		_result = (localVal "__result__" TPVoid)
		caseEnvVal caseEnv = "__case" ++ show (caseEnvValNum caseEnv) ++ "__"
		linkCaseItem :: D.CaseItem -> (Exp, DataType)
		linkCaseItem (cond, e) = 
			let 
				(ex, caseEnv) = runState (linkCaseCond cond) $ CaseEvn env _case 1 []
				caseDefs = caseEnvDefs caseEnv
				vars = map (declareVal env) caseDefs
				env' = envAddVals caseDefs env
				itemExpr = expr env' e
				setResultTo to = Set Nothing (callRef _result) to
				setResult = case itemExpr of
					Braces exprs -> init exprs ++ [setResultTo $ last exprs]
					ee -> [setResultTo ee]

			in  (If (callRef _incomplete) (Braces $ 
				declareVal env _ok : vars ++ [
					ex, 
					If isOk (Braces $ setResult ++ [Set Nothing (callRef _incomplete) (BoolConst False)])
						Nop]) Nop, exprDataType itemExpr)
		linkCaseCond :: D.CaseCondition -> State CaseEnv Exp
		linkCaseCond (D.CaseUnapply _ "" pars) = do
			caseEnv <- get
			let
				val = caseEnvCurrentVal caseEnv
				valTp = defType val
				env' = caseEnvEnv caseEnv
				valCl = dataTypeClass env' valTp
				constr = classConstructor valCl

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
					return $ (declareVal env $ newVal{defBody = Dot (callRef val) (callRef d)}) : [cond']
			pars' <- mapM linkPar $ zip (maybe [] defPars constr) pars
			return $ Braces $ join pars'

		linkCaseCond (D.CaseUnapply _ ref pars) = do
			caseEnv <- get
			let 
				val = caseEnvCurrentVal caseEnv
				env' = caseEnvEnv caseEnv
				tp = dataType env' $ D.DataType ref []
				cl = dataTypeClass env' tp
				newValOpt = localVal  ("__caseOpt" ++ show (caseEnvValNum caseEnv) ++ "__") newTpOpt
				newTpOpt = maybe (TPUnknown "Not found unapply") defType unapply
				newVal = localVal  (caseEnvVal caseEnv) newTp
				newTp = case newTpOpt of
					TPOption _ t -> t
					TPUnknown _ -> tp
					t -> t

				allUnappies = filter ( ("unapply" == ). defName) $ filter ( (DefModStatic `elem`) .defMods) $ classDefs cl
				unapplyCall :: [Exp] -> Exp
				unapplyCall next = maybe (buildIf next) (buildCall next) unapply
				buildCall next f@Def{defType = ftp, defPars = [fpar]} = Braces $
					[declareVal env $ newValOpt{defBody = Dot (callRef (objectDef cl)) (Call f ftp [(fpar, maybeCast tp $ callRef val)] [])},
					If (callFromValOpt "isDefined") 
						(Braces $ (declareVal env $ newVal{defBody = callFromValOpt "get"}): next)
						notOk 
						]
				buildIf next = If (Dot (callRef val) (Is tp)) (Braces $
					(declareVal env $  newVal{defBody = Dot (callRef val) (CastDot tp)}) : next)
					notOk
				unapply :: Maybe Def
				unapply = find (parsTypeIs (defType val)) allUnappies
				parsTypeIs dtp def = case defPars def of
					[x] -> defType x == dtp
					_ -> False
				callFromValOpt fname = Dot (callRef newValOpt) (exprCall env' (Just newTpOpt) (D.Call fname Nothing []))
				
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
		linkCaseCond  (D.CaseType cond tp) = do
			caseEnv <- get
			let 
				oldVal = caseEnvCurrentVal caseEnv
				env' = caseEnvEnv caseEnv
				tp' = dataType env' tp 
				val = case cond of
					D.CaseVal name -> localVal name tp'
					_ -> localVal (caseEnvVal caseEnv) tp'
				caseEnv' = caseEnv {caseEnvCurrentVal = val, caseEnvValNum = caseEnvValNum caseEnv + 1}
			put caseEnv'
			cond' <- linkCaseCond cond
			modify (\e -> e{caseEnvCurrentVal = oldVal})
			let ok = case cond of
					D.CaseVal _ -> Set Nothing (callRef val) $ Dot (callRef oldVal) (CastDot tp')
					_ -> cond'
			return $ If (Dot (callRef oldVal) (Is tp')) ok notOk
		items' = map linkCaseItem items
		_result' = _result {defType = reduceDataTypes env $ map snd items'}
	in Braces $ [
		declareVal env _case, declareVal env _incomplete, declareVal env _result'] 
		++ map fst items' 
		++ [If (callRef _incomplete) (Throw $ StringConst "Case incomplete") Nop, 
			callRef _result']


{------------------------------------------------------------------------------------------------------------------------------ 
 - Calling 
 ------------------------------------------------------------------------------------------------------------------------------}


exprCall :: Env-> Maybe DataType -> D.Exp -> Exp
exprCall _ (Just (TPUnknown t)) e = ExpDError t e
exprCall env (Just _) (D.Call "as" Nothing [tp]) = As $ dataType env tp
exprCall env (Just _) (D.Call "is" Nothing [tp]) = Is $ dataType env tp
exprCall env (Just _) (D.Call "cast" Nothing [tp]) = CastDot $ dataType env tp
exprCall env strictClass cll@(D.Call name pars gens) = 
	case tryExprCall env strictClass cll of
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
tryExprCall env strictClass cll@(D.Call name pars gens) = maybeLambdaCall
	where
		pars' = map (second (\e -> FirstTry e (exprToSome env e))) (fromMaybe [] pars) 
		self = fromMaybe (envSelf env) strictClass
		call' :: (Maybe Class, Exp)
		call' = fromMaybe (Nothing, ExpDError errorString cll) $ listToMaybe $ mplus (findCall True) (findCall False)
		call'' :: Exp
		call'' = case call' of
			(cl, cc@Call{}) -> (resolveDef strictClass cl . correctCall) cc
			_ -> snd call'
			where
				resolveDef Nothing cl c@(Call d _ _ _)
					| DefModConstructor `elem` defMods d = c
					| DefModObject `elem` defMods d = c
					| DefModLocal `elem` defMods d = c
					| isJust cl = Dot (callRef $ objectDef $ fromJust cl) c
					| otherwise = Dot (Self (envSelf env)) c
				resolveDef _ _ c = c


		{-checkedCall = case call'' of
			(Call d tp cpars) ->
				Call d tp (map (checkPar d) cpars)
			_ -> call''
			where 
				checkPar _ par@(Def{defType = dtp, defName = dname}, e) = 
					let tp = (exprDataType e)
					in if isInstanceOfCheck env tp dtp then par else (fst par, 
						ExpLError (show tp ++ " is not instance of " ++ show dtp ++ " in parameter \"" ++ dname ++ "\"") call'')
-}
		maybeLambdaCall = case pars of
			Just [] -> case unwrapGeneric $ exprDataType call'' of
				(TPFun TPVoid _) -> LambdaCall call''
				TPOption True (TPGenericWrap _ (TPFun TPVoid _))-> LambdaCall call''
				TPOption True (TPFun TPVoid _) -> LambdaCall call''
				_ -> call''
			_ -> call''

		

		pars'' :: [(Def, Exp)]
		pars'' = case snd call' of
			Call _ _ r _ -> r
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
			determineGenericType _ g (Just tp) = (className g, (wrapGeneric . dataType env) tp)
			determineGenericType selfType g  _ = (className g, 
					fromMaybe (TPUnknown errorText) $ 
					mplus determineBySelfType determineByPars
				)
				where 
					determineByPars :: Maybe DataType
					determineByPars = (listToMaybe . mapMaybe ( (defType *** (exprDataType >>> unwrapGeneric))>>> tryDetermine g) ) rpars
					determineBySelfType :: Maybe DataType
					determineBySelfType = tryDetermine g (selfType, self)
					errorText = "Could not determine generic type for " ++ show g ++ " in " ++ show cll 
					tryDetermine :: Class -> (DataType, DataType) -> Maybe DataType
					tryDetermine c (TPGenericWrap _  a, TPGenericWrap _ b) = tryDetermine c (a, b)
					tryDetermine c (TPGenericWrap _ a, b) = tryDetermine c (a, b)
					tryDetermine c (a, TPGenericWrap _ b) = tryDetermine c (a, b)
					tryDetermine c (TPClass TPMGeneric _ gg, tp) = if c == gg then Just (wrapGeneric tp) else Nothing
					tryDetermine c (TPArr _ a, TPArr _ a') = tryDetermine c (a, a')
					tryDetermine c (TPOption _ a, TPOption _ a') = tryDetermine c (a, a')
					tryDetermine c (TPOption _ a, a') = tryDetermine c (a, a')
					tryDetermine c (TPMap a b, TPMap a' b') = mplus (tryDetermine c (a, a')) (tryDetermine c (b, b'))
					tryDetermine c (TPTuple a, TPTuple a') = listToMaybe $ mapMaybe (tryDetermine c) (zip a a')
					tryDetermine c (TPClass _ gg cl, TPClass _ gg' cl') = 
						listToMaybe $ mapMaybe (tryDetermine c) (zip gg gg'')
						where
							gg'' = fromMaybe [] $ upGenericsToClass cl (cl', gg')
					tryDetermine c (cl@(TPClass _ _ _), TPObject m cl') = 
						tryDetermine c (cl, TPClass TPMClass [TPClass m [] cl'] (classFind (envIndex env) "Class"))
					tryDetermine c (TPFun a b, TPFun a' b') = listToMaybe $ catMaybes [tryDetermine c (a, a'), tryDetermine c (b, b')]
					tryDetermine c (a, b@TPArr{}) = tryDetermine c (a, dtpw b)
					tryDetermine c (a, b@TPMap{}) = tryDetermine c (a, dtpw b)
					tryDetermine _ _ = Nothing
					dtpw tp = TPClass TPMGeneric (dataTypeGenerics env tp) (dataTypeClass env tp) 
			in case snd call' of
				(Call Def{defGenerics = Just defGens} _ _ _) -> 
					M.fromList $ ddefGenerics defGens ++ dclassGenerics
				_ -> M.fromList dclassGenerics 
								
		errorString :: String
		errorString = "Could find reference for call " ++ callStr ++ "\n" ++
			maybe "" (\cl -> "strict in class " ++ show cl ++ "\n") strictClass ++
			(if detailedReferenceError then "in defs:\n" ++
			(strs "\n" . map (ind . showDef False. snd)) (allDefs)  else "")
			where callStr = name ++ maybe "" (\ps -> "(" ++ strs ", " (map ((++ ":") . fromMaybe "" . fst) ps) ++ ")" ) pars

		correctCall :: Exp -> Exp
		correctCall (Call d tp _ _) = 
			Call d (replaceGenerics True gens'' tp) (map doImplicitConversation pars''') 
				(fromMaybe [] $ fmap cordg $ defGenerics d)
			where
				cordg = map (replaceGenerics True gens'' . TPClass TPMGeneric []) . defGenericsClasses
				doImplicitConversation (dd, e) = (dd, implicitConvertsion env (replaceGenerics True gens'' $ defType dd) e)

		allDefs :: [(Maybe Class, Def)]
		allDefs = maybe allDefsInEnv allDefsInStrictClass strictClass
			where 
			non f = map (\d -> (Nothing, d)) f
			clRef = dataTypeClassRef env (fromJust strictClass)
			allDefsInStrictClass _ = 
				non (allDefsInClass clRef)
				++ non (allDefsInObject clRef)
			allDefsInEnv = non(envVals env)
				++ non (allDefsInClass (dataTypeClassRef env $ envSelf env) )
				++ non (allDefsInObject (dataTypeClassRef env $ envSelf env) )
				++ concatMap (\cl -> map (\d -> (Just cl, d)) $ classStaticDefs cl) (envObjectIndex env)
				++ non objects 
			objects = if isNothing pars then (map (objectDef . snd) . M.toList) (envIndex env) else []
		
		findCall :: Bool -> [(Maybe Class, Exp)]
		findCall hard = (mapMaybe fit . filter ((== name) . defName . snd)) allDefs
			where
				fit :: (Maybe Class, Def) -> Maybe (Maybe Class, Exp)
				fit (cl, d)
					| length pars' == length (defPars d) = 
							if checkParameters pars' (defPars d) then Just $ (cl, def' d) else Nothing
					| otherwise = Nothing

				def' d = Call d (resolveTp d) (zipWith (\dp (_, e) -> (dp, e) ) (defPars' d) pars') []
				resolveTp d = case defType d of
					TPSelf _ -> self
					tp@(TPFun _ dtp)-> if length pars' == length (defPars d) then tp else dtp
					tp -> tp
				defPars' :: Def -> [Def]
				defPars' Def{defType = t, defPars = []} = dataTypePars t
				defPars' Def{defPars = r} = r
				checkParameters :: [(Maybe String, Exp)] -> [Def] -> Bool
				checkParameters cp dp = all (checkParameter) $ zip cp dp
				checkParameter :: ((Maybe String, Exp), Def) -> Bool
				checkParameter ((Just cn, e), Def{defName = dn, defType = tp}) = cn == dn && checkDataType tp (exprDataType e)
				checkParameter ((_, e), Def{defType = tp}) = checkDataType tp (exprDataType e)
				checkDataType dtp tp = if hard then isInstanceOfTp env tp dtp else True




correctCallPar :: Env -> Generics -> (Def, Exp) -> (Def, Exp)
correctCallPar env gens (d@Def{defType = (TPFun _ _)}, FirstTry _ e'@Lambda{}) = correctCallPar env gens (d, e')
correctCallPar env gens (d@Def{defType = (TPFun _ _)}, FirstTry D.Lambda{} e') = correctCallPar env gens (d, e')
correctCallPar env gens (d@Def{defType = tp@(TPFun _ _)}, FirstTry e e')
	| isTpFun (exprDataType e') = checkCallParOnWeak (d, e')
	| otherwise = correctCallPar env gens (d, ExpDError "" $ 
		D.Lambda (map (\(n, _) -> (n, Nothing)) $ lambdaImplicitParameters tp) e)
correctCallPar env gens (d, FirstTry _ e) = correctCallPar env gens (d, e)
correctCallPar _ _ (d@Def{defType = (TPFun _ (TPClass TPMGeneric _ _) )}, Lambda lpars e dtp) = checkCallParOnWeak (d, Lambda lpars e dtp)
correctCallPar env gens(d@Def{defType = (TPFun stp dtp)}, ExpDError _ (D.Lambda lambdaPars lambdaExpr)) = checkCallParOnWeak (d, Lambda lpars' expr' tp')
	where
		lpars' :: [(String, DataType)]
		lpars' = map (second (replaceGenerics True gens)) $ zip (map fst lambdaPars) (stps stp)
		stps :: DataType -> [DataType]
		stps (TPTuple tps) = tps
		stps tp = [tp]
		env' = envAddVals (map (uncurry localVal) lpars') $ env{envTp = dtp'}
		dtp' = replaceGenerics True gens dtp
		expr' = if dtp' == TPVoid then expr env' lambdaExpr else maybeAddReturn env dtp' $ expr env' lambdaExpr
		tp' = if containsGeneric dtp then wrapGeneric (exprDataType expr') else dtp
		containsGeneric = fromMaybe False . forDataType (\t -> case t of
			TPClass TPMGeneric _ _ -> Just True
			_ -> Nothing)
correctCallPar _ _ e = checkCallParOnWeak e

checkCallParOnWeak :: (Def, Exp) -> (Def, Exp)
checkCallParOnWeak (d@Def{defMods = mods}, e) = 
	if DefModWeak `elem` mods then (d, insertWeak e)
	else (d, e)

insertWeak :: Exp -> Exp
insertWeak e = mapExp f e
	where
		f (Lambda p1 ee p2) = Just $ Lambda p1 (Weak ee) p2
		f _ = Nothing


{-----------------------------------------------------------------------------------------------------------------------------------------
 - Inline
 -----------------------------------------------------------------------------------------------------------------------------------------}

maybeInlineCall :: Env -> Exp -> Exp
maybeInlineCall env e = let
	callExpOpt = case e of
		Dot _ c@Call{} -> Just c
		Call{} -> Just e
		_ -> Nothing
	callExp = fromJust callExpOpt
	pars = case callExp of
		Call _ _ p _ -> if DefModStatic `notElem` defMods def then selfPar : p else p
	defOpt = case callExpOpt of 
		Just (Call dd _ _ _) -> Just dd
		Nothing -> Nothing
	def = fromJust defOpt
	selfExp = case e of Dot l _ -> l
	selfTp = exprDataType selfExp
	selfClass = dataTypeClass env selfTp
	selfPar = (localVal "self" selfTp, selfExp)
	
	inline = 
		if null parsForDeclareVars then replacedExp
		else Braces $ (map (Val False . snd) vals) ++ [replacedExp]
		where
			declaredVals :: [Def]
			declaredVals = forExp findVal $ defBody def
				where 
					findVal (Val _ d) = [d]
					findVal _ = []

			defClass :: Class
			defClass = fromJust $ defClassRec selfClass
				where defClassRec cl
					| def `elem` classDefs cl = Just cl
					| otherwise = listToMaybe $ mapMaybe defClassRec $ map fst $ extendsRefs $ classExtends cl
			gens :: Generics
			gens = buildGenerics defClass $ fromJust $ upGenericsToClass defClass ((dataTypeClass env selfTp), (dataTypeGenerics env selfTp))
				
			mapDeclaredValsGenerics = map repGens declaredVals
				where repGens d = (d, d{defType = replaceGenerics False gens $ unblockGenerics $ defType d, 
					defName = "__inline_" ++ envVarSuffix env ++ "_" ++ defName d})

			replacedExp = mapExp rep $ defBody def
			rep :: Exp -> Maybe Exp
			rep (Dot (Call d _ [] _) (Call Def{defMods = mbLamdaMods} _ lambdaCallPars _)) 
				| DefModApplyLambda `elem` mbLamdaMods = fmap (unwrapLambda (map (mapExp rep . snd) lambdaCallPars)) $ lookup d refs
			rep (LambdaCall (Call d _ [] _)) = fmap (unwrapLambda []) $ lookup d refs
			rep (Call d _ [] _) = if DefModField `elem` defMods d then Nothing else lookup d refs
			rep (Val b dd) = 
				fmap (\d -> Val b $ d{defBody = mapExp rep (defBody d)} ) $ lookup dd mapDeclaredValsGenerics
			rep (Self _) = lookup (fst selfPar) refs
			rep _ = Nothing
			refs :: [(Def, Exp)]
			refs = (map (second callRef) vals) ++ pars ++ map (second callRef) mapDeclaredValsGenerics
			vals = (map dec parsForDeclareVars)
			dec (d, pe) = (d, tmpVal env (defName d) (defType d) pe)
			unwrapLambda :: [Exp] -> Exp -> Exp
			unwrapLambda [] (Lambda _ ee _) = ee
			unwrapLambda parExps (Lambda lpars ee _) = let
				nonelemPars = (map (\((nm, tp), lpe) -> Val False $ localValE nm tp lpe) $ filter (not . isElementaryExpression . snd ) (zip lpars parExps))
				elemPars = map (first fst) $ filter (isElementaryExpression . snd ) (zip lpars parExps)
				ee' = case elemPars of
					[] -> ee
					_ -> mapExp replaceOnEP ee
				replaceOnEP (Call d _ [] _) = lookup (defName d) elemPars
				replaceOnEP _ = Nothing
				in case nonelemPars of
					[] -> ee'
					_ -> Braces $ nonelemPars ++ [ee']
			unwrapLambda _ ee = LambdaCall ee

	parsForDeclareVars = filter (checkCountOfUsing . fst) unelementaryPars
		where 
			unelementaryPars = filter (not . isElementaryExpression . snd) pars
			checkCountOfUsing d = (length $ filter (d ==) usingDefs) > 1
			usingDefs = forExp findUsage (defBody def)
			findUsage (Call d _ [] _) = [d]
			findUsage (Self _) = [fst selfPar]
			findUsage _ = []
	in if isJust defOpt && DefModInline `elem` defMods (fromJust defOpt) then inline else e
{-----------------------------------------------------------------------------------------------------------------------------------------
 - Implicit conversion
 -----------------------------------------------------------------------------------------------------------------------------------------}

implicitConvertsion :: Env -> DataType -> Exp -> Exp
implicitConvertsion _ _ Nop = Nop
implicitConvertsion _ TPVoid expression = expression
implicitConvertsion env destinationType expression = if isInstanceOfCheck env (exprDataType theResult) destinationType then theResult 
		else ExpLError ("Could not convert " ++ show (exprDataType theResult) ++ " to " ++ show destinationType) theResult
	where
		theResult = implicitConvertsion' destinationType expression
		implicitConvertsion' (TPMap _ _) (Arr []) = Map []
		implicitConvertsion' (TPMap k v) (Arr exps) = Map $ map tup exps
			where
				tup (Tuple [ke, ve]) = (implicitConvertsion env k ke, implicitConvertsion env v ve)
				tup (Cast _ t) = tup t
				tup e = (ExpLError "Not tuple in map" e, ExpLError "Not tuple in map" e)
		implicitConvertsion' _ Nop = Nop
		implicitConvertsion' dtp ex = let stp = exprDataType ex
			in 
				case ex of
					Braces _ -> maybeAddReturn env dtp ex
					If cond l r -> If cond (implicitConvertsion env dtp l) (implicitConvertsion env dtp r)
					_ -> if stp == dtp then ex else conv stp dtp
			where
				conv (TPGenericWrap _ s) d = conv s d
				conv s (TPGenericWrap _ d) = conv s d
				conv (TPFun _ (TPGenericWrap _ _)) (TPFun _ (TPGenericWrap _ _)) = ex
				conv (TPFun _ _) (TPFun _ fdtp@(TPGenericWrap _ _)) = case ex of
					Lambda lambdaPars le _ -> Lambda lambdaPars  (maybeAddReturn env fdtp le) fdtp
					_ -> ex
				conv (TPFun _ stp) (TPFun _ TPVoid) = if stp == TPVoid then ex else  case ex of
					Lambda lambdaPars le _ -> Lambda lambdaPars  (maybeAddReturn env TPVoid le) TPVoid
					_ -> ex
				conv TPFun{} TPFun{} = ex
				conv _ f@(TPFun _ fdtp) = Lambda (lambdaImplicitParameters f) (maybeAddReturn env fdtp ex) fdtp
				{-conv TPFun{} _ = LambdaCall ex-}
				conv (TPOption True a) bb@(TPOption False b) = Cast bb $ conv a b
				conv (TPOption _ a) (TPOption _ b) = conv a b
				conv TPNil (TPOption _ tp) = None (wrapGeneric tp)
				conv a (TPOption ch b) 
					| a == b = Some ch ex
					| otherwise = Some ch $ conv a b
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
					(Cast tp (Tuple exps)) -> maybeCast tp $ Tuple $ zipWith (implicitConvertsion env) dtps exps
					_ -> ex
				conv (TPArr _ _) (TPArr _ adtp) = case ex of
					Arr exps -> Arr $ map (implicitConvertsion env adtp) exps
					_ -> ex
				conv (TPArr _ _) (TPClass _ [d] Class{className = "PArray"}) = Cast (TPEArr 0 (unwrapGeneric d)) ex
				conv sc dc@TPClass{} = if isInstanceOfTp env sc dc then ex else classConversion dc sc ex
				conv _ _ = ex

				classConversion c (TPGenericWrap _ g) e = classConversion c g e
				classConversion (TPClass _ gens cls) sc e =
					maybe e wrapWithApply $
						find(checkApplyPars . defPars ).
						map (replaceGenericsInDef gens') .
						filter(\d -> 
							(defName d == "apply") 
							&& (DefModStatic `elem` defMods d)) $ (classDefs cls)
					where
						gens' = buildGenerics cls gens
						checkApplyPars [Def{defType = tp}] = isInstanceOfTp env sc tp
						checkApplyPars _ = False
						od = objectDef cls
						wrapWithApply apply@Def{defPars = [par]} = Dot (Call od (defType od) [] []) (Call apply dtp [(par, e)] [])
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
checkErrorsInClass cl@Class{className = name, _classExtends = extends, _classGenerics = gens, _classDefs = defs, _classMods = mods} = 
	map (ErrorParent $ "class " ++ name) (
		checkErrorsInExtends extends
		++ concatMap checkErrorsInClass gens
		++ concatMap checkErrorsInDef defs
		++ checkUninitializedFields cl
		++ if ClassModAbstract `notElem` mods && ClassModStub `notElem` mods && ClassModTrait `notElem` mods then 
			checkAbstactFunctionsInClass cl else []
	)

checkUninitializedFields :: Class -> [Error]
checkUninitializedFields cl@Class{_classDefs = defs} = if ClassModStub `elem` classMods cl then [] else concatMap check fieldsToCheck
	where
		initFields = maybe [] (findInits . defBody) $ classInitDef cl
		findInits :: Exp -> [Def]
		findInits (Braces xs) = concatMap findInits xs
		findInits (Set Nothing (Dot (Self{}) (Call d _ [] _)) _) = [d]
		findInits (If _ l r) = intersect (findInits l) (findInits r)
		findInits _ = []
		fieldsToCheck = filter (\d -> DefModField `elem` defMods d && DefModSpecial `notElem` defMods d && DefModConstructorField `notElem` defMods d) defs
		check d@Def{defName = name, defBody = Nop, defType = tp}
			| not (isTpOption tp) && d `notElem` initFields = [Error $ name ++ ": is not initialized and not option"]
		check _ = []

checkAbstactFunctionsInClass :: Class -> [Error]
--checkAbstactFunctionsInClass cl | trace ("checkAbstactFunctionsInClass : " ++ className cl) False = undefined
checkAbstactFunctionsInClass cl = 
	let 
		allDefsIn cll = filter ((DefModStatic `notElem` ) . defMods) (classDefs cll) ++ concatMap (allDefsIn . fst)  (extendsRefs $ classExtends cll)
		allDefs = allDefsIn cl
		abstactDefs = filter ((DefModAbstract `elem` ) . defMods) allDefs
		implementedDefs = filter ((DefModAbstract `notElem` ) . defMods) allDefs
		hasImplementation d = any (eqDef d) implementedDefs
		eqDef :: Def -> Def -> Bool
		eqDef l r = defName l == defName r && length (defPars l) == length (defPars r) && all eqPar (zip (defPars l) (defPars r))
		makeError d = Error $ "Function " ++ show d ++ " is not implemented"
	in (map makeError . filter (not . hasImplementation)) abstactDefs

checkErrorsInExtends :: Extends -> [Error]
checkErrorsInExtends (Extends cls traits) =
	concatMap checkErrorsInExtendsRef traits
	++ maybe [] checkErrorsInExtendsClass cls

checkErrorsInExtendsClass :: ExtendsClass -> [Error]
checkErrorsInExtendsClass (ExtendsClass ref pars) = 
	checkErrorsInExtendsRef ref
	++ concatMap (checkErrorsInExp . snd) pars

checkErrorsInExtendsRef :: ExtendsRef -> [Error]
checkErrorsInExtendsRef (cl, tps) = concatMap checkErrorsInDataType tps ++ checkExtendsFinal cl

checkExtendsFinal :: Class -> [Error]
checkExtendsFinal Class{_classMods = mods}
	| ClassModFinal `elem` mods = [Error "Inherits the final class"]
checkExtendsFinal _ = []

checkErrorsInDef :: Def -> [Error]
checkErrorsInDef Def {defName = name, defPars = pars, defType = tp, defBody = body, defGenerics = gens, defMods = mods} =
	map (ErrorParent $"def " ++ name) (
		concatMap checkErrorsInDef pars
		++ checkErrorsInDataType tp
		++ checkErrorsInExp body
		++ maybe [] checkErrorsInDefGenerics gens
		++ concatMap checkErrorsInDefMod mods
	)

checkErrorsInDefMod :: DefMod -> [Error]
checkErrorsInDefMod (DefModError e) = [Error e]
checkErrorsInDefMod _ = []

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
		f e@(Call _ tp _ gens) = checkErrorsInDataType tp ++ checkCallAbstractConstructor e ++ concatMap checkErrorsInDataType gens
		f (Lambda pars _ ret) = checkErrorsInDataType ret ++ concatMap (checkErrorsInDataType . snd) pars
		f (Val _ Def{defType = tp}) = checkErrorsInDataType tp
		f (ExpDError t e) = [Error (show e ++ ": " ++ t)]
		f (ExpLError t e) = [Error (show e ++ ": " ++ t)]
		f (ExpError t) = [Error t]
		f e@FirstTry{} = [Error ("First try in out " ++ show e)]
		f (None tp) = checkErrorsInDataType tp
		f _ = []

checkCallAbstractConstructor :: Exp -> [Error]
checkCallAbstractConstructor (Call Def{defMods = dMods} (TPClass _ _ Class{_classMods = clMods}) _ _)
	| (DefModConstructor `elem` dMods) && (ClassModAbstract `elem` clMods) = [Error "Using abstract class"]
checkCallAbstractConstructor _ = []	