module ObjD.Link.Struct (
	Lang(..), Sources, File(..), Class(..), Package(..), DataType(..), ExtendsRef, Extends(..), ClassMod(..), Generics, ClassRef, 
	DataTypeMod(..), WrapReason(..), Def(..), DefMod(..), Exp(..), ExtendsClass(..), Import(..), DefGenerics(..),
	Annotation(..), CallPar, Core(..),

	refDataTypeMod, superGenericsList, buildGenerics, extendsRefs, classExtends, refDataType, superType, option,
	wrapGeneric, mapDataTypeGenerics, classDefs, isTrait, classGenerics, upGenericsToClass, replaceGenerics,
	callRef, classMods, findValWithName, extractStringConst, classImports, classPackageName, extendsNothing, 
	callLocalVal, uint, uint4, uint8, buildGenericsForSelf, superGenerics, extendsClassRef, classConstructor,
	extendsClassClass, replaceGenericsInDef, objectType, unknownDef, unwrapGeneric, findDefWithName, objectDef,
	allDefsInParentClass, isStub, unblockGenerics, classInitDef, superClass, call, exprDataType, allDefsInObject,
	classFile, classPackage, localVal, localValE, byte, ubyte, int, int4, int8, float, float4, float8,
	forExp, mapExp, literalDefName, isElementaryExpression, isSimpleExpression, unoption, unoptionHard, unoptionIfChecked,
	isTpOption, showDef, allDefsInClass, classStaticDefs, dataTypePars, isTpFun, forDataType, fileNameWithPrefix, isRealClass,
	isStruct, isEnum, isCoreFile, classNameWithPrefix, isInline, isField, isStatic, isDef,
	dataTypeClassNameWithPrefix, dataTypeClassName, isConst, classFields, isAbstract, classContainsInit,
	isFinal, isTpClass, isTpEnum, isTpTrait, isNop, enumItems, isType, isGeneric, isGenericWrap, tpGeneric, resolveTypeAlias,
	containsAnnotationWithClassName, isSpecial, isConstructor, mainExtendsRef, isBaseClass, traitExtendsRefs, findAnnotationWithClassName, 
	eqPar, isClass, isCaseClass, isPure, isVoid, isTpStruct, isTpBaseClass, isError, isDefAbstract, isEnumItem, isTpGeneric, isPrivate,
	genName, dataTypeGenClassName, localVarE, localVar, coreDataTypeClass, coreClass, buildCore, classFullName
	) where

import 			 Ex.String
import           Data.Maybe
import           Data.List
import qualified Data.Map            as M
import 			 Control.Arrow
import           Data.Char
import           Control.Monad.State
import qualified ObjD.Struct         as D
import           Data.Decimal


data Lang = ObjC | Java deriving (Eq)


type Sources = [File]
data Core = Core {coreClasses :: M.Map String Class}
data Package = Package {packageName :: [String], packageObject :: Maybe Class, packagePrefix :: String}
instance Eq Package where
	a == b = packageName a == packageName b

data File = File {fileName :: String, filePackage :: Package, fileImports :: [Import], fileClasses :: [Class]}
data Import = ImportClass {importClass :: Class} | ImportObjectDefs {importClass :: Class} deriving (Eq)
instance Eq File where
	File {fileName = a, filePackage = p} == File {fileName = b, filePackage = bp} = a == b && p == bp

fileNameWithPrefix :: File -> String
fileNameWithPrefix f = packagePrefix (filePackage f) ++ fileName f

{-----------------------------------------------------------------------------------------------------------------------------------------
 - CLASS 
 -----------------------------------------------------------------------------------------------------------------------------------------}
data Class = Class {_classFile :: File, _classPackage :: Package, className :: String, genClassName :: String
	, _classGenerics :: [Class], _classExtends :: Extends, _classMods :: [ClassMod], _classDefs :: [Def]
	, _classImports :: [Import], classAnnotations :: [Annotation], classDefsWithTraits :: [Def]}
	| Generic {className :: String, genClassName :: String, _classExtendsRef :: [ExtendsRef]}
	| ClassError {className :: String, genClassName :: String, classErrorText :: String}
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
classNameWithPrefix cl 
	| genClassName cl == className cl = packagePrefix (classPackage cl) ++ cap (className cl)
	| otherwise = genClassName cl
fullClassName :: Class -> String
fullClassName cl = strs "." (packageName $ classPackage cl) ++ case className cl of
	"" -> ""
	nm -> "." ++ nm

classContainsInit :: Class -> Bool 
classContainsInit cl = isJust (classInitDef cl) || any classContainsInit (superClasses cl) 

classInitDef :: Class -> Maybe Def
classInitDef cl = find (\d -> "init" == defName d && null (defPars d)) $ classDefs cl

findDefWithName :: String -> Class -> Maybe Def
findDefWithName name cl = find ((name ==) . defName) $ classDefs cl

findValWithName :: String -> Class -> Maybe Def
findValWithName name cl = find (\d -> name == defName d && DefModField `elem` defMods d && null (defPars d)) $ classDefs cl


data ClassMod = ClassModStub | ClassModStruct | ClassModTrait | ClassModEnum | ClassModObject | ClassModType | 
	ClassModAbstract | ClassModFinal | ClassModCase | ClassModPackageObject | ClassModTraitImpl deriving (Eq)

type ExtendsRef = (Class, [DataType])
data Extends = Extends {extendsClass :: Maybe ExtendsClass, extendsTraits :: [ExtendsRef]}
data ExtendsClass = ExtendsClass ExtendsRef [CallPar]
type Generics = M.Map String DataType
type ClassRef = (Class, Generics)

instance Show Class where
	show (Generic name _ ext) = name ++ extString
		where
			extString = case filter ( (/= "Object") . className . fst) ext of
				[] -> ""
				genExtendsRefs -> " extends " ++ strs " with " (map showExtendsRef genExtendsRefs)
	show (ClassError name _ er) = name ++ ": " ++ er
	show cl =
		tp cl ++ " " ++ className cl ++ (if genClassName cl /= className cl then "\\" ++ genClassName cl ++ "\\" else "") 
			++ sConstr cl ++ " " ++ show (classExtends cl) ++ " {\n" ++
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
	show ClassModTraitImpl = "impl"
			
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
isCoreFile File{filePackage = Package (x : y : _) _ _} = x == "objd" && (y == "lang" || y == "collection")
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
isBaseClass Class{className = "PObject"} = True
isBaseClass _ = False

isRealClass :: Class -> Bool
isRealClass = (ClassModStub `notElem` ) . classMods
isEnum :: Class -> Bool
isEnum = (ClassModEnum `elem` ) . classMods
classFields :: Class -> [Def]
classFields = filter isField . classDefs

classPackageName :: Class -> [String]
classPackageName = packageName . classPackage
classFullName :: Class -> [String]
classFullName cl
	| ClassModPackageObject `elem` classMods cl = classPackageName cl
	| otherwise = classPackageName cl ++ [className cl] 


data Annotation = Annotation Def [CallPar]

annotationClass :: Annotation -> Class
annotationClass (Annotation Def{defType = TPClass _ _ cl} _) = cl

findAnnotationWithClassName :: String -> [Annotation] -> Maybe Annotation
findAnnotationWithClassName nm = find ((== nm) . fullClassName . annotationClass)

containsAnnotationWithClassName :: String -> [Annotation] -> Bool
containsAnnotationWithClassName nm a = isJust $ findAnnotationWithClassName nm a

instance Show Annotation where
	show (Annotation d pars) = "@" ++ defName d ++ showCallPars pars


genName :: [Annotation] -> Maybe String
genName anns = findAnnotationWithClassName "objd.lang.GenName" anns >>= (\a -> case a of
			Annotation _ [(_, StringConst s)] -> Just s
			_ -> Nothing)

{-----------------------------------------------------------------------------------------------------------------------------------------
 - Extends 
 -----------------------------------------------------------------------------------------------------------------------------------------}


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

extendsClassClass :: ExtendsClass -> Class
extendsClassClass (ExtendsClass (cl, _) _) = cl


superClasses :: Class -> [Class]
superClasses = map fst . extendsRefs . classExtends

{-----------------------------------------------------------------------------------------------------------------------------------------
 - Def 
 -----------------------------------------------------------------------------------------------------------------------------------------}

data Def = Def {defName :: String, defPars :: [Def], defType :: DataType, defBody :: Exp, defMods :: [DefMod]
	, defGenerics :: Maybe DefGenerics, defAnnotations :: [Annotation]}
unknownDef :: Def
unknownDef = Def "???" [] TPVoid Nop [] Nothing []
localVal :: String -> DataType -> Def
localVal name tp = Def name [] tp Nop [DefModLocal] Nothing []
localVar :: String -> DataType -> Def
localVar name tp = Def name [] tp Nop [DefModLocal, DefModMutable] Nothing []
localValE :: String -> DataType -> Exp -> Def
localValE name tp e = Def name [] tp e [DefModLocal] Nothing []
localVarE :: String -> DataType -> Exp -> Def
localVarE name tp e = Def name [] tp e [DefModLocal, DefModMutable] Nothing []
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
isPrivate :: Def -> Bool
isPrivate = (DefModPrivate `elem` ) . defMods
isDefAbstract :: Def -> Bool
isDefAbstract = (DefModAbstract `elem` ) . defMods
enumItems :: Class -> [Def]
enumItems = filter isEnumItem . classDefs
instance Eq Def where
	a == b = defName a == defName b && length (defPars a) == length (defPars b) && all eqPar (zip (defPars a)(defPars b)) && (isStatic a == isStatic b)

eqPar :: (Def, Def) -> Bool
eqPar (x, y) = defName x == defName y

data DefMod = DefModStatic | DefModMutable | DefModAbstract | DefModPrivate | DefModPublic | DefModProtected | DefModGlobalVal | DefModWeak
	| DefModConstructor | DefModStub | DefModLocal | DefModObject 
	| DefModField | DefModEnumItem | DefModDef | DefModSpecial | DefModStruct | DefModApplyLambda | DefModSuper | DefModInline 
	| DefModPure | DefModFinal | DefModOverride | DefModError String | DefModConstructorField | DefModVolatile
	| DefModChangedInLambda | DefModEnum
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
	show DefModEnum = "enum"
	show DefModEnumItem = "enum_item"
	show DefModDef = "def"
	show DefModSpecial = "special"
	show DefModStruct = "struct"
	show DefModInline = "inline"
	show DefModVolatile = "volatile"
	show DefModConstructorField = ""
	show DefModSuper = "super"
	show DefModApplyLambda = "applyLambda"
	show DefModPure = "pure"
	show DefModFinal = "final"
	show DefModOverride = "override"
	show DefModChangedInLambda = "block"
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
showDef f Def {defName = name , defPars = [], defType = tp, defBody = e, defMods = mods, defGenerics = gens, defAnnotations = ans} =
	pstrs' "" "\n" "\n" ans ++ strs' " " mods ++ " " ++ name ++ maybe "" show gens ++ " : " ++ show tp ++ if f then " = " ++ show e else ""
showDef f Def {defName = name , defPars = pars, defType = tp, defBody = e, defMods = mods, defGenerics = gens, defAnnotations = ans} =
	pstrs' "" "\n" "\n" ans ++ strs' " " mods ++ " " ++ name ++ maybe "" show gens ++ "(" ++ strs' ", " pars ++ ")" ++ " : " ++ show tp  ++ if f then  " = " ++ show e else ""

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
		ch DefModFinal = 'F'
		ch DefModOverride = 'o'
		ch DefModProtected = 'q'
		ch DefModWeak = 'w'
		ch DefModPure = 'u'
		ch DefModConstructor = 'c'
		ch DefModStub = 'b'
		ch DefModGlobalVal = 'g'
		ch DefModField = 'f'
		ch DefModLocal = 'l'
		ch DefModObject = 'o'
		ch DefModEnumItem = 'E'
		ch DefModEnum = 'e'
		ch DefModDef = 'd'
		ch DefModSpecial = 'i'
		ch DefModStruct = 's'
		ch DefModApplyLambda = 'd'
		ch DefModSuper = 'r'
		ch DefModInline = 'i'
		ch DefModVolatile = 'v'
		ch DefModConstructorField = 'U'
		ch (DefModError _) = '!'
		ch DefModChangedInLambda = 'k'
		

dataTypePars :: DataType -> [Def]
dataTypePars (TPFun pars _) = map (localVal "") pars
dataTypePars _ = []


literalDefName :: String -> String
literalDefName "" = ""
literalDefName nam = nmRec False nam
	where
		mbCap True s = cap s
		mbCap False s = s
		nmm = M.fromList [('+', "add"), ('-', "sub"), ('*', "mul"), ('/', "div")]
		nmRec ncap (x:xs) = maybe ( (if ncap then  toUpper x else x ): nmRec False xs) (\s -> mbCap ncap s ++ nmRec True xs) $ M.lookup x nmm
		nmRec _ _ = ""

{------------------------------------------------------------------------------------------------------------------------------ 
 - DataType 
 ------------------------------------------------------------------------------------------------------------------------------}
data DataType = TPNumber Bool Int | TPFloatNumber Int | TPString | TPVoid 
	| TPClass {tpMod :: DataTypeMod, tpGenerics :: [DataType], tpClass :: Class}
	| TPEArr Int DataType | TPAny | TPChar
	| TPArr Int DataType | TPBool | TPFun [DataType] DataType | TPTuple [DataType] | TPSelf Class | TPUnknown String 
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
isTpFun (TPGenericWrap _ TPFun{}) = True
isTpFun TPFun{} = True
isTpFun _ = False
isTpOption :: DataType -> Bool
isTpOption (TPGenericWrap _ TPOption{}) = True
isTpOption TPOption{} = True
isTpOption _ = False
isTpClass :: DataType -> Bool
isTpClass (TPClass TPMClass _ _) = True
isTpClass _ = False
isTpEnum :: DataType -> Bool
isTpEnum (TPGenericWrap _ (TPClass TPMEnum _ _)) = True
isTpEnum (TPClass TPMEnum _ _) = True
isTpEnum _ = False
isTpGeneric :: DataType -> Bool
isTpGeneric (TPClass TPMGeneric _ _) = True
isTpGeneric _ = False
isTpTrait :: DataType -> Bool
isTpTrait (TPClass TPMTrait _ _) = True
isTpTrait _ = False
isTpStruct :: DataType -> Bool
isTpStruct (TPClass TPMStruct _ _) = True
isTpStruct _ = False
isTpBaseClass :: DataType -> Bool
isTpBaseClass (TPClass TPMClass _ Class{className = "Object"}) = True
isTpBaseClass (TPClass TPMClass _ Class{className = "PObject"}) = True
isTpBaseClass _ = False

unoptionIfChecked :: DataType -> DataType
unoptionIfChecked (TPGenericWrap w (TPOption True d)) = TPGenericWrap w (unwrapGeneric d)
unoptionIfChecked (TPOption True d) = unwrapGeneric d
unoptionIfChecked d = d

unoptionHard :: DataType -> DataType
unoptionHard TPVoid = TPVoid
unoptionHard (TPOption _ o) = unwrapGeneric o
unoptionHard (TPGenericWrap w (TPOption _ o)) = TPGenericWrap w (unwrapGeneric o)
unoptionHard tp = TPUnknown $ show tp ++ " is not option"

unoption :: DataType -> DataType
unoption (TPOption _ o) = o
unoption (TPGenericWrap _ (TPOption _ o)) = o
unoption tp = tp

option :: Bool -> DataType -> DataType
option _ TPVoid = TPVoid
option ch o@(TPOption och tp)
	| ch == och = o
	| otherwise = TPOption (och && ch) tp
option ch (TPGenericWrap w tp) = TPGenericWrap w $ unwrapGeneric $ option ch tp
option ch tp@(TPClass TPMGeneric _ _) = wrapGeneric $ TPOption ch $ wrapGeneric tp
option ch tp = TPOption ch $ wrapGeneric tp

forDataType :: MonadPlus m => (DataType -> m a) -> DataType -> m a
forDataType f tp = mplus (go tp) (f tp)
	where
		go (TPClass _ gens _) = msum $ map (forDataType f) gens
		go (TPArr _ a) = forDataType f a
		go (TPEArr _ a) = forDataType f a
		go (TPFun a b) = mplus (msum $ map (forDataType f) a) (forDataType f b)
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
		go (TPFun a b) = TPFun (map (mapDataType f) a) (mapDataType f b)
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
	let tp' = f $ s ++ [d] 
	in TPFun (init tp') (last tp')
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

dataTypeClassName :: DataType -> String
dataTypeClassName (TPClass _ _ c ) = className c
dataTypeClassName (TPObject _ c) = className c
dataTypeClassName (TPGenericWrap _ c) = dataTypeClassName c
dataTypeClassName (TPArr _ _) = "ImArray"
dataTypeClassName (TPEArr _ _) = "PArray"
dataTypeClassName (TPMap _ _) = "ImHashMap"
dataTypeClassName TPAny = "Any"
dataTypeClassName (TPPointer _) = "Pointer"
dataTypeClassName (TPTuple [_, _]) = "Tuple"
dataTypeClassName (TPTuple a) = "Tuple" ++ show (length a)
dataTypeClassName (TPNumber False 1) = "Byte"
dataTypeClassName (TPNumber True 1) = "UByte"
dataTypeClassName (TPNumber False 0) = "Int"
dataTypeClassName (TPNumber True 0) = "UInt"
dataTypeClassName (TPNumber False 4) = "Int4"
dataTypeClassName (TPNumber True 4) = "UInt4"
dataTypeClassName (TPNumber False 8) = "Int8"
dataTypeClassName (TPNumber True 8) = "UInt8"
dataTypeClassName (TPFloatNumber 4) = "Float4"
dataTypeClassName (TPFloatNumber 8) = "Float8"
dataTypeClassName (TPFloatNumber 0) = "Float"
dataTypeClassName TPChar = "Char"
dataTypeClassName TPString = "String"
dataTypeClassName TPBool = "Bool"
dataTypeClassName TPFun{} = "F"
dataTypeClassName TPOption{} = "Option"
dataTypeClassName _ = ""

coreClass :: Core -> String -> Class
coreClass c nm = fromMaybe (ClassError nm nm ("Core class " ++ nm ++ " not found") ) $ M.lookup nm (coreClasses c)
coreDataTypeClass :: Core -> DataType -> Class
coreDataTypeClass _ (TPClass _ _ c ) = c
coreDataTypeClass _ (TPObject _ c) = c
coreDataTypeClass core (TPGenericWrap _ c) = coreDataTypeClass core c
coreDataTypeClass _ (TPSelf c) = c
coreDataTypeClass core tp = coreClass core (dataTypeClassName tp)
buildCore :: Sources -> Core
buildCore = Core . M.fromList . map (\cl -> (className cl, cl)) . concatMap fileClasses . filter isCoreFile


dataTypeGenClassName :: DataType -> String
dataTypeGenClassName (TPGenericWrap _ c) = dataTypeGenClassName c
dataTypeGenClassName (TPClass _ _ c ) = genClassName c
dataTypeGenClassName (TPObject _ c) = genClassName c
dataTypeGenClassName tp = dataTypeClassName tp


dataTypeClassNameWithPrefix :: DataType -> String
dataTypeClassNameWithPrefix (TPGenericWrap _ c) = dataTypeClassNameWithPrefix c
dataTypeClassNameWithPrefix (TPClass _ _ c ) = classNameWithPrefix c
dataTypeClassNameWithPrefix (TPObject _ c) = classNameWithPrefix c
dataTypeClassNameWithPrefix tp = "CN" ++ dataTypeClassName tp

resolveTypeAlias :: DataType -> DataType
resolveTypeAlias tp@(TPClass TPMType _ c) = fromMaybe (TPUnknown $ "No super type for type " ++ className c) $ superType tp
resolveTypeAlias tp = tp

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

tpGeneric :: DataType
tpGeneric = TPClass TPMGeneric [] (Generic "?" "?" [])

objectType :: DataType -> DataType
objectType (TPClass t _ cl) = TPObject t cl
objectType e = TPUnknown $ "No object type for type " ++ show e

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


{------------------------------------------------------------------------------------------------------------------------------ 
 - Generics 
 ------------------------------------------------------------------------------------------------------------------------------}

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

extendsClassRef :: ExtendsClass -> ExtendsRef
extendsClassRef (ExtendsClass ref _) = ref

replaceGenerics :: Bool -> Generics -> DataType -> DataType
replaceGenerics blk gns tp = mapDataType f tp
	where 
		f (TPClass TPMGeneric _ (Generic g _ _)) = fmap wrapBlock $ M.lookup g gns
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
				defMods = [DefModStatic, DefModObject], defGenerics = Nothing, defAnnotations = []}

replaceGenericsInDef :: Generics -> Def -> Def
replaceGenericsInDef gens d = d {defType = replaceGenerics False gens (defType d), defPars = map (replaceGenericsInDef gens) (defPars d) }


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
	| NullDot Exp Exp Exp -- The last is expanded expression
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
	| NPE
	| Not Exp
	| Negative Exp
	| Cast DataType Exp
	| As DataType
	| Is DataType
	| AsTp DataType Exp
	| IsTp DataType Exp
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
	show (NullDot l r _) = showOp' l "?." r
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
	show (AsTp tp e) = "as<" ++ show tp ++ ">(" ++ show e ++ ")"
	show (IsTp tp e) = "is<" ++ show tp ++ ">(" ++ show e ++ ")"
	show (CastDot tp) = "cast<" ++ show tp ++ ">"
	show (Break) = "break"
	show (Continue) = "continue"
	show (Null tp) = "null<" ++ show tp ++ ">"
	show (NonOpt _ e _) = show e ++ ".get"
	show (LambdaCall e) = show e ++ "()"
	show (Deferencing e) = "*(" ++ show e ++ ")"
	show (Try e f) = "try " ++ show e ++ "\nfinally " ++ show f
	show NPE = "NPE"
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
	where showPar (Def {defName = name, defType = tp}, e) = name ++ "\\" ++ show tp ++ "\\ = " ++ show e
	
callLocalVal :: String -> DataType -> Exp
callLocalVal name tp = Call (localVal name tp) tp [] []

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
		go (NullDot l r e) = mplus (mplus (forExp f l) (forExp f r)) (forExp f e)
		go (Index l r) = mplus (forExp f l) (forExp f r)
		go (PlusPlus e) = forExp f e
		go (MinusMinus e) = forExp f e
		go (Return _ e) = forExp f e
		go (Cast _ e) = forExp f e
		go (Some _ e) = forExp f e
		go (IsTp _ e) = forExp f e
		go (AsTp _ e) = forExp f e
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
		go (NullDot l r e) = NullDot (mapExp f l) (mapExp f r) (mapExp f e)
		go (Index l r) = Index (mapExp f l) (mapExp f r)
		go (PlusPlus e) = PlusPlus $ mapExp f e
		go (MinusMinus e) = MinusMinus $ mapExp f e
		go (Return p e) = Return p $ mapExp f e
		go (Cast t e) = Cast t $ mapExp f e
		go (Some ch e) = Some ch $ mapExp f e
		go (Throw e) = Throw $ mapExp f e
		go (Not e) = Not $ mapExp f e
		go (IsTp tp e) = IsTp tp $ mapExp f e
		go (AsTp tp e) = AsTp tp $ mapExp f e
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
isConst (NullDot l r _) = isConst l && isConst r
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
exprDataType (NullDot _ b _) = option False $ exprDataType b 
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
exprDataType (Lambda pars _ r) = TPFun (map snd pars) r
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
exprDataType NPE = TPThrow
exprDataType (Not _) = TPBool
exprDataType (Negative e) = exprDataType e
exprDataType (Deferencing e) = case exprDataType e of
	TPPointer r -> unwrapGeneric r
	TPGenericWrap _ (TPPointer r) -> unwrapGeneric r
	r -> TPUnknown $ show r ++ " is not a reference"
exprDataType (Cast dtp _) = dtp
exprDataType (As dtp) = option False $ wrapGeneric dtp
exprDataType (AsTp dtp _) = option False $ wrapGeneric dtp
exprDataType (CastDot dtp) = dtp
exprDataType (Is _) = TPBool
exprDataType (IsTp _ _) = TPBool
exprDataType Break = TPVoid
exprDataType (Null tp) = TPPointer tp
exprDataType StringBuild{} = TPString
exprDataType (LambdaCall e) = case unwrapGeneric $ exprDataType e of
	(TPOption True (TPGenericWrap _ (TPFun _ d))) -> d
	(TPOption True (TPFun _ d)) -> d
	(TPFun _ d) -> d
	t -> t
{- exprDataType x = error $ "No exprDataType for " ++ show x -}


