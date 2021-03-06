module ObjD.Link.DataType (
	dataTypeClassRef, dataTypeClass, applyLambdaDef, dataTypeGenerics, getDataType, envSelfClass, dataType, isValidDataType
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env	
import qualified Data.Map            as M
import           Data.Maybe
import qualified ObjD.Struct         as D

dataTypeClassRef :: Env -> DataType -> ClassRef
dataTypeClassRef env tp = let 
		cl = dataTypeClass env tp
	in (cl, buildGenerics cl $ dataTypeGenerics tp)

dataTypeClass :: Env -> DataType -> Class
dataTypeClass _ (TPClass _ _ c ) = c
dataTypeClass env (TPObject _ c) = Class { _classMods = [ClassModObject], className = className c, genClassName = genClassName c, 
	_classExtends = Extends (if className c == "Object" then Nothing else Just $ baseClassExtends False (envIndex env)) [], 
	_classDefs = allDefsInObject (c, M.empty), _classGenerics = [], _classImports = [],
	_classFile = fromMaybe (error $ "No class file for class " ++ className c) $ classFile c,
	_classPackage = classPackage c, classAnnotations = [], classDefsWithTraits = []}
dataTypeClass env (TPGenericWrap _ c) = dataTypeClass env c
dataTypeClass _ (TPSelf c) = c
dataTypeClass env f@TPFun{} = Class { _classMods = [], className = "", genClassName = "", _classExtends =  Extends (Just $ baseClassExtends False (envIndex env)) [],
	_classPackage = Package ["objd", "lang"] Nothing "", _classFile = coreFakeFile, 
	_classDefs = [applyLambdaDef f], _classGenerics = [], _classImports = [], classAnnotations = [], classDefsWithTraits = []}
dataTypeClass env tp = classFind (envIndex env) (dataTypeClassName tp)

coreFakeFile :: File
coreFakeFile = File "fake.od" (Package ["objd", "lang"] Nothing "") [] []


applyLambdaDef :: DataType -> Def
applyLambdaDef (TPGenericWrap _ tp) = applyLambdaDef tp
applyLambdaDef (TPFun stp dtp) = Def {defName = "apply", defPars = map (localVal "") stp, defType = dtp, defBody = Nop, defMods = [DefModApplyLambda], defGenerics = Nothing, defAnnotations = []}
	
dataTypeGenerics :: DataType -> [DataType]
dataTypeGenerics (TPClass _ g _) = g
dataTypeGenerics (TPArr _ g) = [g]
dataTypeGenerics (TPEArr _ g) = [g]
dataTypeGenerics (TPFun s d) = s ++ [d]
dataTypeGenerics (TPMap k v) = [k, v]
dataTypeGenerics (TPOption _ v) = [v]
dataTypeGenerics (TPTuple a) = a
dataTypeGenerics (TPPointer a) = [a]
dataTypeGenerics (TPGenericWrap _ g) = dataTypeGenerics g
dataTypeGenerics _ = []

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
	"Bool" -> wrapGeneric TPBool
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
dataType env (D.DataTypeFun (D.DataTypeTuple tps) d) = TPFun (map (dataType env) tps) (dataType env d)
dataType env (D.DataTypeFun (D.DataType "void" _) d) = TPFun [] (dataType env d)
dataType env (D.DataTypeFun s d) = TPFun [dataType env s] (dataType env d)
dataType env (D.DataTypeTuple [tp]) = dataType env tp
dataType env (D.DataTypeTuple tps) = TPTuple $ map (wrapGeneric . dataType env) tps
dataType env (D.DataTypeOption t) = case unwrapGeneric $ dataType env t of
	tp@(TPClass TPMGeneric _ _) -> wrapGeneric $ TPOption False $ wrapGeneric tp
	tp -> TPOption False $ wrapGeneric tp 
	
getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType env) tp

envSelfClass :: Env -> Class 
envSelfClass env = dataTypeClass env $ envSelf env

isValidDataType :: DataType -> Bool
isValidDataType (TPGenericWrap _ tp) = isValidDataType tp
isValidDataType (TPUnknown _) = False
isValidDataType tp = all isValidDataType $ dataTypeGenerics tp

