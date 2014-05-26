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
	_classExtends = Extends (if className c == "Object" then Nothing else Just $ baseClassExtends (envIndex env)) [], 
	_classDefs = allDefsInObject (c, M.empty), _classGenerics = [], _classImports = [],
	_classFile = fromMaybe (error $ "No class file for class " ++ className c) $ classFile c,
	_classPackage = classPackage c, classAnnotations = [], classDefsWithTraits = []}
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
dataTypeClass env f@TPFun{} = Class { _classMods = [], className = "", genClassName = "", _classExtends =  Extends (Just $ baseClassExtends (envIndex env)) [],
	_classPackage = Package ["objd", "lang"] Nothing "", _classFile = coreFakeFile, 
	_classDefs = [applyLambdaDef f], _classGenerics = [], _classImports = [], classAnnotations = [], classDefsWithTraits = []}
	where
		
dataTypeClass _ x = ClassError (show x) (show x) ("No dataTypeClass for " ++ show x)

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
dataType env (D.DataTypeOption t) = TPOption False $ (wrapGeneric . dataType env) t
	
getDataType :: Env -> Maybe D.DataType -> Exp -> DataType
getDataType env tp e = maybe (exprDataType e) (dataType env) tp

envSelfClass :: Env -> Class 
envSelfClass env = dataTypeClass env $ envSelf env

isValidDataType :: DataType -> Bool
isValidDataType (TPGenericWrap _ tp) = isValidDataType tp
isValidDataType (TPUnknown _) = False
isValidDataType tp = all isValidDataType $ dataTypeGenerics tp

