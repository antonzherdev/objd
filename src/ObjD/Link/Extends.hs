module ObjD.Link.Extends (
	superDataType, superDataTypes, isInstanceOf, isInstanceOfTp, isInstanceOfCheck, baseDataType,
	firstCommonSuperDataType, commonSuperDataType, reduceDataTypes
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import           Data.Maybe
import           Data.List


superDataType :: Env -> DataType -> ExtendsRef -> DataType
superDataType _ (TPClass _ gens cl) extRef@(extCl, _) = 
	TPClass (refDataTypeMod extCl) (superGenericsList (buildGenerics cl gens) extRef) extCl
superDataType _ (TPObject _ _) (extCl, _) = TPObject (refDataTypeMod extCl) extCl
superDataType env tp extRef@(extCl, _) = TPClass (refDataTypeMod extCl) 
	(superGenericsList (buildGenerics (dataTypeClass env tp) (dataTypeGenerics tp)) extRef)  extCl
-- superDataType _ _ = TPVoid

superDataTypes :: Env -> DataType -> [DataType]
superDataTypes env tp = map (superDataType env tp) $ extendsRefs $ classExtends $ dataTypeClass env tp

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
		all (\(clg, tg) -> isInstanceOfTp env clg tg ) $ zip (dataTypeGenerics cl) (dataTypeGenerics target)
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
		[mapDataTypeGenerics (map (\(ag, bg) -> wrapGeneric $ head $ commonSuperDataType env ag bg) . zip (dataTypeGenerics a) ) b]
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


