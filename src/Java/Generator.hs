module Java.Generator ( 
	toJava
)where

import Ex.String
import Data.Maybe
import ObjD.Link as D
import Java.Struct as J

toJava :: D.File -> [J.File]
toJava file@D.File{D.fileClasses = classes} =
	map (genFile file) $ filter (\cls -> not (D.isType cls) && not (D.isStub cls)) classes

genFile :: D.File -> D.Class -> J.File
genFile D.File{D.filePackage = D.Package{D.packageName = package}} cls =
	J.File {
		J.fileIsTest = D.containsAnnotationWithClassName "test.Test" $ D.classAnnotations cls,
		J.filePackage = package,
		J.fileImports = [],
		J.fileClass = (genClass cls) }

genClass :: D.Class -> J.Class
genClass cl = J.Class {
	J.classVisibility = J.Public,
	J.classType = if D.isTrait cl then J.ClassTypeInterface else J.ClassTypeClass,
	J.className = D.className cl,
	J.classGenerics = map genGeneric $ D.classGenerics cl,
	J.classExtends = D.mainExtendsRef (D.classExtends cl) >>= \e -> if D.isBaseClass (fst e) then Nothing else Just (genExtendsRef e),
	J.classImplements = map genExtendsRef $ filter (not . D.isBaseClass . fst) $ D.traitExtendsRefs $ D.classExtends cl,
	J.classDefs = map genDef defs
	}
	where 
		defs = if D.isTrait cl then filter (not . D.isConstructor) (D.classDefs cl) else D.classDefsWithTraits cl

genGeneric :: D.Class -> J.Generic
genGeneric cl = J.Generic {
	J.genericName = D.className cl,
	J.genericExtends = map genExtendsRef $ filter (not . D.isBaseClass . fst) $ D.extendsRefs $ D.classExtends cl
}

genExtendsRef :: D.ExtendsRef -> J.TP
genExtendsRef (cl, gens) = J.TPRef (map genTp gens) (D.className cl) 

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - DataType
 -----------------------------------------------------------------------------------------------------------------------------------------------}
genTp :: D.DataType -> J.TP
genTp (D.TPClass _ gens cl) = J.TPRef (map genTp gens) (D.className cl) 
genTp (D.TPNumber _ 1) = J.ref "byte"
genTp (D.TPNumber _ 8) = J.ref "long"
genTp (D.TPNumber _ _) = J.ref "int"
genTp (D.TPGenericWrap _ (D.TPNumber _ 1)) = J.ref "Byte"
genTp (D.TPGenericWrap _ (D.TPNumber _ 8)) = J.ref "Long"
genTp (D.TPGenericWrap _ (D.TPNumber _ _)) = J.ref "Integer"
genTp (D.TPFloatNumber 8) = J.ref "double"
genTp (D.TPFloatNumber _) = J.ref "float"
genTp (D.TPGenericWrap _ (D.TPFloatNumber 8)) = J.ref "Double"
genTp (D.TPGenericWrap _ (D.TPFloatNumber _)) = J.ref "Float"
genTp D.TPVoid = J.ref "void"
genTp (D.TPGenericWrap _ D.TPVoid) = J.ref "Void"
genTp D.TPChar = J.ref "char"
genTp (D.TPGenericWrap _ D.TPChar) = J.ref "Character"
genTp D.TPBool = J.ref "boolean"
genTp (D.TPGenericWrap _ D.TPBool) = J.ref "Boolean"

genTp (D.TPGenericWrap _ w) = genTp w
genTp D.TPString = J.ref "String"
genTp (D.TPEArr n tp)  = J.TPArr (genTp tp) n
genTp D.TPAny = J.ref "Object"
genTp (D.TPArr _ tp) = J.TPRef [genTp tp] "ImArray"
genTp (D.TPFun (D.TPTuple [stp]) dtp) = J.TPRef [genTp $ D.wrapGeneric stp, genTp $ D.wrapGeneric dtp] "F"
genTp (D.TPFun (D.TPTuple stps) dtp) = J.TPRef (map (genTp . D.wrapGeneric) stps ++ [genTp $ D.wrapGeneric dtp]) ("F" ++ show (length stps))
genTp (D.TPFun stp dtp) = J.TPRef [genTp $ D.wrapGeneric stp, genTp $ D.wrapGeneric dtp] "F"
genTp (D.TPTuple tps) = J.TPRef (map genTp tps) ("Tuple" ++ show (length tps))
genTp (D.TPSelf cl) = J.TPRef (map (J.ref . D.className) (D.classGenerics cl) ) (D.className cl)
genTp (D.TPMap key value) = J.TPRef [genTp key, genTp value] "HashMap"
genTp (D.TPOption _ tp) = genTp tp
genTp D.TPAnyGeneric = J.ref "Object"
genTp (D.TPPointer _) = J.ref "Pointer"

genTp tp = error $ "genTp: " ++ show tp

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Defs
 -----------------------------------------------------------------------------------------------------------------------------------------------}

genDef :: D.Def -> J.Def
genDef d =
 	let 
 		genMod D.DefModPrivate = Just $ J.DefModVisability J.Private
 		genMod D.DefModProtected = Just $ J.DefModVisability J.Protected
 		genMod D.DefModPublic = Just $ J.DefModVisability J.Public
 		genMod D.DefModStatic = Just J.DefModStatic
 		genMod D.DefModAbstract = Just J.DefModAbstract
 		genMod _ = Nothing
 		mods = mapMaybe genMod (D.defMods d)
 	in if D.isField d then 
 			J.Field {
 				J.defMods = mods,
 				J.defName = D.defName d,
 				J.defTp = genTp $ D.defType d,
 				J.defExp = genExp $ D.defBody d
	 		}
 		else if D.isConstructor d then
 			J.Constructor {
 				J.defMods = mods,
 				J.defPars = map genPar $ D.defPars d,
 				J.defStms = genStm $ D.defBody d 
 			}
 		else 
 			J.Def {
 				J.defMods = mods,
 				J.defName = D.defName d ++ concatMap (cap . D.defName) (D.defPars d),
 				J.defTp = genTp $ D.defType d,
 				J.defPars = map genPar $ D.defPars d,
 				J.defStms = genStm $ D.defBody d 
 			}

genPar :: D.Def -> J.DefPar
genPar D.Def{D.defName = nm, D.defType = tp} = (genTp tp, nm)

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Stm
 -----------------------------------------------------------------------------------------------------------------------------------------------}


genExp :: D.Exp -> J.Exp
genExp _ = J.Nop

genStm :: D.Exp -> [J.Stm]
genStm _ = [J.Stm J.Nop]

