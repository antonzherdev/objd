module ObjD.Link.Check (Error(..), checkErrors
)where

import  Data.List
import  ObjD.Link.Struct

data Error = Error String | ErrorParent String Error
instance Show Error where
	show (Error s) = s
	show (ErrorParent s e) = s ++ ": " ++ show e

checkErrors :: Sources -> [Error]
checkErrors = concatMap checkErrorsInFile . sourcesAllFiles

checkErrorsInFile :: File -> [Error]
checkErrorsInFile File{fileName = name, fileClasses = classes} = 
	map (ErrorParent (name ++ ".od")) $ concatMap checkErrorsInClass classes

checkErrorsInClass :: Class -> [Error]
checkErrorsInClass e@ClassError{} = [Error (show e)]
checkErrorsInClass Generic{_classExtendsRef = extends} = concatMap checkErrorsInExtendsRef extends
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
	TPClass _ gens _ -> concatMap checkErrorsInDataType gens
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