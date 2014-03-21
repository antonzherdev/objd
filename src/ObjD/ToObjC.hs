module ObjD.ToObjC (
	fieldToProperty,
	stmToInterface,
	stmToImpl,
	toObjC
) where

import 		     Ex.String
import           Control.Arrow
import           Data.Char
import           Data.Maybe
import           Data.List
import qualified ObjC.Struct   as C
import qualified ObjD.Link   as D

arc :: Bool
arc = True

toObjC :: D.File -> ((String, [C.FileStm]), (String, [C.FileStm]))
toObjC f@D.File{D.fileClasses = classes} = 
	let 
		name = D.fileNameWithPrefix f
		isClass c = D.isRealClass c && not (D.isStruct c) && not (D.isTrait c) && not (D.isEnum c)
		cls = filter isClass classes
		isStruct c = D.isRealClass c && D.isStruct c
		structs = filter isStruct classes
		enums = filter D.isEnum classes
		isTrait c = D.isRealClass c && D.isTrait c

		dImports' = procImports f
		
		
		h = [C.Import $ if D.isCoreFile f then "objdcore.h" else "objd.h"] 
			++ fst dImports' 
			++ [C.EmptyLine] 
			++ map classDecl (cls ++ enums) 
			++ map (C.ProtocolDecl . D.classNameWithPrefix) (filter D.isTrait classes) 
			++ map structDecl structs
			++ [C.EmptyLine] 
			++ concatMap (fst . gen) classes
		
		structDecl c = C.TypeDefStruct (D.classNameWithPrefix c) (D.classNameWithPrefix c)
		classDecl c = C.ClassDecl $ D.classNameWithPrefix c

		m = let
				 impls = concatMap (snd . gen) classes
			in if null impls then []
			else [C.Import "objd.h" | D.isCoreFile f] ++
				[C.Import (name ++ ".h") ] ++ [ C.EmptyLine] ++ snd dImports' ++ impls
		gen c
			| isClass c = ([stmToInterface c], [stmToImpl c])
			| isStruct c = genStruct c
			| D.isEnum c = (genEnumInterface c, genEnumImpl c)
			| isTrait c = ([genProtocol c], [])
			| otherwise = ([], [])

	in ((name ++ ".h", h), (name ++ ".m", m))


{- Interface -}

funName :: D.Def -> String
funName D.Def{D.defName = "init", D.defPars = []} = "_init"
funName D.Def{D.defName = "isEqual"} = "_isEqual"
funName D.Def{D.defName = d} = d

stmToInterface :: D.Class -> C.FileStm
stmToInterface cl =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = classExtends cl,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs,
		C.interfaceFuns = constrFuns ++ [typeInstanceFun]
			++ intefaceFuns defs ++ staticGetters,
		C.interfaceFields = [(C.Private, map (implField env) implFields)]
	}
	where 
		env = Env cl 0 D.TPVoid False
		name = D.classNameWithPrefix cl
		defs = D.classDefs cl
		implFields = filter needField (D.classDefsWithTraits cl)
		needField f = D.isField f && not (D.isStatic f)
		constrFuns = fromMaybe [] $ fmap (\constr -> [createFun name constr, initFun constr]) (D.classConstructor cl)
		staticGetters = (map staticGetterFun .filter (\f -> 
			(D.DefModPrivate `notElem` D.defMods f) && D.isField f && D.isStatic f)) defs

typeInstanceFun :: C.Fun		
typeInstanceFun = C.Fun C.InstanceFun (C.TPSimple "ODClassType*" []) "type" []		

classExtends :: D.Class -> C.Extends
classExtends cl = addTraits cl $ maybe (C.Extends "NSObject" []) ext (D.extendsClass $ D.classExtends cl)
	where
		ext (D.ExtendsClass (ccl, _) _)
			| D.ClassModTrait `elem` D.classMods ccl = C.Extends "NSObject" [D.classNameWithPrefix ccl]
			| D.className ccl == "Object" = C.Extends "NSObject" []
			| otherwise = C.Extends (D.classNameWithPrefix ccl) []
addTraits :: D.Class -> C.Extends -> C.Extends		
addTraits cl (C.Extends cls protocols) = C.Extends cls $ protocols ++ map (D.classNameWithPrefix . fst) (D.extendsTraits $ D.classExtends cl)


staticGetterFun :: D.Def -> C.Fun
staticGetterFun D.Def{D.defName = name, D.defType = tp} = C.Fun C.ObjectFun (showDataType tp) name []

		
needProperty :: D.Def -> Bool
needProperty v = D.DefModPrivate `notElem` D.defMods v && D.isField v && not (D.isStatic v)


fieldToProperty :: D.Def -> C.Property
fieldToProperty D.Def{D.defName = name, D.defMods = mods, D.defType = tp} = C.Property {
	C.propertyName = name,
	C.propertyType = showDataType tp,
	C.propertyModifiers = if D.DefModMutable `elem` mods 
		then C.NonAtomic : mutModes tp ++ weak
		else [C.NonAtomic, C.ReadOnly] ++ weak
}
	where
		weak = [C.Weak | D.DefModWeak `elem` mods]
		mutModes (D.TPClass D.TPMClass _ _) = [C.Retain | D.DefModWeak `notElem` mods]
		mutModes (D.TPClass D.TPMEnum _ _) = [C.Retain | D.DefModWeak `notElem` mods]
		mutModes (D.TPClass D.TPMGeneric _ _) = [C.Retain | D.DefModWeak `notElem` mods]
		mutModes (D.TPFun {}) = [C.Copy]
		mutModes _ = []
		
idTp :: C.DataType
idTp = C.TPSimple "id" []
instancetypeTp :: C.DataType
instancetypeTp = C.TPSimple "instancetype" []

voidTp :: C.DataType
voidTp = C.TPSimple "void" []

initFun :: D.Def -> C.Fun
initFun D.Def{D.defPars = []} = C.Fun C.InstanceFun instancetypeTp "init" []
initFun D.Def{D.defPars = decls} = C.Fun C.InstanceFun instancetypeTp "initWith" (map funPar decls)

funPar :: D.Def -> C.FunPar
funPar D.Def {D.defName = name, D.defType = dataType} = C.FunPar name (showDataType dataType) name

createFun :: String -> D.Def -> C.Fun
createFun clsName D.Def{D.defPars = []} = C.Fun C.ObjectFun instancetypeTp (createFunName clsName) []
createFun clsName D.Def{D.defPars = decls} = C.Fun C.ObjectFun instancetypeTp (createFunName clsName ++ "With") (map funPar decls)

createFunName :: String -> String
createFunName (x1:x2:xs)
	| isUpper x1 && isUpper x2  =  createFunName(x2 : xs)
	| isUpper x1 = toLower x1 : x2 : xs
	| otherwise = x1 : x2 : xs
createFunName (x : xs) 
	| isUpper x = toLower x : xs
	| otherwise = x : xs
createFunName x = x


intefaceFuns :: [D.Def] -> [C.Fun]
intefaceFuns = map stm2Fun . filter (\v -> D.DefModPrivate `notElem` D.defMods v && D.isDef v)

stm2Fun :: D.Def -> C.Fun
stm2Fun d@D.Def{D.defPars = pars, D.defType = tp, D.defMods = mods} =
	C.Fun {
		C.funType = if D.DefModStatic `elem` mods then C.ObjectFun else C.InstanceFun, 
		C.funReturnType = showDataType tp, 
		C.funName = funName d, 
		C.funPars = map par pars}
	where
		par (D.Def{D.defName = nm, D.defType = ttp}) = C.FunPar nm (showDataType ttp) nm

genProtocol :: D.Class -> C.FileStm
genProtocol cl =
	C.Protocol {
		C.interfaceName = D.classNameWithPrefix cl,
		C.interfaceFuns = intefaceFuns defs,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs,
		C.interfaceExtends = addTraits cl $ 
			C.Extends ((cn . D.classNameWithPrefix . D.extendsClassClass . fromMaybe (error $ "No class extends for " ++ D.classNameWithPrefix cl) . D.extendsClass) $ D.classExtends cl) []
	}
	where
		defs =  D.classDefs cl
		cn n = if n == "ODObject" then "NSObject" else n


{- Implementation -}

stmToImpl :: D.Class -> C.FileStm
stmToImpl cl =
	C.Implementation {
		C.implName = clsName,
		C.implFields = [],
		C.implSynthesizes = (map (synthesize env) . filter needProperty) implFields,
		C.implFuns = nub $ constrFuns ++ [implInitialize env] ++ dealoc env 
			++ implFuns env defs ++ [instanceType] ++ staticGetters ++ copyImpls ++ (if equalsIsPosible cl then [equal, hash] else []) ++ [description],
		C.implStaticFields = map (implField env) staticFields
	}
	where
		clsName = D.classNameWithPrefix cl
		env = Env cl 0 D.TPVoid False
		defs :: [D.Def]
		defs = D.classDefsWithTraits cl
					
		constrFuns = fromMaybe [] $ fmap (\constr -> [implCreate cl constr, implInit env constr]) (D.classConstructor cl)
		implFields = filter needField defs
		needField f = D.isField f && not (D.isStatic f)
		isStaticField f = D.isStatic f && D.isField f
		staticFields = filter isStaticField defs
		staticGetters = (map staticGetter . filter((D.DefModPrivate `notElem`) . D.defMods)) staticFields
		staticGetter f = C.ImplFun (staticGetterFun f) [C.Return $ C.Ref $ fieldName env f]

		reloadedEquals = filter ( ("isEqual" == ). D.defName) defs
		equal :: C.ImplFun
		equal 
			| null reloadedEquals = C.ImplFun equalFun (equalPrelude clsName (not (null equalFields)) ++ equalsFun C.Self (C.Ref "o") equalFields)
			| otherwise = C.ImplFun equalFun $ [
				C.If (C.BoolOp Eq C.Self (C.Ref "other")) [C.Return $ C.BoolConst True] [],
				C.If (C.Not $ C.Ref "other") [C.Return $ C.BoolConst False] []]
				++ map reloadedEqualCall reloadedEquals
				++ [C.Return $ C.BoolConst False]
		reloadedEqualCall :: D.Def -> C.Stm
		reloadedEqualCall D.Def{D.defPars = [D.Def{D.defType = tp, D.defName = parName}]} = case tp of
			D.TPClass D.TPMTrait _ _ ->
				C.If (C.Call (C.Ref "other") "conformsTo" [("protocol", C.ProtocolRef (C.Ref $ D.dataTypeClassNameWithPrefix tp))] []) [
					C.Return $ C.Call C.Self "_isEqual" [(parName, C.Cast (C.TPSimple "id" [D.dataTypeClassNameWithPrefix tp]) (C.Ref "other") )] []
				] []
			_ ->
				C.If (C.Call (C.Ref "other") "isKindOf" [("class", C.Call (C.Ref $ D.dataTypeClassNameWithPrefix tp) "class" [] [])] []) [
					C.Return $ C.Call C.Self "_isEqual" [(parName, C.Cast (C.TPSimple ((D.dataTypeClassNameWithPrefix tp) ++ "*") []) (C.Ref "other") )] []
				] []
		reloadedEqualCall d = C.Stm $ C.Error $ "Incorrect equal def " ++ show d

		equalFields = maybe [] (D.defPars) $ D.classConstructor cl
		
 		hash = C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSUInteger" []) "hash" []) (hashFun equalFields)
 		description = C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSString*" []) "description" []) 
 			(descriptionFun descStart equalFields)
 		descStart = C.Call (C.Ref "NSMutableString") "stringWith" [("format", C.StringConst "<%@: ")] 
			[C.CCall (C.Ref "NSStringFromClass") [C.Call C.Self "class" [] []]]
		instanceType = C.ImplFun typeInstanceFun [C.Return $ C.Call (C.Ref clsName) "type" [] []]

fieldName :: Env -> D.Def -> String
fieldName env def 
	| D.DefModStatic `elem` D.defMods def =  staticName env $ D.defName def
	| otherwise = '_' : D.defName def

staticName :: Env -> String -> String
staticName env name = '_' : D.classNameWithPrefix (envClass env) ++ "_" ++ name


equalPrelude :: String -> Bool -> [C.Stm]
equalPrelude clsName o = [
			C.If (C.BoolOp Eq C.Self (C.Ref "other")) [C.Return $ C.BoolConst True] [],
			C.If (C.BoolOp Or (C.Not $ C.Ref "other") (C.Not equalClass)) [C.Return $ C.BoolConst False] []
			] ++ [C.Var selfTp "o" (C.Cast selfTp (C.Ref "other")) [] | o]
	where
		selfTp = (C.TPSimple (clsName ++ "*") []) 
		equalClass = C.Call (C.Call C.Self "class" [] []) "isEqual" [("", C.Call (C.Ref "other") "class" [] [])] []


equalFun :: C.Fun
equalFun = C.Fun C.InstanceFun (C.TPSimple "BOOL" []) "isEqual" [(C.FunPar "" (C.TPSimple "id" []) "other")]

equalsIsPosible :: D.Class -> Bool
equalsIsPosible cl = 
	(null $ filter ( (D.DefModMutable `elem` ). D.defMods) defs)
	|| (not $ null $ filter ( isVal . D.defMods) defs)
	where
		defs = D.classDefs cl
		isVal mods = (D.DefModField `elem` mods) && (D.DefModMutable `notElem` mods) && (D.DefModStatic `notElem` mods)

copyImpls :: [C.ImplFun]
copyImpls = [C.ImplFun (C.Fun C.InstanceFun idTp "copyWith" [C.FunPar "zone" (C.TPSimple "NSZone*" []) "zone"]) [C.Return C.Self]]

synthesize :: Env -> D.Def -> C.ImplSynthesize
synthesize env d@D.Def{D.defName = x} = C.ImplSynthesize x $ fieldName env d

implField :: Env -> D.Def -> C.ImplField
implField env d@D.Def{D.defType = tp, D.defMods = mods, D.defBody = e} = 
	C.ImplField (fieldName env d) (showDataType tp) ["__weak" | D.DefModWeak `elem` mods] $ 
		if D.DefModStatic `elem` mods && D.isConst e then tExpTo env{envCStruct = 1} tp e else C.Nop

implCreate :: D.Class -> D.Def -> C.ImplFun
implCreate cl constr@D.Def{D.defPars = constrPars} = let 
		clsName = D.classNameWithPrefix cl
		pars D.Def{D.defName = name} = (name, C.Ref name)
	in C.ImplFun (createFun clsName constr) [C.Return $
		autorelease $ C.Call (C.Call (C.Ref clsName) "alloc" [] []) (if null constrPars then "init" else "initWith") (map pars constrPars) []
	] 

retain :: C.Exp -> C.Exp
retain f 
	| arc = f
	| otherwise = C.Call f "retain" [] []			

autorelease :: C.Exp -> C.Exp
autorelease e 
	| arc = e
	| otherwise = C.Call e "autorelease" [] []

dealoc :: Env -> [C.ImplFun]
dealoc env@Env{envClass = cl}
	| arc = []
	| otherwise = [C.ImplFun (C.Fun C.InstanceFun voidTp "dealloc" []) $
		 mapMaybe releaseField (D.classFields cl) ++ [C.Stm $C.Call C.Super "dealloc" [] []]]
	where 
		releaseField d@D.Def{D.defType = D.TPClass{}} = Just $ C.Stm $ C.Call (C.Ref $ fieldName env d) "release" [] []
		releaseField _ = Nothing

implInitialize :: Env ->  C.ImplFun 		
implInitialize env@Env{envClass = cl} = let 
	fields = filter hasInitialize (D.classFields cl)
	hasInitialize D.Def{D.defBody = D.Nop} = False
	hasInitialize d@D.Def{D.defBody = b} = not (D.isConst b) && D.isField d && D.isStatic d
	selfClass = C.Call (C.Ref $ D.classNameWithPrefix cl) "class" [] []
	typeInit = C.Set Nothing (C.Ref $ staticName env "type") $ 
		C.Call (C.Ref "ODClassType") "classTypeWith" [("cls", selfClass)] []
	in C.ImplFun (C.Fun C.ObjectFun voidTp "initialize" []) (
			(C.Stm $ C.Call C.Super "initialize" [] []) : [C.If (C.BoolOp Eq C.Self selfClass)
				(typeInit : map (implInitField env) fields)
				[]
				])
				


implInit :: Env -> D.Def -> C.ImplFun
implInit env@Env{envClass = cl} constr@D.Def{D.defPars = constrPars}  = C.ImplFun (initFun constr) (
			[C.Set Nothing C.Self (superInit $ D.extendsClass $ D.classExtends cl)]
			++ declareWeakSelf env (implInitFields (filter hasField constrPars) (filter hasInit (D.classDefsWithTraits cl)))
			++ [C.Stm C.Nop, C.Return C.Self]
			)
	where
		hasInit D.Def{D.defBody = D.Nop} = False
		hasInit d = D.isField d && not (D.isStatic d)

		hasField f = any ((D.defName f == ) . D.defName) (D.classDefs cl)

		superInit Nothing = C.Call C.Super "init" [] []
		superInit (Just (D.ExtendsClass _ [])) = C.Call C.Super "init" [] []
		superInit (Just (D.ExtendsClass _ pars)) = C.Call C.Super "initWith" (map (D.defName *** tExp env) pars) []


		selfClass = C.Call (C.Ref $ D.classNameWithPrefix cl) "class" [] []
		call_init = [C.Stm $ C.Call C.Self "_init" [] []]
		needCall_init = (not $ D.isAbstract cl) && D.classContainsInit cl
		callInit = 
			if needCall_init then 
				if D.isFinal cl then call_init
				else [C.If (C.BoolOp Eq ( C.Call C.Self "class" [] []) selfClass) call_init []] 
			else []

		implInitFields :: [D.Def] -> [D.Def] -> [C.Stm]
		implInitFields [] [] 
			| not needCall_init = []
		implInitFields co fields = [C.If C.Self (map implConstrField co ++ map (implInitField env) fields ++ callInit) []]
		implConstrField d@D.Def{D.defName = name, D.defType = tp} = C.Set Nothing (C.Ref $ fieldName env d) (implRight tp) 
			where
				implRight D.TPFun{} = C.Call (C.Ref name) "copy" [] []
				implRight D.TPClass{} = retain $ C.Ref name
				implRight _ = C.Ref name

implInitField :: Env -> D.Def -> C.Stm
implInitField env d@D.Def{D.defBody = def, D.defType = tp} = C.Set Nothing (C.Ref $ fieldName env d) (tExpTo env tp def)
		
implFuns :: Env -> [D.Def] -> [C.ImplFun]
implFuns env = map stm2ImplFun . filter D.isDef
	where
		stm2ImplFun def@D.Def {D.defName = name, D.defBody = db, D.defMods = mods, D.defType = tp}
			| D.DefModAbstract `elem` mods = C.ImplFun (stm2Fun def) [C.Throw $ C.StringConst $ "Method " ++ name++ " is abstract"]
			| otherwise = C.ImplFun (stm2Fun def) (declareWeakSelf env $ tStm (env{envDataType = tp}) [] db)
			
		
{- Struct -}
genStruct :: D.Class -> ([C.FileStm], [C.FileStm])
genStruct cl = 
	([C.Struct name fields', con, eq, hash, descriptionDecl] ++ map def' defs ++  map def' staticFields ++ [wrapClass, C.EmptyLine], 
		[description] ++ map defImpl' defs ++ map staticFieldImpl' staticFields ++ [wrapImpl, C.EmptyLine])
	where
		name = D.classNameWithPrefix cl
		clDefs = D.classDefs cl
		defs = filter D.isDef clDefs
		env = Env cl 0 D.TPVoid False
		fields = filter (\d -> not (D.isStatic d) && D.isField d) clDefs
		staticFields = filter (\d -> (D.isStatic d) && D.isField d) clDefs
		fields' = map toField fields
		toField D.Def{D.defName = n, D.defType = tp, D.defMods = mods} = C.ImplField n (showDataType tp) 
				(["__weak" | D.DefModWeak `elem` mods] ++ ["__unsafe_unretained" | D.isTpClass tp || D.isTpEnum tp || D.isTpTrait tp] ) C.Nop
		con = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple name [], 
			C.cfunName = name ++ "Make", C.cfunPars = map toPar fields, C.cfunExps = 
				[C.Return $ C.ShortCast selfTp $ C.EArr $ map toEArr fields]
		}
		eq = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple "BOOL" [], C.cfunName = name ++ "Eq", 
			C.cfunPars = [C.CFunPar (C.TPSimple name []) "s1", C.CFunPar (C.TPSimple name []) "s2"], 
			C.cfunExps = equalsFun (C.Ref "s1") (C.Ref "s2") fields
		}
		hash = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple "NSUInteger" [], C.cfunName = name ++ "Hash", 
			C.cfunPars = [C.CFunPar (C.TPSimple name []) "self"], 
			C.cfunExps = hashFun fields
		}
		descriptionDecl = C.CFunDecl{C.cfunMods = [], C.cfunReturnType = C.TPSimple "NSString*" [], C.cfunName = name ++ "Description", 
			C.cfunPars = [C.CFunPar (C.TPSimple name []) "self"]
		}
		description = C.cfun descriptionDecl $ descriptionFun descStart fields
		descStart = C.Call (C.Ref "NSMutableString") "stringWith" [("string", C.StringConst $ "<" ++ name ++ ": ")] []
		toPar D.Def{D.defName = n, D.defType = tp}= C.CFunPar (showDataType tp) n
		
		def' d@D.Def{D.defType = tp, D.defPars = pars, D.defMods = mods} = C.CFunDecl{C.cfunMods = [], 
			C.cfunReturnType = showDataType tp, C.cfunName = structDefName name d,
			C.cfunPars =  if D.DefModStatic `notElem` mods then C.CFunPar (C.TPSimple name []) "self" : pars' else pars'}
			where
				pars' = map par' pars
				par' D.Def{D.defName = nn, D.defType = tpp} = C.CFunPar (showDataType tpp) nn
		selfTp = C.TPSimple name []
		wrapFun = C.Fun C.ObjectFun idTp "wrapWith" [C.FunPar "value" selfTp "value"]
		initWrapFun = C.Fun C.InstanceFun idTp "initWith" [C.FunPar "value" selfTp "value"]
		wrapClass = C.Interface {
			C.interfaceName = wrapName,
			C.interfaceExtends = C.Extends "NSObject" [],
			C.interfaceProperties = [C.Property "value" selfTp [C.ReadOnly, C.NonAtomic]],
			C.interfaceFuns = [wrapFun, initWrapFun],
			C.interfaceFields = []
		}
		defImpl' d@D.Def{D.defType = tp, D.defBody = e, D.defPars = pars, D.defMods = mods} = C.CFun{C.cfunMods = [], 
			C.cfunReturnType = showDataType tp, C.cfunName = structDefName name d,
			C.cfunPars = if D.DefModStatic `notElem` mods then C.CFunPar (C.TPSimple name []) "self" : pars' else pars',
			C.cfunExps = tStm (env {envDataType = tp}) [] e}
			where
				pars' = map par' pars
				par' D.Def{D.defName = nn, D.defType = tpp} = C.CFunPar (showDataType tpp) nn
		staticFieldImpl' d@D.Def{D.defName = n, D.defType = tp, D.defBody = e} = C.CFun{C.cfunMods = [], 
			C.cfunReturnType = showDataType tp, C.cfunName = structDefName name d,
			C.cfunPars = [],
			C.cfunExps = 
				if D.isConst e && not (D.isNop e) then 
					[C.Var{C.varType = showDataType tp, C.varName = "_ret", C.varExp = buildExp n e, C.varMods = ["static"]},
					 C.Return $ C.Ref "_ret"
					]
				else 
					[C.Var{C.varType = showDataType tp, C.varName = "_ret", C.varExp = C.Nil, C.varMods = ["static"]},
					 C.If (C.BoolOp Eq (C.Ref "_ret") C.Nil) [
					 	C.Set Nothing (C.Ref "_ret") $ buildExp n e
					 ] [],
					 C.Return $ C.Ref "_ret"
					]
			}
			where
				buildExp "type" D.Nop = C.Call (C.Ref "ODPType") "typeWith" [
					("cls", C.Call (C.Ref wrapName) "class" [] []),
					("name", C.StringConst name),
					("size", C.CCall (C.Ref "sizeof") [C.Ref name]),
					("wrap", C.Lambda [("data", C.tp "void*"), ("i", C.tp "NSUInteger")] [
						C.Return $ C.CCall (C.Ref "wrap") [C.Ref name, C.Index (C.Cast (C.tp $ name ++ "*") $ C.Ref "data") (C.Ref "i")]
						] idTp)
					] []
				buildExp _ ee = tExpTo env{envCStruct = 1} tp ee
		wrapImpl = C.Implementation {
			C.implName = wrapName,
			C.implFields = [C.ImplField "_value" selfTp [] C.Nop],
			C.implSynthesizes = [C.ImplSynthesize "value" "_value"],
			C.implFuns = [wrapFunImpl, initWrapFunImpl, descriptionImpl, equalsImpl, hashImpl] ++ maybeToList compareImpl ++ copyImpls,
			C.implStaticFields = []
		}	
		wrapName = name ++ "Wrap"
		selfWrapTp = C.TPSimple (wrapName ++ "*") []
		wrapFunImpl = C.ImplFun wrapFun [C.Return $ C.Call (C.Call (C.Ref wrapName) "alloc" [] []) "initWith" [("value", C.Ref "value")] []]
		initWrapFunImpl = C.ImplFun initWrapFun [
			C.Set Nothing C.Self $ C.Call C.Super "init" [] [],
			C.If C.Self [C.Set Nothing (C.Ref "_value") (C.Ref "value")] [],
			C.Return C.Self
			]
		descriptionImpl = C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSString*" []) "description" []) [
			C.Return $ C.CCall (C.Ref $ name ++ "Description") [C.Ref "_value"]
			]
		equalsImpl = C.ImplFun equalFun $ equalPrelude wrapName True ++ [
			C.Return $ C.CCall (C.Ref $ name ++ "Eq") $ [
				C.Ref "_value", C.Dot (C.Ref "o") (C.Ref "value")]
			]
		hashImpl = C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSUInteger" []) "hash" []) [
			C.Return $ C.CCall (C.Ref $ name ++ "Hash") [C.Ref "_value"]
			]
		compareFun = find (("compare" == ). D.defName) defs
		compareImpl = fmap (\d -> C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSInteger" []) "compare" [C.FunPar "to" selfWrapTp "to"]) [
			C.Return $ C.CCall (C.Ref $ structDefName name d) [C.Ref "_value", C.Dot (C.Ref "to") (C.Ref "value")]
			]) compareFun
		toEArr D.Def{D.defType = D.TPEArr n _, D.defName = nm}
			| n > 0 = C.EArr $ map ( C.Index (C.Ref nm) . C.IntConst) [0 .. n - 1]
		toEArr d = C.Ref . D.defName  $ d


equalsFun :: C.Exp -> C.Exp -> [D.Def] -> [C.Stm]
equalsFun _ _ [] = [C.Return $ C.BoolConst True]
equalsFun s1 s2 fields = [C.Return $ foldl foldEq C.Nop fields]
	where
		foldEq :: C.Exp -> D.Def -> C.Exp
		foldEq C.Nop d = eqd d
		foldEq p d = C.BoolOp And p (eqd d)
		eqd :: D.Def -> C.Exp
		eqd D.Def{D.defName = n, D.defType = tp} = equals True (tp, C.Dot s1 (C.Ref n)) (tp, C.Dot s2 (C.Ref n))

hashFun :: [D.Def] -> [C.Stm]
hashFun [] = [C.Return $ C.IntConst 0]
hashFun fields = 
	[C.Var (C.TPSimple "NSUInteger" []) "hash" (C.IntConst 0) []] ++
	map hashSet fields ++
	[C.Return $ C.Ref "hash"]
	where
		hashSet D.Def{D.defName = nm, D.defType = tp} = C.Set Nothing (C.Ref "hash") $
			C.MathOp Plus (C.MathOp Mul (C.Ref "hash") (C.IntConst 31)) $ hashCall tp (C.Dot C.Self (C.Ref nm))
		

hashCall :: D.DataType -> C.Exp -> C.Exp
hashCall tp ref = 
	case tp of
		D.TPClass D.TPMEnum _ _ -> C.Call ref "ordinal" [] []
		D.TPClass D.TPMStruct _ scl -> C.CCall (C.Ref $ D.classNameWithPrefix scl ++ "Hash") [ref]
		D.TPFloatNumber{} -> C.CCall (C.Ref $ show tp ++ "Hash") [ref]
		D.TPNumber{} -> ref
		D.TPBool -> ref
		D.TPChar -> ref
		D.TPEArr n atp -> arrHash atp n 0 C.Nop
		_ -> C.Call ref "hash" [] []
	where	
		arrElemHash atp i = hashCall atp $ C.Index ref (C.IntConst i)
		arrHash _ 0 _ _ = C.IntConst 0
		arrHash atp n i op 
			| i >= n = op
			| i == 0 = arrHash atp n 1 (arrElemHash atp i)
			| otherwise = arrHash atp n (i + 1) $ C.MathOp Plus (C.MathOp Mul (C.IntConst 13) op) (arrElemHash atp i)


stringFormatForType :: D.DataType -> String
stringFormatForType (D.TPNumber False 8) = "%ld"
stringFormatForType (D.TPNumber True 8) = "%lu"
stringFormatForType (D.TPNumber False 0) = "%ld"
stringFormatForType (D.TPNumber True 0) = "%lu"
stringFormatForType (D.TPNumber False _)= "%d"
stringFormatForType (D.TPNumber True _)= "%u"
stringFormatForType (D.TPChar)= "%d"
stringFormatForType (D.TPFloatNumber _) = "%f"
stringFormatForType D.TPBool = "%d"
stringFormatForType D.TPVoidRef = "%p"
stringFormatForType (D.TPEArr n tp)  = "[" ++ strs ", " (replicate n (stringFormatForType tp)) ++ "]"
stringFormatForType _ = "%@"

stringExpressionsForTp :: D.DataType -> C.Exp -> [C.Exp]
stringExpressionsForTp rtp ref = (case rtp of
			D.TPClass D.TPMStruct _ scl -> [C.CCall (C.Ref $ D.classNameWithPrefix scl ++ "Description") [ref]]
			D.TPEArr n etp -> concatMap (\j -> stringExpressionsForTp etp $ C.Index ref (C.IntConst j)) [0..n - 1]
			D.TPNumber False 0 -> [C.ShortCast (C.TPSimple "long" []) ref]
			D.TPNumber True 0 -> [C.ShortCast (C.TPSimple "unsigned long" []) ref]
			_ -> [ref]
			)

descriptionFun :: C.Exp -> [D.Def] -> [C.Stm]
descriptionFun start fields = 
	[C.Var (C.TPSimple "NSMutableString*" []) "description" start []] ++
	snd (mapAccumL append 0 $ filter pos fields)++
	[C.Stm end, C.Return $ C.Ref "description"]
	where
		pos D.Def{D.defType = D.TPFun{}} = False
		pos _ = True
		end = C.Call (C.Ref "description") "append" [("string", C.StringConst ">")] []
		append :: Int -> D.Def -> (Int, C.Stm)
		append i D.Def{D.defName = nm, D.defType = tp} = (i + 1, C.Stm $ C.Call (C.Ref "description") "append" 
			[("format", C.StringConst $ (if i > 0 then ", " else "") ++ nm ++ "="  ++ stringFormatForType tp)]
			(stringExpressionsForTp tp $ C.Dot C.Self (C.Ref nm)) )
			where
				
		
		

structDefName :: String -> D.Def -> String
structDefName sn D.Def{D.defName = dn, D.defPars = pars} = lowFirst sn ++ cap dn ++ (strs "" . map (cap . D.defName)) pars
	where 
		lowFirst (x1:x2:xs)
			| isUpper x1 && isUpper x2 = toLower x1 : lowFirst (x2:xs)
			| otherwise = x1:x2:xs
		lowFirst x = x
{- Enum -}

enumValuesFun :: C.Fun
enumValuesFun = C.Fun C.ObjectFun (C.TPSimple "NSArray*" []) "values" []

genEnumInterface :: D.Class -> [C.FileStm]
genEnumInterface cl = [
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = classExtends cl,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs',
		C.interfaceFuns = intefaceFuns defs' ++ map (enumItemGetterFun name) (D.enumItems cl) ++ [enumValuesFun],
		C.interfaceFields = []
	}]
	where 
		name = D.classNameWithPrefix cl
		defs' = filter ((/= "values") . D.defName) $ D.classDefs cl
	
enumItemGetterFun :: String -> D.Def -> C.Fun
enumItemGetterFun name D.Def{D.defName = itemName} = C.Fun C.ObjectFun (C.TPSimple (name ++ "*") []) itemName []

genEnumImpl :: D.Class -> [C.FileStm]
genEnumImpl cl@D.Class {} = [
	C.Implementation {
		C.implName = clsName,
		C.implFields = (map (implField env) . filter needProperty) defs,
		C.implSynthesizes = (map (synthesize env) . filter needProperty) defs,
		C.implFuns = nub $ [implCreate cl constr, implInit env constr, initialize] ++ dealoc env 
			++ implFuns env defs ++ map itemGetter items ++ [valuesFun],
		C.implStaticFields = map stField items ++ [C.ImplField valuesVarName (C.TPSimple "NSArray*" []) [] C.Nop] 
	}]
	where
		clsName = D.classNameWithPrefix cl
		env = Env  cl 0 D.TPVoid False
		valuesVarName =  "_" ++ clsName ++ "_values"
		items = D.enumItems cl
		defs = filter ((/= "values") . D.defName) $ D.classDefs cl
		constr = fromMaybe (error "No class constructor") (D.classConstructor cl)
		stField d = C.ImplField (fieldName env d) (C.TPSimple (clsName ++ "*") []) [] C.Nop
		itemGetter e = C.ImplFun (enumItemGetterFun clsName e) [C.Return $ C.Ref $ fieldName env e]
		initialize = C.ImplFun (C.Fun C.ObjectFun voidTp "initialize" []) (
			((C.Stm $ C.Call C.Super "initialize" [] []) : map initItem items) ++ [setValues])
		initItem :: D.Def -> C.Stm
		initItem d@D.Def{D.defBody = body} = C.Set Nothing (C.Ref $ fieldName env d) $ retain $ tExp env body
		valuesFun = C.ImplFun enumValuesFun [C.Return $ C.Ref valuesVarName]
		setValues :: C.Stm
		setValues = C.Set Nothing (C.Ref valuesVarName) (C.Arr $ map (C.Ref . (fieldName env)) items)


-----------------------------------------------------------------------------------------------------------------------------------------
-- Imports 
-----------------------------------------------------------------------------------------------------------------------------------------
procImports :: D.File -> ([C.FileStm], [C.FileStm])
procImports thisFile@D.File{D.fileClasses = classes} = (h, m)
	where 
		procClasses :: [(D.Class, Bool)]
		procClasses = concatMap procClass classes
		procClass :: D.Class -> [(D.Class, Bool)]
		procClass cl = procExtends (D.classExtends cl) 
			++ concatMap procDef (D.classDefsWithTraits cl)
		procDef :: D.Def -> [(D.Class, Bool)]
		procDef def = procDataType (D.defType def) ++ procExp (D.defBody def) ++ concatMap procDef (D.defPars def)
		procExtends :: D.Extends -> [(D.Class, Bool)]
		procExtends e = concatMap procExtendsRef (D.extendsRefs e) ++ 
			(fromMaybe [] . fmap (\(D.ExtendsClass _ p) -> procPars p ) ) (D.extendsClass e)
		procPars :: [D.CallPar] -> [(D.Class, Bool)] 
		procPars pars = concatMap (procExp . snd) pars
		procExtendsRef :: D.ExtendsRef -> [(D.Class, Bool)] 
		procExtendsRef (cl, _) = [(cl, True) | needRetCl cl] 
		procDataType :: D.DataType -> [(D.Class, Bool)] 
		procDataType (D.TPGenericWrap _ cl) = procDataType cl
		procDataType (D.TPClass _ _ cl) = retCl cl
		procDataType (D.TPObject _ cl) = retCl cl
		procDataType _ = []
		procExp :: D.Exp -> [(D.Class, Bool)] 
		procExp = D.forExp f
			where
				f (D.Dot l _) = procDataType $ D.exprDataType l
				f (D.BoolOp _ l _) = procDataType $ D.exprDataType l
				f _ = []
		retCl :: D.Class -> [(D.Class, Bool)] 
		retCl cl
			| needRetCl cl = [(cl, D.isStruct cl)]
			| otherwise = []
		needRetCl :: D.Class -> Bool
		needRetCl cl = not (D.isType cl) && not (D.isGeneric cl) 
		isStubObject cl = D.ClassModStub `elem` D.classMods cl && D.ClassModObject `elem` D.classMods cl
		
		hardImportFiles :: [D.File]
		hardImportFiles = (filter filterFile . nub . mapMaybe (D.classFile . fst) . filter snd) procClasses
		weekImportClasses :: [D.Class]
		weekImportClasses = (nub . filter ((\f -> f `notElem` hardImportFiles && filterFile f). D._classFile) . map fst . filter (not . snd)) procClasses
		weekImportFiles :: [D.File]
		weekImportFiles = (filter filterFile . nub . mapMaybe D.classFile) weekImportClasses
		filterFile :: D.File -> Bool
		filterFile f = f /= thisFile && (not (D.isCoreFile f) || D.isCoreFile thisFile)
		h = map cImport hardImportFiles ++ mapMaybe decl weekImportClasses
		m = map cImport weekImportFiles

		cImport f = C.Import (D.fileNameWithPrefix f ++ ".h")
		decl cl 
			| isStubObject cl = Nothing
			| D.isTrait cl =  Just $ C.ProtocolDecl . D.classNameWithPrefix $ cl
			| otherwise = Just $ C.ClassDecl . D.classNameWithPrefix $ cl

{-----------------------------------------------------------------------------------------------------------------------------------------
 - DataType 
 -----------------------------------------------------------------------------------------------------------------------------------------}
showDataType :: D.DataType -> C.DataType
showDataType (D.TPEArr 0 _) = C.TPSimple "id<CNImSeq>" []
showDataType (D.TPEArr n tp) = C.TPArr n $ show $ showDataType tp
showDataType (D.TPArr _ _) = C.TPSimple "id<CNImSeq>" []
showDataType (D.TPMap _ _)  = C.TPSimple "id<CNImMap>" []
showDataType (D.TPNumber False 1) = C.TPSimple "char" []
showDataType (D.TPNumber True 1) = C.TPSimple "unsigned char" []
showDataType (D.TPNumber False 4) = C.TPSimple "int" []
showDataType (D.TPNumber True 4) = C.TPSimple "unsigned int" []
showDataType (D.TPNumber False 8) = C.TPSimple "long" []
showDataType (D.TPNumber True 8) = C.TPSimple "unsigned long" []
showDataType (D.TPNumber False 0) = C.TPSimple "NSInteger" []
showDataType (D.TPNumber True 0) = C.TPSimple "NSUInteger" []
showDataType (D.TPFloatNumber 4) = C.TPSimple "float" []
showDataType (D.TPFloatNumber 8) = C.TPSimple "double" []
showDataType (D.TPFloatNumber 0) = C.TPSimple "CGFloat" []
showDataType D.TPString = C.TPSimple "NSString*" []
showDataType D.TPBool = C.TPSimple "BOOL" []
showDataType D.TPAny = idTp
showDataType D.TPAnyGeneric = idTp
showDataType D.TPNil = idTp
showDataType (D.TPClass D.TPMStruct _ c) = C.TPSimple (D.classNameWithPrefix c) []
showDataType tp@(D.TPClass D.TPMType _ _) = showDataType $ fromMaybe (error "Not found super type for type") $ D.superType tp
showDataType (D.TPClass D.TPMClass _ c) 
	| D.className c == "Object" = C.TPSimple "NSObject*" []
	| otherwise = C.TPSimple (D.classNameWithPrefix c ++ "*") []
showDataType (D.TPClass D.TPMEnum _ c) = C.TPSimple (D.classNameWithPrefix c ++ "*") []
showDataType (D.TPClass D.TPMTrait _ c) = C.TPSimple "id" [D.classNameWithPrefix c]
showDataType (D.TPClass{}) = idTp
showDataType (D.TPSelf _) = idTp
showDataType (D.TPTuple _) = C.TPSimple "CNTuple*" []
showDataType (D.TPOption _) = idTp
showDataType (D.TPFun D.TPVoid d) = C.TPBlock (showDataType d) []
showDataType (D.TPFun (D.TPTuple ss) d) = C.TPBlock (showDataType d) (map showDataType ss)
showDataType (D.TPFun s d) = C.TPBlock (showDataType d) [showDataType s]
showDataType (D.TPGenericWrap _ (D.TPClass D.TPMStruct _ _)) = idTp
showDataType (D.TPGenericWrap _ D.TPNumber{}) = idTp
showDataType (D.TPGenericWrap _ D.TPFloatNumber{}) = idTp
showDataType (D.TPGenericWrap _ D.TPBool) = idTp
showDataType (D.TPGenericWrap _ D.TPChar) = idTp
showDataType (D.TPGenericWrap _ D.TPVoid) = idTp
showDataType (D.TPGenericWrap _ c) = showDataType c
showDataType D.TPChar = C.TPSimple "unichar" []
showDataType tp = C.TPSimple (show tp) []

{- Exp -}
tPars :: Env -> D.Def -> [(D.Def, D.Exp)] -> [(String, C.Exp)]
tPars env _ = map (\(d, e) -> (D.defName d, maybeVal (D.exprDataType e, D.defType d) $ tExp env e))

tExpTo :: Env -> D.DataType -> D.Exp -> C.Exp 
tExpTo env tp e = maybeVal (D.exprDataType e, tp) (tExp env e)

castGeneric :: D.Exp -> C.Exp -> C.Exp
castGeneric dexp e = case D.exprDataType dexp of
	D.TPGenericWrap _ c@(D.TPClass D.TPMClass _ _) -> C.Cast (showDataType c) e
	D.TPGenericWrap _ c@(D.TPClass D.TPMTrait _ _) -> C.Cast (showDataType c) e
	D.TPGenericWrap _ c@(D.TPClass D.TPMEnum _ _) -> C.Cast (showDataType c) e
	D.TPGenericWrap _ c@D.TPTuple{} -> C.Cast (showDataType c) e
	D.TPGenericWrap _ c@D.TPArr{} -> C.Cast (showDataType c) e
	D.TPGenericWrap _ c@D.TPEArr{} -> C.Cast (showDataType c) e
	D.TPGenericWrap _ c@D.TPMap{} -> C.Cast (showDataType c) e
	_ -> e

data Env = Env{envClass :: D.Class, envCStruct :: Int, envDataType :: D.DataType, envWeakSelf :: Bool}

declareWeakSelf :: Env -> [C.Stm] -> [C.Stm]
declareWeakSelf env stms = if hasWeakSelf True stms then (C.Var (C.tp $ (D.classNameWithPrefix $ envClass env) ++ "*") "_weakSelf" C.Self ["__weak"]) : stms else stms

hasWeakSelf :: Bool -> [C.Stm] -> Bool
hasWeakSelf goToLambda stms = isJust $ C.forStms ((\_ -> True), (\_ -> Nothing), if goToLambda then (\_ -> True) else blockLambda, isWeakSelf) stms
	where
		isWeakSelf e@(C.Ref "_self") = Just e
		isWeakSelf _ = Nothing
		blockLambda C.Lambda{} = False
		blockLambda _ = True

selfCall :: Env -> C.Exp
selfCall env = if envWeakSelf env then C.Ref "_self" else C.Self

selfGetField :: Env -> String -> C.Exp
selfGetField env field = if envWeakSelf env then C.Arrow (C.Ref "_self") (C.Ref $ "_" ++ field) else C.Dot C.Self (C.Ref field)

setSelf :: Env -> [C.Stm] -> [C.Stm]
setSelf env stm = if hasWeakSelf False stm then [(C.Var (C.tp $ (D.classNameWithPrefix $ envClass env) ++ "*") "_self" (C.Ref "_weakSelf") [])] ++ stm else stm

tExp :: Env -> D.Exp -> C.Exp
tExp _ (D.IntConst i) = C.IntConst i
tExp _ (D.StringConst i) = C.StringConst i
tExp _ (D.Nil) = C.Nil
tExp _ (D.BoolConst i) = C.BoolConst i
tExp _ (D.FloatConst i) = C.FloatConst i
tExp env (D.BoolOp Eq l r) = equals True (D.exprDataType l, tExp env l) (D.exprDataType r, tExp env r) 
tExp env (D.BoolOp NotEq l r) = equals False (D.exprDataType l, tExp env l) (D.exprDataType r, tExp env r) 
tExp env (D.BoolOp t l r) = C.BoolOp (t' t) (tExpTo env tp l) (tExpTo env tp r)
	where
		t' ExactEq = Eq
		t' ExactNotEq = NotEq
		t' tt = tt
		tp = ttp t
		ttp And = D.TPBool
		ttp Or = D.TPBool
		ttp _ = D.unwrapGeneric $ D.exprDataType l
tExp env (D.MathOp t l r) = let 
		l' = tExp env l
		r' = tExp env r
		ltp = case D.exprDataType l of
			D.TPGenericWrap _ tt -> tt
			tt -> tt
		rtp = case D.exprDataType r of
			D.TPGenericWrap _ tt -> tt
			tt -> tt
	in case ltp of
		D.TPString -> case rtp of
			D.TPString -> C.Call l' "stringByAppending" [("string", r')] []
			_ -> C.Call l' "stringByAppending" [("format",  C.StringConst $ stringFormatForType rtp)] [maybeVal (D.exprDataType r, rtp) r']
		_ -> C.MathOp t (tExpTo env ltp l) (tExpTo env ltp r)
tExp env (D.PlusPlus e) = C.PlusPlus (tExp env e)
tExp env (D.MinusMinus e) = C.MinusMinus (tExp env e)


tExp env (D.Dot (D.Self (D.TPClass D.TPMStruct _ c)) (D.Call d@D.Def {D.defName = name, D.defMods = mods} _ pars)) 
	| D.DefModField `elem` mods = C.Dot (C.Ref "self") (C.Ref name)
	| otherwise = C.CCall (C.Ref $ structDefName (D.classNameWithPrefix c) d) (C.Ref "self" : (map snd . tPars env d) pars)

tExp env (D.Dot (D.Self stp) (D.Call d@D.Def{D.defMods = mods, D.defName = name} _ pars)) 
	| D.DefModField `elem` mods && null pars && D.DefModSuper `notElem` mods && (not (envWeakSelf env) || D.DefModStatic `elem` mods) = 
		C.Ref $ fieldName env d
	| D.DefModField `elem` mods && D.DefModSuper `notElem` mods && not ((envWeakSelf env) || D.DefModStatic `elem` mods) = 
		C.CCall (C.Ref $ fieldName env d) ((map snd . tPars env d) pars)
	| D.DefModStatic `elem` mods = C.Call (C.Ref $ D.classNameWithPrefix $ D.tpClass stp) name (tPars env d pars) []
	| D.DefModField `elem` mods && null pars = selfGetField env name
	| otherwise = C.Call (selfCall env) name (tPars env d pars) [] 
		
tExp env (D.Dot l (D.Call dd@D.Def{D.defName = name, D.defMods = mods} _ pars))
	| D.DefModStatic `elem` mods && isStubObject = 
		if D.DefModField `elem` mods then C.Ref name
		else C.CCall (C.Ref $ name) ((map snd . tPars env dd) pars)
	| D.DefModApplyLambda `elem` mods = C.CCall (castGeneric l $ tExp env l) ((map snd . tPars env dd) pars) 
	| D.DefModField `elem` mods && null pars && 
		not (D.DefModStruct `elem` mods && D.DefModStatic `elem` mods) = 
			C.Dot (castGeneric l $ tExpTo env ltp l) (C.Ref name)
	| D.DefModField `elem` mods && 
		not (D.DefModStruct `elem` mods && D.DefModStatic `elem` mods) = 
			C.Dot (castGeneric l $ tExpTo env ltp l) $ C.CCall (C.Ref name) ((map snd . tPars env dd) pars)
	| D.DefModConstructor `elem` mods = callConstructor env dd pars
	| D.DefModStruct `elem` mods = case ltp  of
		(D.TPClass D.TPMStruct _ c) -> structCall (D.classNameWithPrefix c) (castGeneric l $ tExpTo env ltp l)
		(D.TPObject D.TPMStruct c) -> C.CCall (C.Ref $ structDefName (D.classNameWithPrefix c) dd) ((map snd . tPars env dd) pars)
		tp -> structCall (show tp) (castGeneric l $ tExpTo env ltp l)
	| otherwise = C.Call (castGeneric l $ tExp env l) (funName dd) (tPars env dd pars) []
	where
		 structCall c self = C.CCall (C.Ref $ structDefName c dd) (self : (map snd . tPars env dd) pars)
		 ltp = D.unwrapGeneric $ D.exprDataType l
		 isStubObject = case ltp of
		 	D.TPObject _  cl -> D.ClassModStub `elem` D.classMods cl && D.ClassModObject `elem` D.classMods cl
		 	_ -> False
tExp env (D.Dot l (D.Is dtp)) = C.Call (castGeneric l $ tExp env l) "isKindOf" 
	[("class", C.Call (C.Ref $ D.dataTypeClassNameWithPrefix dtp) "class" [] [])] []
tExp env (D.Dot l (D.As dtp)) = case dtp of
	D.TPClass D.TPMTrait _ _ -> C.Call (C.Ref "ODObject") "asKindOf" 
		[("Protocol", C.ProtocolRef $ C.Ref $ D.dataTypeClassNameWithPrefix dtp), ("object", castGeneric l $ tExp env l)] []
	_ -> C.Call (C.Ref "ODObject") "asKindOf" 
		[("class", C.Call (C.Ref $ D.dataTypeClassNameWithPrefix dtp) "class" [] []), ("object", castGeneric l $ tExp env l)] []
tExp env (D.Dot l (D.CastDot dtp)) = tExp env (D.Cast dtp l)
tExp env (D.Dot l (D.Cast dtp c)) = tExp env (D.Cast dtp (D.Dot l c))
tExp env (D.Dot l (D.LambdaCall c)) = C.CCall (tExp env (D.Dot l c)) [] 


tExp env (D.Self _) = selfCall env
tExp _ (D.Super _) = C.Super
tExp env (D.LambdaCall e) = C.CCall (tExp env e) []
tExp env (D.Call d@D.Def{D.defName = name, D.defMods = mods, D.defType = tp} _ pars)
	| D.DefModField `elem` mods = C.Ref $ fieldName env d
	| D.DefModEnumItem `elem` mods = C.Ref $ fieldName env d
	| D.DefModLocal `elem` mods && null pars = C.Ref name
	| D.DefModConstructor `elem` mods = callConstructor env d pars
	| D.DefModGlobalVal `elem` mods = C.Ref name
	| D.DefModObject `elem` mods = C.Ref $ D.dataTypeClassNameWithPrefix tp
	| otherwise = C.CCall (C.Ref name) (map snd . tPars env d $ pars)
tExp env (D.If cond t f) = C.InlineIf (tExpTo env D.TPBool cond) (tExp env t) (tExp env f)
tExp env (D.Index e i) = case D.exprDataType e of
	D.TPObject D.TPMEnum _ -> C.Index (C.Call (tExp env e)  "values" [] []) (tExp env i)
	D.TPMap k _ -> C.Call (tExp env e) "apply" [("key", tExpTo env k i)] []
	D.TPArr _ _ -> C.Call (tExp env e) "apply" [("index", tExpTo env D.uint i)] []
	_ -> C.Index (tExp env e) (tExp env i)
tExp env (D.Lambda pars e rtp) = 
	let 
		isNeedUnwrap :: D.DataType -> Bool
		{-isNeedUnwrap (D.TPGenericWrap (D.TPClass D.TPMStruct _ _)) = True
		isNeedUnwrap (D.TPGenericWrap D.TPInt) = True
		isNeedUnwrap (D.TPGenericWrap D.TPBool) = True
		isNeedUnwrap (D.TPGenericWrap D.TPFloat) = True
		isNeedUnwrap (D.TPGenericWrap D.TPUInt) = True-}
		isNeedUnwrap _ = False
		par' (name, tp) 
			| isNeedUnwrap tp = (name ++ "_", idTp)
			| otherwise = (name, showDataType tp)
		unwrapPars :: [C.Stm]
		unwrapPars = (map unwrapPar. filter (isNeedUnwrap . snd)) pars
		unwrapPar ::(String, D.DataType) -> C.Stm
		unwrapPar (name, D.TPGenericWrap gw tp) = C.Var (showDataType tp) name (maybeVal (D.TPGenericWrap gw tp, tp) $ C.Ref $ name ++ "_") []
		stm = tStm env{envDataType = rtp} [] e
	in
	C.Lambda (map par' pars) (unwrapPars ++ setSelf env stm) (showDataType rtp)
tExp env (D.Weak expr) = tExp env{envWeakSelf = True} expr
tExp env (D.Arr exps) = C.Arr $ map (tExpToType env D.tpGeneric) exps
tExp env (D.Map exps) = C.Map $ map (tExpToType env D.tpGeneric *** tExpToType env D.tpGeneric) exps
tExp env (D.Tuple exps) = C.CCall (C.Ref $ "tuple" ++ if length exps == 2 then "" else show (length exps) ) $ map (tExpToType env D.tpGeneric) exps
tExp env (D.Opt e) = let tp = D.exprDataType e
	in C.Call (C.Ref "CNOption") "apply" [("value", maybeVal (tp, D.wrapGeneric tp) (tExp env e))] []
tExp env (D.Some e) = let tp = D.exprDataType e
	in C.Call (C.Ref "CNOption") "some" [("value", maybeVal (tp, D.wrapGeneric tp) (tExp env e))] []
tExp _ (D.None _) = C.Call (C.Ref "CNOption") "none" [] []
tExp env (D.Not e) = C.Not (tExpTo env D.TPBool e)
tExp env (D.Negative e) = C.Negative (tExp env e)
tExp env (D.Cast dtp e) = let 
		stp = D.exprDataType e
		stp' = D.unwrapGeneric stp
		toString format = C.Call (C.Ref "NSString") "stringWith" [("format", C.StringConst format)] (stringExpressionsForTp stp $ tExpTo env stp e)
		cast = C.Cast (showDataType dtp) e'
		e' = (tExpTo env stp' e)
		voidRefStructCast = C.CCall (C.Ref "voidRef") [e']
		sear exps = C.EArr $ map (tExp env) exps
		ear etp exps = if envCStruct env == 2 then sear exps
			else C.ShortCast (C.TPArr 0 $ show $ showDataType $ D.unwrapGeneric etp) $ sear exps
	in case (stp', D.unwrapGeneric dtp) of
		(D.TPNumber{}, D.TPString) -> toString $ stringFormatForType stp
		(D.TPFloatNumber{}, D.TPString) -> toString $ stringFormatForType stp
		(D.TPFloatNumber{}, D.TPFloatNumber{}) -> case e of
			D.FloatConst n -> C.FloatConst n
			_ -> cast
		(D.TPNumber{}, D.TPFloatNumber{}) -> case e of
			D.IntConst n -> C.FloatConst $ read $ show n ++ ".0"
			_ -> cast
		(D.TPNumber{}, D.TPNumber{}) -> case e of
			D.IntConst n -> C.IntConst n
			_ -> cast
		(D.TPArr{}, D.TPEArr 0 etp) ->
			case e of
				D.Arr exps -> case etp of
					D.TPClass{} -> C.EArrConst "arrs" (show $ showDataType etp) $ map (tExp env) exps
					_ -> C.EArrConst ("arr" ++ (dataTypeSuffix etp)) "" $ map (tExp env) exps
				_ -> error $ "Could not convert to EArr " ++ show e
		(D.TPArr{}, D.TPEArr _ etp) -> 
			case e of
				D.Arr exps -> ear etp exps
				_ -> error $ "Could not convert to EArr " ++ show e
		(D.TPNumber{}, D.TPVoidRef) -> voidRefStructCast
		(D.TPFloatNumber{}, D.TPVoidRef) -> voidRefStructCast
		(D.TPClass D.TPMStruct _ _, D.TPVoidRef) -> voidRefStructCast
		(D.TPVoidRef, _) -> case showDataType dtp of 
			C.TPSimple t a -> C.RefUp $ C.Cast (C.TPSimple (t ++ "*") a)  e'
		(D.TPArr _ etp, D.TPVoidRef) ->
			case e of
				D.Arr exps -> ear etp exps
				_ -> cast
		(D.TPAny, dd@D.TPNumber{}) -> C.CCall (C.Ref $ "unum" ++ dataTypeSuffix dd) [e']
		(D.TPAny, dd@D.TPFloatNumber{}) -> C.CCall (C.Ref $ "unum" ++ dataTypeSuffix dd) [e']
		_ -> cast 

tExp env (D.StringBuild pars lastString) = C.Call (C.Ref "NSString") "stringWith" [("format", C.StringConst format)] pars'
	where
		format = concatMap (\(prev, e) -> prev ++ stringFormatForType (D.exprDataType e) ) pars ++ lastString
		pars' = concatMap (par' . snd) pars
		par' expr = stringExpressionsForTp (D.exprDataType expr) $ tExp env expr
tExp _ e@D.ExpDError{} = C.Error $ show e
tExp _ e@D.ExpLError{} = C.Error $ show e
tExp _ D.Nop = C.Nop

tExp _ (D.Braces []) = C.Nop
tExp env (D.Braces [x]) = tExp env x
tExp env e@(D.Braces _) = 
	let 
		rtp = D.exprDataType e
		lambda = C.Lambda [] (setSelf env $ tStm env{envDataType = rtp} [] e) (showDataType rtp)
	in C.CCall lambda []
tExp env (D.Return _ e) = tExp env e
tExp _ x = C.Error $ "No tExp for " ++ show x

tExpToType :: Env -> D.DataType -> D.Exp -> C.Exp
tExpToType env tp e = maybeVal (D.exprDataType e, tp) (tExp env e)

tStm :: Env -> [D.Exp] -> D.Exp -> [C.Stm]
tStm _ _ (D.Nop) = []

tStm _ _ (D.Braces []) = []
tStm v _ (D.Braces [x]) = tStm v [] x
tStm v _ (D.Braces xs) = concatMap (tStm v xs) xs

tStm v _ (D.If cond t f) = [C.If (tExpTo v D.TPBool cond) (tStm v [] t) (tStm v [] f)]
tStm v _ (D.While cond t) = [C.While (tExpTo v D.TPBool cond) (tStm v [] t)]
tStm v _ (D.Synchronized ref b) = [C.Synchronized (tExp v ref) (tStm v [] b)]
tStm v _ (D.Do cond t) = [C.Do (tExpTo v D.TPBool cond) (tStm v [] t)]

tStm env _ (D.Set tp l r) = let 
	l' = tExp env l
	r' = tExp env r
	ltp = D.exprDataType l
	rtp = D.exprDataType r
	in [C.Set tp l' (maybeVal (rtp, ltp) r')]
tStm Env{envDataType = D.TPVoid} _ (D.Return True _) = [C.Return C.Nop]
tStm env@Env{envDataType = D.TPVoid} _ (D.Return _ e) = [C.Stm $ tExp env{envCStruct = 0} e]
tStm env@Env{envDataType = tp}  _ (D.Return _ e) = [C.Return $ tExpToType env{envCStruct = 0} tp e]
tStm env parexps (D.Val def@D.Def{D.defName = name, D.defType = tp, D.defBody = e, D.defMods = mods}) = 
	[C.Var (showDataType tp) name (tExpToType env tp e) (["__block" | needBlock] ++ ["__weak" | isWeak])]
	where 
		needBlock = D.DefModMutable `elem` mods && existsSetInLambda
		existsSetInLambda = any (isJust . setsInLambda) parLambdas
		parLambdas :: [D.Exp]
		parLambdas = D.forExp isLambda (D.Braces parexps)
		isLambda ee@D.Lambda{} = [ee]
		isLambda _ = []
		isWeak = D.DefModWeak `elem` mods
		setsInLambda (D.Lambda _ lambdaExpr _) = D.forExp isSet lambdaExpr
		isSet ee@(D.Set _ (D.Call d _ _) _) = if D.defName d == D.defName def then Just ee else Nothing
		isSet ee@(D.PlusPlus (D.Call d _ _)) = if D.defName d == D.defName def then Just ee else Nothing
		isSet ee@(D.MinusMinus (D.Call d _ _)) = if D.defName d == D.defName def then Just ee else Nothing
		isSet _ = Nothing
tStm env _ (D.Throw e) = [C.Throw $ tExp env e]
tStm _ _ D.Break = [C.Break]
tStm env pe (D.Weak expr) = tStm env{envWeakSelf = True} pe expr
tStm env _ x = [C.Stm $ tExp env x]

equals :: Bool -> (D.DataType, C.Exp) -> (D.DataType, C.Exp) -> C.Exp
equals False (D.TPClass D.TPMEnum _ _, e1) (D.TPClass D.TPMEnum _ _, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPClass D.TPMClass _ cl, e1) (_, e2) 
	| not (equalsIsPosible cl) = C.BoolOp NotEq e1 e2
equals False (D.TPNumber{}, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPChar, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPBool, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPNil, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (_, e1) (D.TPNil, e2) = C.BoolOp NotEq e1 e2
equals False s1@(_, _) s2@(_, _) = C.Not $ equals True s1 s2

equals True (D.TPClass D.TPMEnum _ _, e1) (D.TPClass D.TPMEnum _ _, e2) = C.BoolOp Eq e1 e2
equals True (dtp@(D.TPClass D.TPMStruct _ c), e1) (stp, e2) = C.CCall (C.Ref (D.classNameWithPrefix c ++ "Eq")) [e1, maybeVal (stp, dtp) e2]
equals True (D.TPGenericWrap{}, e1) (D.TPGenericWrap{}, e2) = C.Call e1 "isEqual" [("", e2)] []
equals True (stp@(D.TPGenericWrap _ stp'), e1) (dtp, e2) = equals True (stp', maybeVal (stp, dtp) e1) (dtp, e2)
equals True (stp, e1) (dtp@(D.TPGenericWrap _ dtp'), e2) = equals True (stp, e1) (dtp', maybeVal (stp, dtp) e2)
equals True (D.TPFloatNumber 0, e1) (_, e2) = C.CCall (C.Ref "eqf") [e1, e2]
equals True (D.TPFloatNumber 4, e1) (_, e2) = C.CCall (C.Ref "eqf4") [e1, e2]
equals True (D.TPFloatNumber 8, e1) (_, e2) = C.CCall (C.Ref "eqf8") [e1, e2]
equals True (D.TPNumber{}, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (D.TPChar, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (D.TPBool, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (D.TPNil, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (D.TPEArr _ _, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (_, e1) (D.TPNil, e2) = C.BoolOp Eq e1 e2
equals True (D.TPClass D.TPMClass _ cl, e1) (_, e2) 
	| not (equalsIsPosible cl) = C.BoolOp Eq e1 e2
equals True (_, e1) (_, e2) = C.Call e1 "isEqual" [("", e2)] []

callConstructor :: Env -> D.Def -> [(D.Def, D.Exp)] -> C.Exp
callConstructor env d@D.Def{D.defType = tp} pars = let
	name = D.dataTypeClassNameWithPrefix $ D.resolveTypeAlias tp
	earr env' = C.EArr ((map snd. tPars env' d) pars) 
	in case tp of 
		D.TPClass D.TPMStruct _ _ -> 
			if envCStruct env == 1 then C.ShortCast (showDataType tp) $ earr env{envCStruct = 2}
			else if envCStruct env == 2 then earr env
			else C.CCall (C.Ref (name++ "Make")) ((map snd. tPars env d) pars)
	 	_ -> C.Call 
				(C.Ref name) 
				(createFunName $ name ++ if null pars then "" else "With") 
				(tPars env d pars)
				[]

dataTypeSuffix :: D.DataType -> String
dataTypeSuffix (D.TPNumber False 1) = "c"
dataTypeSuffix (D.TPNumber True 1) = "uc"
dataTypeSuffix (D.TPNumber False 4) = "i4"
dataTypeSuffix (D.TPNumber True 4) = "ui4"
dataTypeSuffix (D.TPNumber False 0) = "i"
dataTypeSuffix (D.TPNumber True 0) = "ui"
dataTypeSuffix (D.TPNumber False 8) = "i8"
dataTypeSuffix (D.TPNumber True 8) = "ui8"
dataTypeSuffix D.TPBool = "b"
dataTypeSuffix D.TPChar = "s"
dataTypeSuffix (D.TPFloatNumber 0) = "f"
dataTypeSuffix (D.TPFloatNumber 4) = "f4"
dataTypeSuffix (D.TPFloatNumber 8) = "f8"

data MaybeValTP = TPGen | TPNum | TPStruct | TPNoMatter | TPBool | TPFloat
maybeVal :: (D.DataType, D.DataType) -> C.Exp -> C.Exp
maybeVal (stp, dtp) e = let 
	tp D.TPGenericWrap{} = TPGen
	tp (D.TPClass D.TPMGeneric _ _) = TPGen
	tp (D.TPClass D.TPMStruct _ _) = TPStruct
	tp D.TPNumber{} = TPNum
	tp D.TPChar{} = TPNum
	tp D.TPFloatNumber{} = TPFloat
	tp D.TPBool{} = TPBool
	tp _ = TPNoMatter
	fnm = ("num" ++ ) . dataTypeSuffix
	in case (tp stp, tp dtp) of
		(TPStruct, TPGen) -> C.CCall (C.Ref "wrap") [C.Ref $ D.classNameWithPrefix $ D.tpClass stp, e]
		(TPGen, TPStruct) -> C.CCall (C.Ref "uwrap") [C.Ref $ D.classNameWithPrefix $ D.tpClass dtp, e]
		(TPNum, TPGen) -> case e of
			C.IntConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref $ fnm stp) [e]
		(TPGen, TPNum) -> C.CCall (C.Ref $ "u" ++ fnm dtp) [e]
		(TPFloat, TPGen) -> case e of
			C.FloatConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref $ fnm stp) [e]
		(TPGen, TPFloat) -> C.CCall (C.Ref $ "u" ++ fnm dtp) [e]
		(TPBool, TPGen) -> case e of
			C.BoolConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref $ fnm stp) [e]
		(TPGen, TPBool) -> C.CCall (C.Ref $ "u" ++ fnm dtp) [e]
		_ -> e
