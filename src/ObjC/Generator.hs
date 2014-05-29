module ObjC.Generator (
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
import qualified ObjD.Link.Struct   as D

arc :: Bool
arc = True

toObjC :: D.Core -> D.File -> ((String, [C.FileStm]), (String, [C.FileStm]))
toObjC core f@D.File{D.fileClasses = classes} = 
	let 
		name = D.fileNameWithPrefix f
		isClass c = D.isRealClass c && not (D.isStruct c) && not (D.isTrait c) && not (D.isEnum c)
		cls = filter isClass classes
		isStruct c = D.isRealClass c && D.isStruct c
		structs = filter isStruct classes
		enums = filter D.isEnum classes
		isTrait c = D.isRealClass c && D.isTrait c

		dImports' = procImports core f
		
		
		h = [C.Import $ if D.isCoreFile f then "objdcore.h" else "objd.h"] 
			++ fst dImports' 
			++ [C.EmptyLine] 
			++ map classDecl (cls ++ enums) 
			++ map (C.ProtocolDecl . D.classNameWithPrefix) (filter D.isTrait classes) 
			++ map structDecl structs
			++ [C.EmptyLine] 
			++ genClasses D.isEnum classes
			++ genClasses (not . D.isEnum) classes
		
		genClasses b = concatMap ((++ [C.EmptyLine, C.EmptyLine]) . fst . gen) . (filter b)

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
funName D.Def{D.defName = d} = d

stmToInterface :: D.Class -> C.FileStm
stmToInterface cl =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = classExtends cl,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs,
		C.interfaceFuns = constrFuns ++ [typeInstanceFun | D.ClassModTraitImpl `notElem` D.classMods cl]
			++ intefaceFuns defs ++ staticGetters,
		C.interfaceFields = [(C.Protected, map (implField env) implFields)]
	}
	where 
		env = Env cl 0 D.TPVoid False
		name = D.classNameWithPrefix cl
		defs = filter (not . D.isInline) $ D.classDefs cl
		implFields = filter needField (D.classDefsWithTraits cl)
		needField f = D.isField f && not (D.isStatic f)
		constrFuns = maybe [] (\constr -> [createFun name constr, initFun constr]) (D.classConstructor cl)
		staticGetters = (map staticGetterFun .filter (\f -> 
			(D.DefModPrivate `notElem` D.defMods f) && D.isField f && D.isStatic f)) defs

typeInstanceFun :: C.Fun		
typeInstanceFun = C.Fun C.InstanceFun (C.TPSimple "CNClassType*" []) "type" []		

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
stm2Fun D.Def{D.defName = "isEqual", D.defPars = [D.Def{D.defName = "to"}]} =
	C.Fun {C.funType = C.InstanceFun, C.funReturnType = C.TPSimple "BOOL" [], C.funName = "isEqual", C.funPars = [C.FunPar "" idTp "to"]}
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
		C.interfaceExtends = case addTraits cl $ C.Extends "" [] of 
			C.Extends "" [] -> C.Extends "" ["NSObject"]
			e -> e
	}
	where
		defs =  D.classDefs cl


{- Implementation -}

stmToImpl :: D.Class -> C.FileStm
stmToImpl cl =
	C.Implementation {
		C.implName = clsName,
		C.implFields = [],
		C.implSynthesizes = (map (synthesize env) . filter needProperty) implFields,
		C.implFuns = nub $ constrFuns ++ maybeToList (implInitialize env) ++ dealoc env 
			++ implFuns env defs ++ [instanceType | D.ClassModTraitImpl `notElem` D.classMods cl] ++ staticGetters ++ copyImpls,
		C.implStaticFields = map (implField env) staticFields
	}
	where
		clsName = D.classNameWithPrefix cl
		env = Env cl 0 D.TPVoid False
		defs :: [D.Def]
		defs = filter (\f -> not (D.isInline f && D.isPrivate f)) $ D.classDefsWithTraits cl
					
		constrFuns = maybe [] (\constr -> [implCreate cl constr, implInit env constr]) (D.classConstructor cl)
		implFields = filter needField defs
		needField f = D.isField f && not (D.isStatic f)
		isStaticField f = D.isStatic f && D.isField f
		staticFields = filter isStaticField defs
		staticGetters = (map staticGetter . filter((D.DefModPrivate `notElem`) . D.defMods)) staticFields
		staticGetter f = C.ImplFun (staticGetterFun f) [C.Return $ C.Ref $ fieldName env f]
		instanceType = C.ImplFun typeInstanceFun [C.Return $ C.Call (C.Ref clsName) "type" [] []]

fieldName :: Env -> D.Def -> String
fieldName env def 
	| D.DefModEnumItem `elem` D.defMods def =  D.classNameWithPrefix (envClass env) ++ "_" ++ D.defName def
	| D.DefModStatic `elem` D.defMods def =  staticName env $ D.defName def
	| otherwise = '_' : D.defName def

staticName :: Env -> String -> String
staticName env name = '_' : D.classNameWithPrefix (envClass env) ++ "_" ++ name


copyImpls :: [C.ImplFun]
copyImpls = [C.ImplFun (C.Fun C.InstanceFun idTp "copyWith" [C.FunPar "zone" (C.TPSimple "NSZone*" []) "zone"]) [C.Return C.Self]]

synthesize :: Env -> D.Def -> C.ImplSynthesize
synthesize env d@D.Def{D.defName = x} = C.ImplSynthesize x $ fieldName env d

implField :: Env -> D.Def -> C.ImplField
implField env d@D.Def{D.defType = tp, D.defMods = mods, D.defBody = e} = 
	C.ImplField (fieldName env d) (showDataType tp) (["__weak" | D.DefModWeak `elem` mods] ++ ["volatile" | D.DefModVolatile `elem` mods]) $ 
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

implInitialize :: Env -> Maybe C.ImplFun 		
implInitialize env@Env{envClass = cl} = let 
	fields = filter hasInitialize (D.classFields cl)
	hasInitialize D.Def{D.defBody = D.Nop} = False
	hasInitialize d@D.Def{D.defBody = b} = not (D.isConst b) && D.isField d && D.isStatic d
	selfClass = C.Call (C.Ref $ D.classNameWithPrefix cl) "class" [] []
	typeInit = [C.Set Nothing (C.Ref $ staticName env "type") $ 
		C.Call (C.Ref "CNClassType") "classTypeWith" [("cls", selfClass)] [] | D.ClassModTraitImpl `notElem` D.classMods cl]
	stms = typeInit ++ map (implInitField env) fields
	in if null stms then Nothing else Just $ C.ImplFun (C.Fun C.ObjectFun voidTp "initialize" []) (
			(C.Stm $ C.Call C.Super "initialize" [] []) : [C.If (C.BoolOp Eq C.Self selfClass)
				stms
				[]
				])
				


implInit :: Env -> D.Def -> C.ImplFun
implInit env@Env{envClass = cl} constr@D.Def{D.defPars = constrPars}  = C.ImplFun (initFun constr) (
			[C.Set Nothing C.Self (superInit $ D.extendsClass $ D.classExtends cl)]
			++ declareWeakSelf env (implInitFields 
					(filter hasField constrPars) 
					(filter hasInit (D.classDefsWithTraits cl)))
			++ [C.Stm C.Nop, C.Return C.Self]
			)
	where
		hasInit D.Def{D.defBody = D.Nop} = False
		hasInit d = D.isField d && not (D.isStatic d)

		hasField f = any ((D.defName f == ) . D.defName) (D.classDefs cl)

		superInit Nothing = C.Call C.Super "init" [] []
		superInit (Just (D.ExtendsClass _ [])) = C.Call C.Super "init" [] []
		superInit (Just (D.ExtendsClass _ pars)) = C.Call C.Super "initWith" (map (\(d, e) -> (D.defName d, tExpTo env (D.defType d) e)) pars) []


		selfClass = C.Call (C.Ref $ D.classNameWithPrefix cl) "class" [] []
		call_init = [C.Stm $ C.Call C.Self "_init" [] []]
		needCall_init = not (D.isAbstract cl) && D.classContainsInit cl
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
			| otherwise = C.ImplFun (stm2Fun def) (declareWeakSelf env $ translate (env{envDataType = tp}) db)
			
		
{- Struct -}
genStruct :: D.Class -> ([C.FileStm], [C.FileStm])
genStruct cl = 
	([C.Struct name fields', con] ++ map def' defs ++  map def' staticFields ++ [wrapClass, C.EmptyLine], 
		map defImpl' defs ++ map staticFieldImpl' staticFields ++ [wrapImpl, C.EmptyLine])
	where
		name = D.classNameWithPrefix cl
		clDefs = D.classDefs cl
		defs = filter D.isDef clDefs
		env = Env cl 0 D.TPVoid False
		fields = filter (\d -> not (D.isStatic d) && D.isField d) clDefs
		staticFields = filter (\d -> D.isStatic d && D.isField d) clDefs
		fields' = map toField fields
		toField D.Def{D.defName = n, D.defType = tp, D.defMods = mods} = C.ImplField n (showDataType tp) 
				(["__weak" | D.DefModWeak `elem` mods] ++ ["__unsafe_unretained" | D.isTpClass tp || D.isTpTrait tp] ) C.Nop
		con = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple name [], 
			C.cfunName = name ++ "Make", C.cfunPars = map toPar fields, C.cfunExps = 
				[C.Return $ C.ShortCast selfTp $ C.EArr $ map toEArr fields]
		}
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
			C.cfunExps = translate (env {envDataType = tp}) e}
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
				buildExp "type" D.Nop = C.Call (C.Ref "CNPType") "typeWith" [
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
			C.implFuns = [wrapFunImpl, initWrapFunImpl] ++ maybeToList compareImpl ++ copyImpls,
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
		compareFun = find (("compare" == ). D.defName) defs
		compareImpl = fmap (\d -> C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSInteger" []) "compare" [C.FunPar "to" selfWrapTp "to"]) [
			C.Return $ C.CCall (C.Ref $ structDefName name d) [C.Ref "_value", C.Dot (C.Ref "to") (C.Ref "value")]
			]) compareFun
		toEArr D.Def{D.defType = D.TPEArr n _, D.defName = nm}
			| n > 0 = C.EArr $ map ( C.Index (C.Ref nm) . C.IntConst) [0 .. n - 1]
		toEArr d = C.Ref . D.defName  $ d



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
stringFormatForType D.TPPointer{} = "%p"
stringFormatForType (D.TPEArr n tp)  = "[" ++ strs ", " (replicate n (stringFormatForType tp)) ++ "]"
stringFormatForType _ = "%@"

stringExpressionsForTp :: D.DataType -> C.Exp -> [C.Exp]
stringExpressionsForTp rtp ref = case rtp of
			D.TPClass D.TPMStruct _ scl -> [C.CCall (C.Ref $ structNameForCalling (D.classNameWithPrefix scl) ++ "Description") [ref]]
			D.TPClass D.TPMEnum _ _ -> [enumValue rtp ref]
			D.TPEArr n etp -> concatMap (\j -> stringExpressionsForTp etp $ C.Index ref (C.IntConst j)) [0..n - 1]
			D.TPNumber False 0 -> [C.ShortCast (C.TPSimple "long" []) ref]
			D.TPNumber True 0 -> [C.ShortCast (C.TPSimple "unsigned long" []) ref]
			_ -> [ref]
							

structDefName :: String -> D.Def -> String
structDefName sn D.Def{D.defName = dn, D.defPars = pars} = structNameForCalling sn ++ cap dn ++ (strs "" . map (cap . D.defName)) pars

structNameForCalling :: String -> String 
structNameForCalling (x1:x2:xs)
	| isUpper x1 && isUpper x2 = toLower x1 : structNameForCalling (x2:xs)
	| otherwise = x1:x2:xs
structNameForCalling x = x
{- Enum -}

enumValuesFun :: C.Fun
enumValuesFun = C.Fun C.ObjectFun (C.TPSimple "NSArray*" []) "values" []

genEnumInterface :: D.Class -> [C.FileStm]
genEnumInterface cl = [
	C.Enum{
		C.enumName = name ++ "R",
		C.enumItems = C.EnumItem (name ++ "_" ++ "Nil") (Just 0) : genEnumItems 1 (D.enumItems cl)
	}, C.Interface {
		C.interfaceName = name ,
		C.interfaceExtends = classExtends cl,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs',
		C.interfaceFuns = intefaceFuns defs' ++ [enumValuesFun],
		C.interfaceFields = []
	}, 
	C.CVar [C.CFunStatic] (C.TPArr (length $ D.enumItems cl) (name ++ "*")) (name ++ "_Values")
	] ++ map enumItemGetterFun (D.enumItems cl)
	where 
		name = D.classNameWithPrefix cl
		defs' = filter ((/= "values") . D.defName) $ D.classDefs cl
		enumItemGetterFun D.Def{D.defName = itemName} = C.CVar [C.CFunStatic] (C.TPSimple (name ++ "*") []) (name ++ "_" ++ itemName ++ "_Desc")
		genEnumItems :: Int -> [D.Def] -> [C.EnumItem]
		genEnumItems _ [] = []
		genEnumItems n (D.Def{D.defName = itemName}:xs) = (C.EnumItem (name ++ "_" ++ itemName) (Just n)):(genEnumItems (n + 1) xs)

genEnumImpl :: D.Class -> [C.FileStm]
genEnumImpl cl@D.Class {} = [
	C.Implementation {
		C.implName = clsName,
		C.implFields = (map (implField env) . filter needProperty) defs,
		C.implSynthesizes = (map (synthesize env) . filter needProperty) defs,
		C.implFuns = nub $ [implCreate cl constr, implInit env constr, initialize] ++ dealoc env 
			++ implFuns env defs ++ [valuesFun],
		C.implStaticFields = [] -- map stField items ++ [C.ImplField valuesVarName (C.TPSimple "NSArray*" []) [] C.Nop] 
	}]
	where
		clsName = D.classNameWithPrefix cl
		env = Env  cl 0 D.TPVoid False
		valuesVarName =  clsName ++ "_Values"
		items = D.enumItems cl
		defs = filter ((/= "values") . D.defName) $ D.classDefs cl
		constr = fromMaybe (error "No class constructor") (D.classConstructor cl)
		initialize = C.ImplFun (C.Fun C.ObjectFun voidTp "load" []) (
			((C.Stm $ C.Call C.Super "load" [] []) : map initItem items) ++ initArr 0 items)
		initArr _ [] = []
		initArr n (d:xs) =  C.Set Nothing (C.Index (C.Ref $ valuesVarName) (C.IntConst n)) (C.Ref $ enumItemDesc env d):initArr (n + 1) xs
		initItem :: D.Def -> C.Stm
		initItem d@D.Def{D.defBody = body} = C.Set Nothing (C.Ref $ enumItemDesc env d) $ retain $ tExp env body
		valuesFun = C.ImplFun enumValuesFun [C.Return $ (C.Arr $ map (C.Ref . (enumItemDesc env)) items)]
		

enumItemDesc :: Env -> D.Def -> String
enumItemDesc env D.Def{D.defName = name} = D.classNameWithPrefix (envClass env) ++ "_" ++ name ++ "_Desc"

enumNil :: D.Class -> C.Exp
enumNil cl = C.Ref $ D.classNameWithPrefix cl ++ "_Nil"

enumValue :: D.DataType -> C.Exp -> C.Exp
enumValue tp e = C.Index (enumValues tp) e
		
enumValues :: D.DataType -> C.Exp
enumValues tp = C.Ref $ (D.dataTypeClassNameWithPrefix tp) ++ "_Values"


nilForType :: D.DataType -> C.Exp
nilForType (D.TPGenericWrap _ tp) = nilForType tp
nilForType (D.TPClass D.TPMEnum _ cl) = enumNil cl
nilForType _ = C.Nil

-----------------------------------------------------------------------------------------------------------------------------------------
-- Imports 
-----------------------------------------------------------------------------------------------------------------------------------------
procImports :: D.Core -> D.File -> ([C.FileStm], [C.FileStm])
procImports core thisFile@D.File{D.fileClasses = classes} = (h, m)
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
		procDataType (D.TPOption _ cl) = procDataType cl
		procDataType tp 
			| isCore = retCl $ D.coreDataTypeClass core tp
			| otherwise = []
		procExp :: D.Exp -> [(D.Class, Bool)] 
		procExp = D.forExp f
			where
				f (D.Dot l _) = procDataType $ D.exprDataType l
				f (D.BoolOp _ l _) = procDataType $ D.exprDataType l
				f _ = []
		retCl :: D.Class -> [(D.Class, Bool)] 
		retCl D.ClassError{} = []
		retCl cl
			| needRetCl cl = [(cl, D.isStruct cl || D.isEnum cl)]
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
		isCore = D.isCoreFile thisFile
		filterFile :: D.File -> Bool
		filterFile f = f /= thisFile && (isCore || not (D.isCoreFile f))
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
showDataType (D.TPArr _ _) = C.TPSimple "NSArray*" []
showDataType (D.TPMap _ _)  = C.TPSimple "NSDictionary*" []
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
showDataType (D.TPPointer tp) = case D.unwrapGeneric tp of
	D.TPClass D.TPMGeneric _ _ -> C.TPSimple "void*" []
	t -> C.TPRef $ showDataType t
showDataType D.TPBool = C.TPSimple "BOOL" []
showDataType D.TPAny = idTp
showDataType D.TPAnyGeneric = idTp
showDataType D.TPNil = idTp
showDataType (D.TPClass D.TPMStruct _ c) = C.TPSimple (D.classNameWithPrefix c) []
showDataType tp@(D.TPClass D.TPMType _ _) = showDataType $ fromMaybe (error "Not found super type for type") $ D.superType tp
showDataType (D.TPClass D.TPMClass _ c) 
	| D.className c == "Object" = C.TPSimple "NSObject*" []
	| otherwise = C.TPSimple (D.classNameWithPrefix c ++ "*") []
showDataType (D.TPClass D.TPMEnum _ c) = C.TPSimple (D.classNameWithPrefix c ++ "R") []
showDataType (D.TPClass D.TPMTrait _ c) = C.TPSimple "id" [D.classNameWithPrefix c]
showDataType (D.TPClass{}) = idTp
showDataType (D.TPSelf _) = idTp
showDataType (D.TPTuple items) 
	| length items > 2 = C.TPSimple ("CNTuple" ++ show (length items) ++ "*") []
	| otherwise = C.TPSimple "CNTuple*" []
showDataType (D.TPOption _ (D.TPGenericWrap _ (D.TPClass D.TPMEnum _ c))) = C.TPSimple (D.classNameWithPrefix c ++ "R") []
showDataType (D.TPOption _ tp) = showDataType tp
showDataType (D.TPFun [] d) = C.TPBlock (showDataType d) []
showDataType (D.TPFun ss d) = C.TPBlock (showDataType d) (map showDataType ss)
showDataType (D.TPGenericWrap _ (D.TPClass D.TPMStruct _ _)) = idTp
showDataType (D.TPGenericWrap _ D.TPNumber{}) = idTp
showDataType (D.TPGenericWrap _ D.TPFloatNumber{}) = idTp
showDataType (D.TPGenericWrap _ D.TPBool) = idTp
showDataType (D.TPGenericWrap _ D.TPChar) = idTp
showDataType (D.TPGenericWrap _ D.TPVoid) = idTp
showDataType (D.TPGenericWrap _ (D.TPClass D.TPMEnum _ c)) = C.TPSimple (D.classNameWithPrefix c ++ "*") []
showDataType (D.TPGenericWrap _ c) = showDataType c
showDataType D.TPChar = C.TPSimple "unichar" []
showDataType tp = C.TPSimple (show tp) []

{- Exp -}
tPars :: Env -> D.Def -> [(D.Def, D.Exp)] -> [(String, C.Exp)]
tPars env _ = map (\(d, e) -> (D.defName d, maybeVal (D.exprDataType e, D.defType d) $ tExp env e))

tExpTo :: Env -> D.DataType -> D.Exp -> C.Exp 
tExpTo env tp e = maybeVal (D.exprDataType e, tp) (tExp env e)

castGeneric :: D.Exp -> C.Exp -> C.Exp
castGeneric dexp e = let 
	cst (D.TPOption _ tp) = cst (D.wrapGeneric tp)
	cst (D.TPGenericWrap _ (D.TPOption _ tp)) = cst (D.wrapGeneric tp)
	cst (D.TPGenericWrap _ c@(D.TPClass D.TPMClass _ _)) = C.Cast (showDataType c) e
	cst (D.TPGenericWrap _ c@(D.TPClass D.TPMTrait _ _)) = C.Cast (showDataType c) e
	cst (D.TPGenericWrap _ c@(D.TPClass D.TPMEnum _ _)) = C.Cast (showDataType c) e
	cst (D.TPGenericWrap _ c@D.TPTuple{}) = C.Cast (showDataType c) e
	cst (D.TPGenericWrap _ c@D.TPArr{}) = C.Cast (showDataType c) e
	cst (D.TPGenericWrap _ c@D.TPEArr{}) = C.Cast (showDataType c) e
	cst (D.TPGenericWrap _ c@D.TPMap{}) = C.Cast (showDataType c) e
	cst _ = e
	in cst $ D.exprDataType dexp

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

setSelf :: Env -> D.DataType -> [C.Stm] -> [C.Stm]
setSelf env tp stm = if hasWeakSelf False stm then [
		C.Var (C.tp $ (D.classNameWithPrefix $ envClass env) ++ "*") "_self" (C.Ref "_weakSelf") [],
		C.If (C.BoolOp NotEq (C.Ref "_self") C.Nil) stm (ret tp)] else stm
	where 
		ret D.TPVoid = []
		ret D.TPBool = [C.Return $ C.BoolConst False]
		ret D.TPNumber{} = [C.Return $ C.IntConst 0]
		ret D.TPFloatNumber{} = [C.Return $ C.FloatConst 0.0]
		ret _ = [C.Return C.Nil]
		

tExp :: Env -> D.Exp -> C.Exp
{- Optimizations -}
tExp env (D.Dot l (D.Call (D.Def{D.defName = "im"}) _ [] _)) 
	| isExp = tExp env l
	where isExp = case D.exprDataType l of
		D.TPClass _ _ D.Class{D.className = "MArray"} -> True
		_ -> False
-- sizeof(structure)
tExp _ (D.Dot (D.Dot (D.Call (D.Def{D.defType = D.TPObject _ cl}) _ [] _) 
		(D.Call (D.Def{D.defName = "type"}) _ [] _)) (D.Call (D.Def{D.defName = "size"}) _ [] _)) 
	= C.CCall (C.Ref "sizeof") [C.Ref $ D.classNameWithPrefix cl]


tExp _ (D.IntConst i) = C.IntConst i
tExp _ (D.StringConst i) = C.StringConst i
tExp _ (D.Nil) = C.Nil -- C.Call (C.Ref "NSNull") "null" [] []
tExp _ (D.BoolConst i) = C.BoolConst i
tExp _ (D.FloatConst i) = C.FloatConst i
tExp env (D.BoolOp t l D.Continue) = C.BoolOp t (tExp env l) (C.Ref "CNGo_Continue") 
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
		ltp = D.unwrapGeneric $ D.exprDataType l 
		rtp = D.unwrapGeneric $ D.exprDataType r
	in case ltp of
		D.TPString -> case rtp of
			D.TPString -> C.Call l' "stringByAppending" [("string", r')] []
			_ -> C.Call l' "stringByAppending" [("format",  C.StringConst $ stringFormatForType rtp)] [maybeVal (D.exprDataType r, rtp) r']
		_ -> C.MathOp t (tExpTo env ltp l) (tExpTo env ltp r)
tExp env (D.PlusPlus e) = C.PlusPlus (tExp env e)
tExp env (D.MinusMinus e) = C.MinusMinus (tExp env e)


{- Dot -}
tExp env (D.NullDot l (D.Call dd@D.Def{D.defMods = mods} _ pars _) _) 
	| not (null pars) && D.DefModApplyLambda `elem` mods =
		let 
			tp = D.exprDataType l
		in C.ExpBraces [
			C.Var (showDataType tp) "__nd" (castGeneric l $ tExp env l) [],
			C.Stm $ C.InlineIf (C.BoolOp Eq (C.Ref "__nd") C.Nil) C.Nil (C.CCall (C.Ref "__nd") ((map snd . tPars env dd) pars))
		]
tExp env (D.NullDot l r _) = tExpToType env (D.wrapGeneric $ D.exprDataType r) (D.Dot l r)
tExp env (D.Dot (D.Self (D.TPClass D.TPMStruct _ c)) (D.Call d@D.Def {D.defName = name, D.defMods = mods} _ pars _)) 
	| D.DefModField `elem` mods = C.Dot (C.Ref "self") (C.Ref name)
	| otherwise = C.CCall (C.Ref $ structDefName (D.classNameWithPrefix c) d) (C.Ref "self" : (map snd . tPars env d) pars)
tExp env (D.Dot (D.Self stp) (D.Call d@D.Def{D.defMods = mods, D.defName = name} _ pars _)) 
	| D.DefModField `elem` mods && null pars && D.DefModSuper `notElem` mods && (not (envWeakSelf env) || D.DefModStatic `elem` mods) = 
		C.Ref $ fieldName env d
	| D.DefModField `elem` mods && D.DefModSuper `notElem` mods && not ((envWeakSelf env) || D.DefModStatic `elem` mods) = 
		C.CCall (C.Ref $ fieldName env d) ((map snd . tPars env d) pars)
	| D.DefModStatic `elem` mods = C.Call (C.Ref $ D.classNameWithPrefix $ D.tpClass stp) name (tPars env d pars) []
	| D.DefModField `elem` mods && null pars = selfGetField env name
	| otherwise = C.Call (selfCall env) name (tPars env d pars) [] 
tExp _ (D.Dot (D.Call D.Def{D.defType = tp} _ [] _) (D.Call D.Def{D.defName = enumName, D.defMods = mods} _ [] _))
	| D.DefModEnumItem `elem` mods = C.Ref $ D.dataTypeClassNameWithPrefix tp ++ "_" ++ enumName
tExp env (D.Dot l (D.Call dd@D.Def{D.defName = name, D.defMods = mods} _ pars _))
	| D.DefModStatic `elem` mods && isStubObject = 
		if D.DefModField `elem` mods then C.Ref name
		else C.CCall (C.Ref $ name) ((map snd . tPars env dd) pars)
	| D.DefModApplyLambda `elem` mods = C.CCall (castGeneric l $ tExp env l) ((map snd . tPars env dd) pars) 
	| D.DefModEnum `elem` mods && name == "ordinal" = tExpTo env ltp l
	| D.DefModEnum `elem` mods && D.DefModField `elem` mods = C.Dot enumLeft (C.Ref name)
	| D.DefModEnum `elem` mods = stdCall enumLeft
	| D.DefModField `elem` mods && null pars && 
		not (D.DefModStruct `elem` mods && D.DefModStatic `elem` mods) = 
			C.Dot (castGeneric l $ tExpTo env ltp l) (C.Ref name)
	| D.DefModField `elem` mods && 
		not (D.DefModStruct `elem` mods && D.DefModStatic `elem` mods) = 
			C.Dot (castGeneric l $ tExpTo env ltp l) $ C.CCall (C.Ref name) ((map snd . tPars env dd) pars)
	| D.DefModConstructor `elem` mods = callConstructor env dd pars
	| D.DefModStruct `elem` mods = case ltp of
		(D.TPClass D.TPMStruct _ c) -> structCall (D.classNameWithPrefix c) (castGeneric l $ tExpTo env ltp l)
		(D.TPObject D.TPMStruct c) -> C.CCall (C.Ref $ structDefName (D.classNameWithPrefix c) dd) ((map snd . tPars env dd) pars)
		D.TPPointer{} -> structCall "cnPointer" (castGeneric l $ tExpTo env ltp l)
		tp -> structCall (show tp) (castGeneric l $ tExpTo env ltp l)
	| otherwise = stdCall (castGeneric l $ tExp env l)
	where
		enumLeft = enumValue ltp (tExpTo env ltp l)
		stdCall ll = C.Call ll (funName dd) (tPars env dd pars) []
		structCall c self = C.CCall (C.Ref $ structDefName c dd) (self : (map snd . tPars env dd) pars)
		ltp = D.unwrapGeneric $ D.exprDataType l
		isStubObject = case ltp of
		 	D.TPObject _  cl -> D.ClassModStub `elem` D.classMods cl && D.ClassModObject `elem` D.classMods cl
		 	_ -> False
tExp env (D.Dot l (D.Is dtp)) = case D.unwrapGeneric dtp of
	D.TPClass D.TPMGeneric _ _ -> C.BoolConst False
	D.TPClass D.TPMTrait _ _ -> C.Call (castGeneric l $ tExp env l) "conformsTo" 
		[("protocol", C.ProtocolRef $ C.Ref $ D.dataTypeClassNameWithPrefix dtp)] []
	_ -> C.Call (castGeneric l $ tExp env l) "isKindOf" 
		[("class", C.Call (C.Ref $ D.dataTypeClassNameWithPrefix dtp) "class" [] [])] []
tExp env (D.Dot l (D.As dtp)) = case D.unwrapGeneric dtp of
	D.TPClass D.TPMTrait _ _ -> C.Call (C.Ref "CNObject") "asKindOf" 
		[("protocol", C.ProtocolRef $ C.Ref $ D.dataTypeClassNameWithPrefix dtp), ("object", castGeneric l $ tExp env l)] []
	_ -> C.Call (C.Ref "CNObject") "asKindOf" 
		[("class", C.Call (C.Ref $ D.dataTypeClassNameWithPrefix dtp) "class" [] []), ("object", castGeneric l $ tExp env l)] []
tExp env (D.Dot l (D.CastDot dtp)) = tExp env (D.Cast dtp l)
tExp env (D.Dot l (D.Cast dtp c)) = tExp env (D.Cast dtp (D.Dot l c))
tExp env (D.Dot l (D.LambdaCall c)) = tExp env (D.LambdaCall $ D.Dot l c)

tExp env (D.Arrow l (D.Call D.Def{D.defName = name} _ [] _)) = C.Arrow (tExp env l) (C.Ref name)
tExp env (D.Arrow l r) = C.Arrow (tExp env l) (tExp env r)

tExp _ (D.Self (D.TPClass D.TPMEnum _ _)) = C.Arrow C.Self (C.Ref "_ordinal")
tExp env (D.Self _) = selfCall env
tExp _ (D.Super _) = C.Super
tExp env (D.LambdaCall e) = let 
		tp = D.exprDataType e
		e' = tExp env e 
	in C.CCall (if D.isGenericWrap tp then C.Cast (showDataType tp) e' else e') []
tExp env (D.Call d@D.Def{D.defName = name, D.defMods = mods, D.defType = tp} _ pars _)
	| D.DefModField `elem` mods = C.Ref $ fieldName env d
	| D.DefModEnumItem `elem` mods = C.Ref $ fieldName env d
	| D.DefModLocal `elem` mods && null pars = C.Ref name
	| D.DefModConstructor `elem` mods = callConstructor env d pars
	| D.DefModGlobalVal `elem` mods = C.Ref name
	| D.DefModObject `elem` mods = C.Ref $ D.dataTypeClassNameWithPrefix tp
	| otherwise = C.CCall (C.Ref name) (map snd . tPars env d $ pars)
tExp env (D.If cond t f) = C.InlineIf (tExpTo env D.TPBool cond) (tExp env t) (tExp env f)
tExp env (D.Index e i) = case D.exprDataType e of
	D.TPObject D.TPMEnum cl -> C.Cast (showDataType (D.TPClass D.TPMEnum [] cl)) $ C.MathOp Plus (tExp env i) (C.IntConst 1)
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
		stm = translate env{envDataType = rtp} e
	in
	C.Lambda (map par' pars) (unwrapPars ++ setSelf env rtp stm) (showDataType rtp)
tExp env (D.Weak expr) = tExp env{envWeakSelf = True} expr
tExp env (D.Arr exps) = C.Arr $ map (tExpToType env D.tpGeneric) exps
tExp env (D.Map exps) = C.Map $ map (tExpToType env D.tpGeneric *** tExpToType env D.tpGeneric) exps
tExp env (D.Tuple exps) = C.CCall (C.Ref $ "tuple" ++ if length exps == 2 then "" else show (length exps) ) $ map (tExpToType env D.tpGeneric) exps
tExp env (D.Some _ e) = tExpTo env (D.option True $ D.exprDataType e) e
tExp _ (D.None tp) = nilForType tp
tExp env (D.Not e) = C.Not (tExpTo env D.TPBool e)
tExp env (D.Negative e) = C.Negative (tExp env e)
tExp env (D.Cast dtp e) = let 
		stp = D.exprDataType e
		stp' = D.unwrapGeneric stp
		toString format = C.Call (C.Ref "NSString") "stringWith" [("format", C.StringConst format)] (stringExpressionsForTp stp $ tExpTo env stp e)
		cast = C.Cast (showDataType dtp) e'
		e' = (tExpTo env stp' e)
		{-voidRefStructCast = C.CCall (C.Ref "voidRef") [e']-}
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
		{-(D.TPNumber{}, D.TPVoidRef) -> voidRefStructCast
		(D.TPFloatNumber{}, D.TPVoidRef) -> voidRefStructCast
		(D.TPClass D.TPMStruct _ _, D.TPVoidRef) -> voidRefStructCast
		(D.TPVoidRef, _) -> case showDataType dtp of 
			C.TPSimple t a -> C.RefUp $ C.Cast (C.TPSimple (t ++ "*") a)  e'
		(D.TPArr _ etp, D.TPVoidRef) ->
			case e of
				D.Arr exps -> ear etp exps
				_ -> cast-}
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

tExp _ D.Null{} = C.Null
tExp _ (D.Braces []) = C.Nop
tExp env (D.Braces [x]) = tExp env x
tExp env (D.Braces stms) = C.ExpBraces (concatMap (translate env) (init stms) ++ [C.Stm (tExp env $ last stms)])
tExp env ee@(D.NonOpt ch e _) = let 
		tp = D.unwrapGeneric $ D.exprDataType ee
		check = ch && case tp of
			D.TPVoid -> False
			D.TPClass D.TPMGeneric _ _ -> False
			_ -> True
	in 
		if isElementary tp then 
			if check then maybeVal (D.exprDataType e, tp) $ C.CCall (C.Ref "nonnil") [tExp env e]
			else tExpTo env tp e
		else
			if check then C.Cast (showDataType tp) $ C.CCall (C.Ref "nonnil") [tExp env e]
			else C.Cast (showDataType tp) $ tExpTo env tp e
tExp env (D.Return _ e) = tExp env e
tExp env (D.Throw e) = C.ExpBraces [C.Throw $ tExp env e]
tExp _ D.NPE = C.ExpBraces [C.Throw $ C.StringConst "Not null"]
tExp env (D.Deferencing e) = C.Deferencing (tExp env e)
tExp _ x = C.Error $ "No tExp for " ++ show x


isElementary :: D.DataType -> Bool
isElementary (D.TPClass D.TPMStruct _ _) = True
isElementary D.TPNumber{} = True
isElementary D.TPChar{} = True
isElementary D.TPFloatNumber{} = True
isElementary D.TPBool{} = True
isElementary _ = False

tExpToType :: Env -> D.DataType -> D.Exp -> C.Exp
tExpToType env tp e = maybeVal (D.exprDataType e, tp) (tExp env e)

translate :: Env -> D.Exp -> [C.Stm]
translate env e = case tStm env e of
	[C.Braces r] -> r
	r -> r

processGo :: D.Exp -> D.Exp -> D.Exp -> D.Exp
processGo cn brk e = D.mapExp procGo e 
	where
		procGo (D.Return _ (D.Dot (D.Call D.Def{D.defName = "Go"} _ [] _) (D.Call D.Def{D.defName = "Continue"} _ [] _))) = Just cn
		procGo (D.Return _ (D.Dot (D.Call D.Def{D.defName = "Go"} _ [] _) (D.Call D.Def{D.defName = "Break"} _ [] _))) = Just brk 
		procGo (D.Return _ ee) = Just $ D.If (D.BoolOp Eq ee D.Continue) cn brk
		procGo _ = Nothing

tStm :: Env -> D.Exp -> [C.Stm]
tStm _ (D.Nop) = []

tStm _ (D.Braces []) = []
tStm v (D.Braces [x]) = translate v x
tStm v (D.Braces xs) = [C.Braces $ concatMap (tStm v) xs]

tStm v (D.If cond t (D.None _)) = [C.If (tExpTo v D.TPBool cond) (translate v t) []]
tStm v (D.If cond t f) = [C.If (tExpTo v D.TPBool cond) (translate v t) (translate v f)]
tStm v (D.While cond t) = [C.While (tExpTo v D.TPBool cond) (translate v t)]
tStm v (D.Synchronized ref b) = [C.Synchronized (tExp v ref) (translate v b)]
tStm v (D.Do cond t) = [C.Do (tExpTo v D.TPBool cond) (translate v t)]

tStm env (D.Set Nothing sl x@(D.Dot l (D.Call (D.Def{D.defName = dn}) _ [(_, D.Lambda [(cycleVar, cycleTp)] cycleBody _)] _))) 
	| dn == "go" =
		case D.exprDataType l of
			D.TPArr _ _ -> forin
			D.TPClass _ _ D.Class{D.className = "MArray"} -> forin 
			D.TPClass _ _ D.Class{D.className = "Array"} -> forin 
			D.TPClass _ _ D.Class{D.className = "ImArray"} -> forin 
			_ -> [C.Stm $ tExp env x]
	where
		sl' = tExp env sl
		forin = [C.Set Nothing sl' (C.Ref "CNGo_Continue"),
			C.ForIn (showDataType cycleTp) cycleVar (tExp env l) (translate env ( processGo D.Continue (D.Braces [D.Set Nothing sl D.Break, D.Break]) cycleBody) )]
		
tStm env x@(D.Dot l (D.Call (D.Def{D.defName = dn}) _ [(_, D.Lambda [(cycleVar, cycleTp)] cycleBody _)] _)) 
	| dn == "for" || dn == "go" =
		case D.exprDataType l of
			D.TPArr _ _ -> forin
			D.TPClass _ _ D.Class{D.className = "MArray"} -> forin 
			D.TPClass _ _ D.Class{D.className = "Array"} -> forin 
			D.TPClass _ _ D.Class{D.className = "ImArray"} -> forin 
			_ -> [C.Stm $ tExp env x]
	where
		forin = [C.ForIn (showDataType cycleTp) cycleVar (tExp env l) (translate env ((if dn == "go" then processGo D.Continue D.Break else id) cycleBody) )]
tStm env (D.Set Nothing l D.Break) = [C.Set Nothing (tExp env l) (C.Ref "CNGo_Break")]

tStm env (D.Set tp l r) = let 
	l' = tExp env l
	r' = tExp env r
	ltp = D.exprDataType l
	rtp = D.exprDataType r
	in [C.Set tp l' (maybeVal (rtp, ltp) r')]
tStm Env{envDataType = D.TPVoid} (D.Return True _) = [C.Return C.Nop]
tStm env@Env{envDataType = D.TPVoid} (D.Return _ e) = [C.Stm $ tExp env{envCStruct = 0} e]
tStm env@Env{envDataType = tp} (D.Return _ e) = [C.Return $ tExpToType env{envCStruct = 0} tp e]
tStm env (D.Val separate D.Def{D.defName = name, D.defType = tp, D.defBody = e, D.defMods = mods}) = 
	[C.Var (showDataType tp) name (if separate then C.Nop else tExpToType env tp e)
		 (["__block" | D.DefModChangedInLambda `elem` mods] ++ ["__weak" | D.DefModWeak `elem` mods] ++ ["volatile" | D.DefModVolatile `elem` mods] )]
	++ (if separate then tStm env e else [])
tStm env (D.Throw e) = [C.Throw $ tExp env e]
tStm _ D.Break = [C.Break]
tStm _ D.Continue = [C.Continue]
tStm env (D.Weak expr) = tStm env{envWeakSelf = True} expr
tStm env (D.Try e f) = [C.Try (translate env e) (translate env f)]
tStm env (D.NullDot l (D.Call dd@D.Def{D.defMods = mods} _ pars _) _) 
	| not (null pars) && D.DefModApplyLambda `elem` mods =
		let 
			tp = D.exprDataType l
		in [C.Braces [
			C.Var (showDataType tp) "__nd" (tExp env l) [],
			C.If (C.BoolOp NotEq (C.Ref "__nd") C.Nil) [C.Stm $ C.CCall (C.Ref "__nd") ((map snd . tPars env dd) pars)] []
		]]

tStm env x = [C.Stm $ tExp env x]

equals :: Bool -> (D.DataType, C.Exp) -> (D.DataType, C.Exp) -> C.Exp
equals d (D.TPGenericWrap _ t@(D.TPOption{}), e) r = equals d (t, e) r
equals d l (D.TPGenericWrap _ t@(D.TPOption{}), e) = equals d l (t, e)
equals d (D.TPOption _ (D.TPGenericWrap _ t@(D.TPClass D.TPMEnum _ _)), e) r = equals d (t, e) r
equals d l (D.TPOption _ (D.TPGenericWrap _ t@(D.TPClass D.TPMEnum _ _)), e) = equals d l (t, e)

equals False (D.TPClass D.TPMEnum _ _, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPNumber{}, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPChar, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPBool, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPNil, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (_, e1) (_, C.Nil) = C.BoolOp NotEq e1 C.Nil
equals False (_, C.Nil) (_, e2) = C.BoolOp NotEq C.Nil e2
equals False s1@(_, _) s2@(_, _) = C.Not $ equals True s1 s2

equals True (_, e1) (_, C.Nil) = C.BoolOp Eq e1 C.Nil
equals True (_, C.Nil) (_, e2) = C.BoolOp Eq C.Nil e2
equals True (D.TPClass D.TPMEnum _ _, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (dtp@(D.TPClass D.TPMStruct _ c), e1) (stp, e2) = C.CCall (C.Ref (structNameForCalling (D.classNameWithPrefix c) ++ "IsEqualTo")) [e1, maybeVal (stp, dtp) e2]
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

data MaybeValTP = TPGen D.DataType | TPNum | TPStruct | TPNoMatter | TPBool | TPFloat | TPEnum
maybeVal :: (D.DataType, D.DataType) -> C.Exp -> C.Exp
maybeVal (stp, dtp) e = let 
	tp t@D.TPOption{} = TPGen t
	tp (D.TPGenericWrap _ t@D.TPOption{}) = TPGen t
	tp t@D.TPGenericWrap{} = TPGen t
	tp t@(D.TPClass D.TPMGeneric _ _) = TPGen t
	tp (D.TPClass D.TPMStruct _ _) = TPStruct
	tp (D.TPClass D.TPMEnum _ _) = TPEnum
	tp D.TPNumber{} = TPNum
	tp D.TPChar{} = TPNum
	tp D.TPFloatNumber{} = TPFloat
	tp D.TPBool{} = TPBool
	tp _ = TPNoMatter
	fnm = ("num" ++ ) . dataTypeSuffix
	in case (tp stp, tp dtp) of
		(TPStruct, TPGen _) -> C.CCall (C.Ref "wrap") [C.Ref $ D.classNameWithPrefix $ D.tpClass stp, e]
		(TPGen _, TPStruct) -> C.CCall (C.Ref "uwrap") [C.Ref $ D.classNameWithPrefix $ D.tpClass dtp, e]
		(TPNum, TPGen _) -> case e of
			C.IntConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref $ fnm stp) [e]
		(TPGen _, TPNum) -> C.CCall (C.Ref $ "u" ++ fnm dtp) [e]
		(TPFloat, TPGen _) -> case e of
			C.FloatConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref $ fnm stp) [e]
		(TPGen _, TPFloat) -> C.CCall (C.Ref $ "u" ++ fnm dtp) [e]
		(TPBool, TPGen _) -> case e of
			C.BoolConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref $ fnm stp) [e]
		(TPGen _, TPBool) -> C.CCall (C.Ref $ "u" ++ fnm dtp) [e]
		(TPEnum, TPGen D.TPOption{}) -> e
		(TPGen D.TPOption{}, TPEnum) -> e
		(TPEnum, TPGen _) -> enumValue stp e
		(TPGen _, TPEnum) -> C.Cast (showDataType $ D.unwrapGeneric stp) $ C.Call e "ordinal" [] []
		_ -> e
