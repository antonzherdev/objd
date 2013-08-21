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

toObjC :: D.File -> ([C.FileStm], [C.FileStm])
toObjC f@D.File{D.fileName = name, D.fileClasses = classes, D.fileCImports = cImports} = 
	let 
		isClass c = D.isRealClass c && not (D.isStruct c) && not (D.isTrait c) && not (D.isEnum c)
		cls = filter isClass classes
		isStruct c = D.isRealClass c && D.isStruct c
		structs = filter isStruct classes
		enums = filter D.isEnum classes
		isTrait c = D.isRealClass c && D.isTrait c

		cImports' = map cImport' cImports
		cImport' (D.CImportLib n) = C.ImportLib n
		cImport' (D.CImportUser n) = C.Import n
		dImports' = procImports f
		
		
		h = (if D.isCoreFile f then [C.ImportLib "Foundation/Foundation.h"] else [ C.Import "objd.h"] )
			++ fst dImports' 
			++ [C.EmptyLine] 
			++ map classDecl (cls ++ enums) 
			++ map (C.ProtocolDecl . D.className) (filter D.isTrait classes) 
			++ map structDecl structs
			++ [C.EmptyLine] 
			++ concatMap (fst . gen) classes
		
		structDecl c = C.TypeDefStruct (D.className c) (D.className c)
		classDecl c = C.ClassDecl $ D.className c

		m = let
				 impls = concatMap (snd . gen) classes
			in if null impls then []
			else [C.Import (name ++ ".h") ] ++ cImports' ++ [ C.EmptyLine] ++ snd dImports' ++ impls
		gen c
			| isClass c = ([stmToInterface c], [stmToImpl c])
			| isStruct c = genStruct c
			| D.isEnum c = (genEnumInterface c, genEnumImpl c)
			| isTrait c = ([genProtocol c], [])
			| otherwise = ([], [])
	in (h, m)


{- Interface -}

stmToInterface :: D.Class -> C.FileStm
stmToInterface (cl@D.Class {D.className = name, D.classDefs = defs}) =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = classExtends cl,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs,
		C.interfaceFuns = [createFun name constr, initFun constr]
			++ intefaceFuns defs ++ staticGetters
	}
	where 
		constr = fromMaybe (error "No class constructor") (D.classConstructor cl)
		staticGetters = (map staticGetterFun .filter (\f -> 
			(D.DefModPrivate `notElem` D.defMods f) && D.isField f && D.isStatic f)) defs
		
classExtends :: D.Class -> C.Extends
classExtends cl = addTraits $ maybe (C.Extends "NSObject" []) ext (D.extendsClass $ D.classExtends cl)
	where
		ext (D.ExtendsClass (ccl, _) _)
			| D.ClassModTrait `elem` D.classMods ccl = C.Extends "NSObject" [D.className ccl]
			| D.className ccl == "ODObject" = C.Extends "NSObject" []
			| otherwise = C.Extends (D.className ccl) []
		addTraits (C.Extends cls protocols) = C.Extends cls $ protocols ++ map (D.className . fst) (D.extendsTraits $ D.classExtends cl)


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
		mutModes D.TPArr{} = [C.ReadOnly]
		mutModes (D.TPClass D.TPMClass _ _) = [C.Retain | D.DefModWeak `notElem` mods]
		mutModes (D.TPClass D.TPMEnum _ _) = [C.Retain | D.DefModWeak `notElem` mods]
		mutModes _ = []
		
idTp :: C.DataType
idTp = C.TPSimple "id" []
voidTp :: C.DataType
voidTp = C.TPSimple "void" []

initFun :: D.Def -> C.Fun
initFun D.Def{D.defPars = []} = C.Fun C.InstanceFun idTp "init" []
initFun D.Def{D.defPars = decls} = C.Fun C.InstanceFun idTp "initWith" (map funPar decls)

funPar :: D.Def -> C.FunPar
funPar D.Def {D.defName = name, D.defType = dataType} = C.FunPar name (showDataType dataType) name

createFun :: String -> D.Def -> C.Fun
createFun clsName D.Def{D.defPars = []} = C.Fun C.ObjectFun idTp (createFunName clsName) []
createFun clsName D.Def{D.defPars = decls} = C.Fun C.ObjectFun idTp (createFunName clsName ++ "With") (map funPar decls)

createFunName :: String -> String
createFunName (x1:x2:xs)
	| isUpper x1 && isUpper x2  =  createFunName(x2 : xs)
	| isUpper x1 = toLower x1 : x2 : xs
	| otherwise = x1 : x2 : xs


intefaceFuns :: [D.Def] -> [C.Fun]
intefaceFuns = map stm2Fun . filter (\v -> D.DefModPrivate `notElem` D.defMods v && D.isDef v)

stm2Fun :: D.Def -> C.Fun
stm2Fun D.Def{D.defName = name, D.defPars = pars, D.defType = tp, D.defMods = mods} =
	C.Fun {
		C.funType = if D.DefModStatic `elem` mods then C.ObjectFun else C.InstanceFun, 
		C.funReturnType = showDataType tp, 
		C.funName = name, 
		C.funPars = map par pars}
	where
		par (D.Def{D.defName = nm, D.defType = ttp}) = C.FunPar nm (showDataType ttp) nm

genProtocol :: D.Class -> C.FileStm
genProtocol (D.Class {D.className = name, D.classDefs = defs, D.classExtends = exts}) =
	C.Protocol {
		C.interfaceName = name,
		C.interfaceFuns = intefaceFuns defs,
		C.interfaceExtends = C.Extends ((cn . D.className . D.extendsClassClass . fromJust . D.extendsClass) exts) []
	}
	where
		cn n = if n == "ODObject" then "NSObject" else n


{- Implementation -}

stmToImpl :: D.Class -> C.FileStm
stmToImpl cl@D.Class {D.className = clsName, D.classDefs = clDefs} =
	C.Implementation {
		C.implName = clsName,
		C.implFields = map (implField False) implFields,
		C.implSynthesizes = (map synthesize . filter needProperty) implFields,
		C.implFuns = [implCreate cl constr, implInit cl constr] ++ maybeToList (implInitialize cl) ++ dealoc cl 
			++ implFuns defs ++ staticGetters ++ copyImpls ++ (if equalsIsPosible cl then [equal, hash] else []) ++ [description],
		C.implStaticFields = map (implField True) staticFields
	}
	where
		defs :: [D.Def]
		defs = nub $ clDefs ++ traitDefs cl
		
		traitDefs :: D.Class -> [D.Def]
		traitDefs cll =
			(if D.isTrait cll then filter ( (D.DefModAbstract `notElem`). D.defMods) (D.classDefs cll) else []) ++
			concatMap (traitDefs . fst) ((D.extendsRefs . D.classExtends) cll)
		constr = fromMaybe (error "No class constructor") (D.classConstructor cl)
		implFields = filter needField defs
		needField f = D.isField f && not (D.isStatic f)
		isStaticField f = D.isStatic f && D.isField f
		staticFields = filter isStaticField defs
		staticGetters = (map staticGetter . filter((D.DefModPrivate `notElem`) . D.defMods)) staticFields
		staticGetter f@D.Def{D.defName = name} = C.ImplFun (staticGetterFun f) [C.Return $ C.Ref $ '_' : name]

		equal = C.ImplFun equalFun (equalPrelude clsName (not (null equalFields)) ++ equalsFun C.Self (C.Ref "o") equalFields)
		equalFields = maybe [] (D.defPars) $ D.classConstructor cl
		
 		hash = C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSUInteger" []) "hash" []) (hashFun equalFields)
 		description = C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSString*" []) "description" []) 
 			(descriptionFun descStart equalFields)
 		descStart = C.Call (C.Ref "NSMutableString") "stringWith" [("format", C.StringConst "<%@: ")] 
			[C.CCall (C.Ref "NSStringFromClass") [C.Call C.Self "class" [] []]]

equalPrelude :: String -> Bool -> [C.Stm]
equalPrelude clsName o = [
			C.If (C.BoolOp Eq C.Self (C.Ref "other")) [C.Return $ C.BoolConst True] [],
			C.If (C.BoolOp Or (C.Not $ C.Ref "other") (C.Not equalClass)) [C.Return $ C.BoolConst False] []
			] ++ [C.Var selfTp "o" (C.Cast selfTp (C.Ref "other")) [] | o]
	where
		equalClass = C.Call (C.Call C.Self "class" [] []) "isEqual" [("", C.Call (C.Ref "other") "class" [] [])] []
		selfTp = (C.TPSimple (clsName ++ "*") []) 

equalFun :: C.Fun
equalFun = C.Fun C.InstanceFun (C.TPSimple "BOOL" []) "isEqual" [(C.FunPar "" (C.TPSimple "id" []) "other")]

equalsIsPosible :: D.Class -> Bool
equalsIsPosible D.Class {D.classDefs = defs} = 
	(null $ filter ( (D.DefModMutable `elem` ). D.defMods) defs)
	|| (not $ null $ filter ( isVal . D.defMods) defs)
	where
		isVal mods = (D.DefModField `elem` mods) && (D.DefModMutable `notElem` mods)

copyImpls :: [C.ImplFun]
copyImpls = [C.ImplFun (C.Fun C.InstanceFun idTp "copyWith" [C.FunPar "zone" (C.TPSimple "NSZone*" []) "zone"]) [C.Return C.Self]]

synthesize :: D.Def -> C.ImplSynthesize
synthesize D.Def{D.defName = x} = C.ImplSynthesize x ('_' : x)
implField :: Bool -> D.Def -> C.ImplField
implField static D.Def{D.defName = x, D.defType = tp, D.defMods = mods, D.defBody = e} = 
	C.ImplField ('_' : x) (showDataType tp) ["__weak" | D.DefModWeak `elem` mods] $ 
		if static && D.isConst e then tExpTo newEnv{envCStruct = True} tp e else C.Nop

implCreate :: D.Class -> D.Def -> C.ImplFun
implCreate cl constr@D.Def{D.defPars = constrPars} = let 
		clsName = D.className cl
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

dealoc :: D.Class -> [C.ImplFun]
dealoc cl 
	| arc = []
	| otherwise = [C.ImplFun (C.Fun C.InstanceFun voidTp "dealloc" []) $
		 mapMaybe releaseField (D.classFields cl) ++ [C.Stm $C.Call C.Super "dealloc" [] []]]
	where 
		releaseField D.Def{D.defName = name, D.defType = D.TPClass{}} = Just $ C.Stm $ C.Call (C.Ref $ '_' : name) "release" [] []
		releaseField _ = Nothing

implInitialize :: D.Class -> Maybe C.ImplFun 		
implInitialize cl = let 
	fields = filter hasInitialize (D.classFields cl)
	hasInitialize D.Def{D.defBody = D.Nop} = False
	hasInitialize d@D.Def{D.defBody = b} = not (D.isConst b) && D.isField d && D.isStatic d
	in case fields of
		[] -> Nothing
		_ -> Just $ C.ImplFun (C.Fun C.ObjectFun voidTp "initialize" []) (
			((C.Stm $ C.Call C.Super "initialize" [] []) : map implInitField fields))



implInit :: D.Class -> D.Def -> C.ImplFun
implInit cl constr@D.Def{D.defPars = constrPars}  = C.ImplFun (initFun constr) (
			[C.Set Nothing C.Self (superInit $ D.extendsClass $ D.classExtends cl)]
			++ implInitFields (filter hasField constrPars) (filter hasInit (D.classDefs cl))
			++ [C.Stm C.Nop, C.Return C.Self]
			)
	where
		hasInit D.Def{D.defBody = D.Nop} = False
		hasInit d = D.isField d && not (D.isStatic d)

		hasField f= any ((D.defName f == ) . D.defName) (D.classDefs cl)

		superInit Nothing = C.Call C.Super "init" [] []
		superInit (Just (D.ExtendsClass _ [])) = C.Call C.Super "init" [] []
		superInit (Just (D.ExtendsClass _ pars)) = C.Call C.Super "initWith" (map (D.defName *** tExp newEnv) pars) []

		implInitFields :: [D.Def] -> [D.Def] -> [C.Stm]
		implInitFields [] [] = []
		implInitFields co fields = [C.If C.Self (map implConstrField co ++ map implInitField fields) []]
		implConstrField D.Def{D.defName = name, D.defType = tp} = C.Set Nothing (C.Ref $ '_' : name) (implRight tp) 
			where
				implRight D.TPClass{} = retain $ C.Ref name
				implRight _ = C.Ref name

implInitField :: D.Def -> C.Stm
implInitField D.Def{D.defName = name, D.defBody = def, D.defType = tp} = C.Set Nothing (C.Ref $ '_' : name) (tExpTo newEnv tp def)
		
implFuns :: [D.Def] -> [C.ImplFun]
implFuns = map stm2ImplFun . filter D.isDef
	where
		stm2ImplFun def@D.Def {D.defName = name, D.defBody = db, D.defMods = mods, D.defType = tp}
			| D.DefModAbstract `elem` mods = C.ImplFun (stm2Fun def) [C.Throw $ C.StringConst $ "Method " ++ name++ " is abstract"]
			| otherwise = C.ImplFun (stm2Fun def) (tStm tp [] db)
		
		
{- Struct -}
genStruct :: D.Class -> ([C.FileStm], [C.FileStm])
genStruct D.Class {D.className = name, D.classDefs = defs} = 
	([C.Struct name fields', con, eq, hash, description] ++ defs' ++ [wrapClass, C.EmptyLine], 
		(map defImpl' . filter D.isDef) defs ++ [wrapImpl, C.EmptyLine])
	where
		fields = filter D.isField defs
		fields' = map toField fields
		toField D.Def{D.defName = n, D.defType = tp, D.defMods = mods} = C.ImplField n (showDataType tp) ["__weak" | D.DefModWeak `elem` mods] C.Nop
		con = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple name [], 
			C.cfunName = name ++ "Make", C.cfunPars = map toPar fields, C.cfunExps = 
				[C.Var (C.TPSimple name []) "ret" C.Nop []] ++
				map toSet fields ++
				[C.Return $ C.Ref "ret"]
		}
		eq = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple "BOOL" [], C.cfunName = name ++ "Eq", 
			C.cfunPars = [C.CFunPar (C.TPSimple name []) "s1", C.CFunPar (C.TPSimple name []) "s2"], 
			C.cfunExps = equalsFun (C.Ref "s1") (C.Ref "s2") fields
		}
		hash = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple "NSUInteger" [], C.cfunName = name ++ "Hash", 
			C.cfunPars = [C.CFunPar (C.TPSimple name []) "self"], 
			C.cfunExps = hashFun fields
		}
		description = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple "NSString*" [], C.cfunName = name ++ "Description", 
			C.cfunPars = [C.CFunPar (C.TPSimple name []) "self"], 
			C.cfunExps = descriptionFun descStart fields
		}
		descStart = C.Call (C.Ref "NSMutableString") "stringWith" [("string", C.StringConst $ "<" ++ name ++ ": ")] []
		toPar D.Def{D.defName = n, D.defType = tp}= C.CFunPar (showDataType tp) n
		toSet D.Def{D.defName = n, D.defType = tp@D.TPEArr{}} = C.Stm $ eArraySet (C.Dot (C.Ref "ret") (C.Ref n)) (C.Ref n) tp
		toSet D.Def{D.defName = n} = C.Set Nothing (C.Dot (C.Ref "ret") (C.Ref n)) (C.Ref n)
		
		defs' = (map def' . filter D.isDef) defs
		def' D.Def{D.defName = n, D.defType = tp, D.defPars = pars, D.defMods = mods} = C.CFunDecl{C.cfunMods = [], 
			C.cfunReturnType = showDataType tp, C.cfunName = structDefName name n,
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
			C.interfaceFuns = [wrapFun, initWrapFun]
		}
		defImpl' D.Def{D.defName = n, D.defType = tp, D.defBody = e, D.defPars = pars, D.defMods = mods} = C.CFun{C.cfunMods = [], 
			C.cfunReturnType = showDataType tp, C.cfunName = structDefName name n,
			C.cfunPars = if D.DefModStatic `notElem` mods then C.CFunPar (C.TPSimple name []) "self" : pars' else pars',
			C.cfunExps = tStm tp [] e}
			where
				pars' = map par' pars
				par' D.Def{D.defName = nn, D.defType = tpp} = C.CFunPar (showDataType tpp) nn
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
		hasCompare = any (("compare" == ). D.defName) defs
		compareImpl = if hasCompare then Just $ C.ImplFun (C.Fun C.InstanceFun (C.TPSimple "NSInteger" []) "compare" [C.FunPar "to" selfWrapTp "to"]) [
			C.Return $ C.CCall (C.Ref $ structDefName name "Compare") [C.Ref "_value", C.Dot (C.Ref "to") (C.Ref "value")]
			] else Nothing


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
		D.TPClass D.TPMStruct _ scl -> C.CCall (C.Ref $ D.className scl ++ "Hash") [ref]
		D.TPFloat -> C.Call (C.Call (C.Ref "NSNumber") "numberWith" [("double", ref)] []) "hash" [] []
		D.TPInt -> ref
		D.TPUInt -> ref
		D.TPBool -> ref
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
stringFormatForType D.TPInt = "%li"
stringFormatForType D.TPUInt = "%li"
stringFormatForType D.TPFloat = "%f"
stringFormatForType D.TPBool = "%d"
stringFormatForType (D.TPEArr n tp)  = "[" ++ strs ", " (replicate n (stringFormatForType tp)) ++ "]"
stringFormatForType _ = "%@"


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
			(case tp of
				D.TPClass D.TPMStruct _ scl -> [C.CCall (C.Ref $ D.className scl ++ "Description") [ref]]
				D.TPEArr n _ -> map (\j -> C.Index ref (C.IntConst j)) [0..n - 1]
				_ -> [ref]
				))
			where
				ref = C.Dot C.Self (C.Ref nm)
		
		

structDefName :: String -> String -> String
structDefName sn dn = lowFirst sn ++ cap dn
	where 
		lowFirst (x1:x2:xs)
			| isUpper x1 && isUpper x2 = toLower x1 : lowFirst (x2:xs)
			| otherwise = x1:x2:xs
		lowFirst x = x
{- Enum -}

enumValuesFun :: C.Fun
enumValuesFun = C.Fun C.ObjectFun (C.TPSimple "NSArray*" []) "values" []

genEnumInterface :: D.Class -> [C.FileStm]
genEnumInterface cl@D.Class {D.className = name, D.classDefs = defs} = [
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = classExtends cl,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs',
		C.interfaceFuns = intefaceFuns defs' ++ map (enumItemGetterFun name) (D.enumItems cl) ++ [enumValuesFun]
	}]
	where 
		defs' = filter ((/= "values") . D.defName) defs
	
enumItemGetterFun :: String -> D.Def -> C.Fun
enumItemGetterFun name D.Def{D.defName = itemName} = C.Fun C.ObjectFun (C.TPSimple (name ++ "*") []) itemName []

genEnumImpl :: D.Class -> [C.FileStm]
genEnumImpl cl@D.Class {D.className = clsName} = [
	C.Implementation {
		C.implName = clsName,
		C.implFields = (map (implField False) . filter needProperty) defs,
		C.implSynthesizes = (map synthesize . filter needProperty) defs,
		C.implFuns = [implCreate cl constr, implInit cl constr, initialize] ++ dealoc cl 
			++ implFuns defs ++ map itemGetter items ++ [valuesFun],
		C.implStaticFields = map stField items ++ [C.ImplField valuesVarName (C.TPSimple "NSArray*" []) [] C.Nop] 
	}]
	where
		valuesVarName =  "_" ++ clsName ++ "_values"
		items = D.enumItems cl
		defs = filter ((/= "values") . D.defName) $ D.classDefs cl
		constr = fromMaybe (error "No class constructor") (D.classConstructor cl)
		stField D.Def{D.defName = itemName} = C.ImplField ('_' : itemName) (C.TPSimple (clsName ++ "*") []) [] C.Nop
		itemGetter e@D.Def{D.defName = itemName} = C.ImplFun (enumItemGetterFun clsName e) [C.Return $ C.Ref $ '_' : itemName]
		initialize = C.ImplFun (C.Fun C.ObjectFun voidTp "initialize" []) (
			((C.Stm $ C.Call C.Super "initialize" [] []) : map initItem items) ++ [setValues])
		initItem :: D.Def -> C.Stm
		initItem D.Def{D.defName = itemName, D.defBody = body} = C.Set Nothing (C.Ref $ '_' : itemName) $ retain $ tExp newEnv body
		valuesFun = C.ImplFun enumValuesFun [C.Return $ C.Ref valuesVarName]
		setValues :: C.Stm
		setValues = C.Set Nothing (C.Ref valuesVarName) (C.Arr $ map (C.Ref . ('_' : ). D.defName) items)


{- Imports -}
procImports :: D.File -> ([C.FileStm], [C.FileStm])
procImports D.File{D.fileImports = imps, D.fileClasses = classes} = (h, m)
	where 
		filePossibleWeakImport D.File{D.fileCImports = [], D.fileClasses = cls} = all classPosibleWeakImport cls
		filePossibleWeakImport _ = False
		classPosibleWeakImport cl@D.Class{D.classMods = mods} = 
				(D.ClassModStub `elem` mods || D.ClassModStruct `notElem` mods) && not (hasExtends cl)
		classPosibleWeakImport _ = False
		hasExtends cl = cl `elem` extends
		extends = (map fst . concatMap (D.extendsRefs . D.classExtends)) classes
		cImport D.File{D.fileName = fn} = C.Import (fn ++ ".h")
		h = concatMap procH imps
		procH file
			| filePossibleWeakImport file = map decl . 
				filter (\ c -> not (D.isStruct c)) $ D.fileClasses file
			| otherwise = [cImport file]
		m = (map cImport . filter filePossibleWeakImport) imps
		decl cl 
			| D.isTrait cl = C.ProtocolDecl . D.className $ cl
			| otherwise = C.ClassDecl . D.className $ cl

{- DataType -}
showDataType :: D.DataType -> C.DataType
showDataType (D.TPEArr n tp) = C.TPArr n $ show (showDataType tp)
showDataType (D.TPArr _ _) = C.TPSimple "id<CNList>" []
showDataType (D.TPMap _ _)  = C.TPSimple "id<CNMap>" []
showDataType D.TPInt = C.TPSimple "NSInteger" []
showDataType D.TPUInt = C.TPSimple "NSUInteger" []
showDataType D.TPFloat = C.TPSimple "double" []
showDataType D.TPString = C.TPSimple "NSString*" []
showDataType D.TPBool = C.TPSimple "BOOL" []
showDataType (D.TPClass D.TPMStruct _ c) = C.TPSimple (D.className c) []
showDataType tp@(D.TPClass D.TPMType _ _) = showDataType $ fromMaybe (error "Not found super type for type") $ D.superType tp
showDataType (D.TPClass D.TPMClass _ c) 
	| D.className c == "ODObject" = C.TPSimple "NSObject*" []
	| otherwise = C.TPSimple (D.className c ++ "*") []
showDataType (D.TPClass D.TPMEnum _ c) = C.TPSimple (D.className c ++ "*") []
showDataType (D.TPClass D.TPMTrait _ c) = C.TPSimple "id" [D.className c]
showDataType (D.TPClass{}) = idTp
showDataType (D.TPSelf) = idTp
showDataType (D.TPTuple _) = C.TPSimple "CNTuple*" []
showDataType (D.TPOption _) = idTp
showDataType (D.TPFun D.TPVoid d) = C.TPBlock (showDataType d) []
showDataType (D.TPFun (D.TPTuple ss) d) = C.TPBlock (showDataType d) (map showDataType ss)
showDataType (D.TPFun s d) = C.TPBlock (showDataType d) [showDataType s]
showDataType (D.TPGenericWrap (D.TPClass D.TPMStruct _ _)) = idTp
showDataType (D.TPGenericWrap D.TPInt) = idTp
showDataType (D.TPGenericWrap D.TPUInt) = idTp
showDataType (D.TPGenericWrap D.TPFloat) = idTp
showDataType (D.TPGenericWrap D.TPBool) = idTp
showDataType (D.TPGenericWrap c) = showDataType c
showDataType D.TPData = C.TPSimple "void*" []
showDataType tp = C.TPSimple (show tp) []

{- Exp -}
tPars :: Env -> [(D.Def, D.Exp)] -> [(String, C.Exp)]
tPars env = 
	let env' = env {envCStruct = False}
	in map (\(d, e) -> (D.defName d, maybeVal (D.exprDataType e, D.defType d) $ tExp env' e))

tExpTo :: Env -> D.DataType -> D.Exp -> C.Exp 
tExpTo env tp e = maybeVal (D.exprDataType e, tp) (tExp env e)

castGeneric :: D.Exp -> C.Exp -> C.Exp
castGeneric dexp e = case D.exprDataType dexp of
	D.TPGenericWrap c@(D.TPClass D.TPMClass _ _) -> C.Cast (showDataType c) e
	D.TPGenericWrap c@(D.TPClass D.TPMEnum _ _) -> C.Cast (showDataType c) e
	D.TPGenericWrap c@D.TPTuple{} -> C.Cast (showDataType c) e
	_ -> e

data Env = Env{envCStruct :: Bool}
newEnv :: Env
newEnv = Env False


tExp :: Env -> D.Exp -> C.Exp
tExp _ (D.IntConst i) = C.IntConst i
tExp _ (D.StringConst i) = C.StringConst i
tExp _ (D.Nil) = C.Nil
tExp _ (D.BoolConst i) = C.BoolConst i
tExp _ (D.FloatConst i) = C.FloatConst i
tExp env (D.BoolOp Eq l r) = equals True (D.exprDataType l, tExp env l) (D.exprDataType r, tExp env r) 
tExp env (D.BoolOp NotEq l r) = equals False (D.exprDataType l, tExp env l) (D.exprDataType r, tExp env r) 
tExp env (D.BoolOp t l r) = C.BoolOp t (tExpTo env tp l) (tExpTo env tp r)
	where
		tp = ttp t
		ttp And = D.TPBool
		ttp Or = D.TPBool
		ttp _ = D.exprDataType l
tExp env (D.MathOp t l r) = let 
		l' = tExp env l
		r' = tExp env r
		ltp = case D.exprDataType l of
			D.TPGenericWrap tt -> tt
			tt -> tt
		rtp = case D.exprDataType r of
			D.TPGenericWrap tt -> tt
			tt -> tt
	in case ltp of
		D.TPArr _ _ -> addObjectToArray rtp l' r'
		D.TPMap _ _ -> addKVToMap env l' r
		D.TPString -> case rtp of
			D.TPString -> C.Call l' "stringByAppending" [("string", r')] []
			_ -> C.Call l' "stringByAppending" [("format",  C.StringConst $ stringFormatForType rtp)] [maybeVal (D.exprDataType r, rtp) r']
		_ -> C.MathOp t (tExpTo env ltp l) (tExpTo env ltp r)
tExp env (D.PlusPlus e) = C.PlusPlus (tExp env e)
tExp env (D.MinusMinus e) = C.MinusMinus (tExp env e)


tExp env (D.Dot (D.Self (D.TPClass D.TPMStruct _ c)) (D.Call D.Def {D.defName = name, D.defMods = mods} _ pars)) 
	| D.DefModField `elem` mods = C.Dot (C.Ref "self") (C.Ref name)
	| otherwise = C.CCall (C.Ref $ structDefName (D.className c) name) (C.Ref "self" : (map snd . tPars env) pars)
tExp env (D.Dot (D.Self stp) (D.Call D.Def{D.defMods = mods, D.defName = name} _ pars)) 
	| D.DefModField `elem` mods && null pars = C.Ref $ '_' : name
	| D.DefModField `elem` mods = C.CCall (C.Ref ('_' : name)) ((map snd . tPars env) pars)
	| D.DefModStatic `elem` mods = C.Call (C.Ref $ D.className $ D.tpClass stp) name (tPars env pars) []
	| otherwise = C.Call C.Self name (tPars env pars) []
tExp env d@(D.Dot l (D.Call D.Def{D.defName = name, D.defMods = mods} _ pars)) 
	| D.DefModField `elem` mods && null pars = castGeneric d $ C.Dot (tExpTo env ltp l) (C.Ref name)
	| D.DefModField `elem` mods = castGeneric d $ C.Dot (tExpTo env ltp l) $ C.CCall (C.Ref name) ((map snd . tPars env) pars)
	| D.DefModStruct `elem` mods = case ltp  of
		(D.TPClass D.TPMStruct _ c) -> structCall (D.className c) (tExpTo env ltp l)
		(D.TPObject D.TPMStruct c) -> C.CCall (C.Ref $ structDefName (D.className c) name) ((map snd . tPars env) pars)
		tp -> structCall (show tp) (tExpTo env ltp l)
	| D.DefModConstructor `elem` mods = callConstructor env (D.exprDataType d) pars
	| otherwise = castGeneric d $ C.Call (tExp env l) name (tPars env pars ) []
	where
		 structCall c self = C.CCall (C.Ref $ structDefName c name) (self : (map snd . tPars env) pars)
		 ltp = D.unwrapGeneric $ D.exprDataType l
tExp env (D.Dot l (D.Is dtp)) = C.Call (tExp env l) "isKindOf" [("class", C.Call (C.Ref $ D.dataTypeClassName dtp) "class" [] [])] []
tExp env (D.Dot l (D.As dtp)) = C.Call (tExp env l) "asKindOf" [("class", C.Call (C.Ref $ D.dataTypeClassName dtp) "class" [] [])] []
tExp env (D.Dot l (D.CastDot dtp)) = C.Cast (showDataType dtp) (tExp env l)
tExp env (D.Dot l (D.Cast tp c)) = C.Cast (showDataType tp) (tExp env (D.Dot l c))
tExp env (D.Dot l (D.LambdaCall c)) = C.CCall (tExp env (D.Dot l c)) [] 


tExp _ (D.Self _) = C.Self
tExp env (D.LambdaCall e) = C.CCall (tExp env e) []
tExp env (D.Call D.Def{D.defName = name, D.defMods = mods, D.defType = tp} _ pars)
	| D.DefModField `elem` mods = C.Ref $ '_' : name
	| D.DefModEnumItem `elem` mods = C.Ref $ '_' : name
	| D.DefModLocal `elem` mods && null pars = C.Ref name
	| D.DefModConstructor `elem` mods = callConstructor env tp pars
	| D.DefModGlobalVal `elem` mods = C.Ref name
	| D.DefModObject `elem` mods = C.Ref name
	| otherwise = C.CCall (C.Ref name) (map snd . tPars env $ pars)
tExp env (D.If cond t f) = C.InlineIf (tExpTo env D.TPBool cond) (tExp env t) (tExp env f)
tExp env ee@(D.Index e i) = case D.exprDataType e of
	D.TPObject D.TPMEnum _ -> castGeneric ee $ C.Index (C.Call (tExp env e)  "values" [] []) (tExp env i)
	D.TPMap k _ -> castGeneric ee $ C.Call (tExp env e) "apply" [("key", tExpTo env k i)] []
	D.TPArr _ _ -> castGeneric ee $ C.Call (tExp env e) "apply" [("index", tExpTo env D.TPUInt i)] []
	_ -> castGeneric ee $ C.Index (tExp env e) (tExp env i)
tExp _ (D.Lambda pars e rtp) = 
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
		unwrapPar (name, D.TPGenericWrap tp) = C.Var (showDataType tp) name (maybeVal (D.TPGenericWrap tp, tp) $ C.Ref $ name ++ "_") []
	in
	C.Lambda (map par' pars) (unwrapPars ++ tStm rtp [] e) (showDataType rtp)
tExp env (D.Arr exps) = C.Arr $ map (tExpToType env D.tpGeneric) exps
tExp env (D.Map exps) = C.Map $ map (tExpToType env D.tpGeneric *** tExpToType env D.tpGeneric) exps
tExp env (D.Tuple exps) = C.CCall (C.Ref "tuple") $ map (tExpToType env D.tpGeneric) exps
tExp env (D.Opt e) = let tp = D.exprDataType e
	in C.Call (C.Ref "CNOption") "opt" [("", maybeVal (tp, D.TPGenericWrap tp) (tExp env e))] []
tExp _ (D.None _) = C.Call (C.Ref "CNOption") "none" [] []
tExp env (D.Not e) = C.Not (tExp env e)
tExp env (D.Negative e) = C.Negative (tExp env e)
tExp env (D.Cast dtp e) = let 
		stp = D.exprDataType e
		toString format = C.Call (C.Ref "NSString") "stringWith" [("format", C.StringConst format)] [tExpTo env stp e]
	in case (D.unwrapGeneric stp, D.unwrapGeneric dtp) of
		(D.TPInt, D.TPString) -> toString "%li" 
		(D.TPUInt, D.TPString) -> toString "%li" 
		(D.TPFloat, D.TPString) -> toString "%f" 
		(D.TPArr{}, D.TPEArr{}) ->
			case e of
				D.Arr exps -> C.EArr $ map (tExp env) exps
				_ -> error $ "Could not convert to EArr " ++ show e
		(stp', _) -> C.Cast (showDataType dtp) (tExpTo env stp' e)

tExp _ e@D.ExpDError{} = C.Error $ show e
tExp _ e@D.ExpLError{} = C.Error $ show e
tExp _ x = C.Error $ "No tExp for " ++ show x

tExpToType :: Env -> D.DataType -> D.Exp -> C.Exp
tExpToType env tp e = maybeVal (D.exprDataType e, tp) (tExp env e)

eArraySet :: C.Exp -> C.Exp -> D.DataType -> C.Exp
eArraySet l' r' (D.TPEArr n tp) = C.CCall (C.Ref "memcpy") [l', r', C.CCall (C.Ref "sizeof") [C.Index (C.Ref (show $ showDataType tp)) (C.IntConst n)]]

tStm :: D.DataType -> [D.Exp] -> D.Exp -> [C.Stm]
tStm _ _ (D.Nop) = []

tStm _ _ (D.Braces []) = []
tStm v _ (D.Braces [x]) = tStm v [] x
tStm v _ (D.Braces xs) = concatMap (tStm v xs) xs

tStm v _ (D.If cond t f) = [C.If (tExpTo newEnv D.TPBool cond) (tStm v [] t) (tStm v [] f)]
tStm v _ (D.While cond t) = [C.While (tExpTo newEnv D.TPBool cond) (tStm v [] t)]
tStm v _ (D.Do cond t) = [C.Do (tExpTo newEnv D.TPBool cond) (tStm v [] t)]

tStm _ _ (D.Set (Just t) l r) = let 
		l' = tExp newEnv l
		r' = tExp newEnv r
		ltp = D.exprDataType l
		rtp = D.exprDataType r
	in case ltp of
		tp@D.TPEArr{} -> [C.Stm $ eArraySet l' r' tp]
		D.TPArr _ _ ->  case t of
			Plus -> [C.Set Nothing l' (addObjectToArray rtp l' r')]
			Minus -> [C.Set Nothing l' (removeObjectFromArray rtp l' r')]
		D.TPMap _ _ -> [C.Set Nothing l' (addKVToMap newEnv l' r)]
		_ -> [C.Set (Just t) l' (maybeVal (rtp, ltp) r')]
tStm _ _ (D.Set tp l r) = [C.Set tp (tExp newEnv l) (maybeVal (D.exprDataType r, D.exprDataType l) (tExp newEnv r))]
tStm D.TPVoid _ (D.Return True _) = [C.Return C.Nop]
tStm D.TPVoid _ (D.Return _ e) = [C.Stm $ tExp newEnv{envCStruct = False} e]
tStm tp _ (D.Return _ e) = [C.Return $ tExpToType newEnv{envCStruct = False} tp e]
tStm _ parexps (D.Val def@D.Def{D.defName = name, D.defType = tp, D.defBody = e, D.defMods = mods}) = 
	[C.Var (showDataType tp) name (tExpToType newEnv tp e) ["__block" | needBlock]]
	where 
		needBlock = D.DefModMutable `elem` mods && existsSetInLambda
		existsSetInLambda = any (isJust . setsInLambda) parLambdas
		parLambdas :: [D.Exp]
		parLambdas = D.forExp isLambda (D.Braces parexps)
		isLambda ee@D.Lambda{} = [ee]
		isLambda _ = []
		setsInLambda (D.Lambda _ lambdaExpr _) = D.forExp isSet lambdaExpr
		isSet ee@(D.Set _ (D.Call d _ _) _) = if D.defName d == D.defName def then Just ee else Nothing
		isSet _ = Nothing
tStm _ _ (D.Throw e) = [C.Throw $ tExp newEnv e]
tStm _ _ D.Break = [C.Break]

tStm _ _ x = [C.Stm $ tExp newEnv x]

equals :: Bool -> (D.DataType, C.Exp) -> (D.DataType, C.Exp) -> C.Exp
equals False (D.TPClass D.TPMEnum _ _, e1) (D.TPClass D.TPMEnum _ _, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPClass D.TPMClass _ cl, e1) (_, e2) 
	| not (equalsIsPosible cl) = C.BoolOp NotEq e1 e2
equals False (D.TPInt, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPUInt, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPBool, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (D.TPNil, e1) (_, e2) = C.BoolOp NotEq e1 e2
equals False (_, e1) (D.TPNil, e2) = C.BoolOp NotEq e1 e2
equals False s1@(_, _) s2@(_, _) = C.Not $ equals True s1 s2

equals True (D.TPClass D.TPMEnum _ _, e1) (D.TPClass D.TPMEnum _ _, e2) = C.BoolOp Eq e1 e2
equals True (D.TPClass D.TPMStruct _ c, e1) (_, e2) = C.CCall (C.Ref (D.className c ++ "Eq")) [e1, e2]
equals True (D.TPGenericWrap{}, e1) (D.TPGenericWrap{}, e2) = C.Call e1 "isEqual" [("", e2)] []
equals True (stp@(D.TPGenericWrap stp'), e1) (dtp, e2) = equals True (stp', maybeVal (stp, dtp) e1) (dtp, e2)
equals True (stp, e1) (dtp@(D.TPGenericWrap dtp'), e2) = equals True (stp, e1) (dtp', maybeVal (stp, dtp) e2)
equals True (D.TPFloat, e1) (_, e2) = C.CCall (C.Ref "eqf") [e1, e2]
equals True (D.TPInt, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (D.TPUInt, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (D.TPBool, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (D.TPNil, e1) (_, e2) = C.BoolOp Eq e1 e2
equals True (_, e1) (D.TPNil, e2) = C.BoolOp Eq e1 e2
equals True (D.TPClass D.TPMClass _ cl, e1) (_, e2) 
	| not (equalsIsPosible cl) = C.BoolOp Eq e1 e2
equals True (D.TPEArr n tp, e1) (_, e2)  =
	C.CCall (C.Ref "memcmp") [e1, e2, C.CCall (C.Ref "sizeof") [C.Index (C.Ref (show $ showDataType tp)) (C.IntConst n)]]
equals True (_, e1) (_, e2) = C.Call e1 "isEqual" [("", e2)] []

addObjectToArray :: D.DataType -> C.Exp -> C.Exp -> C.Exp
addObjectToArray tp a obj = C.Call a "arrayByAdding" [("object", maybeVal (tp, D.wrapGeneric tp) obj)] []
removeObjectFromArray :: D.DataType -> C.Exp -> C.Exp -> C.Exp
removeObjectFromArray tp a obj = C.Call a "arrayByRemoving" [("object", maybeVal (tp, D.wrapGeneric tp) obj)] []

addKVToMap :: Env -> C.Exp -> D.Exp -> C.Exp
addKVToMap env a (D.Tuple [k, v]) = C.Call a "dictionaryByAdding" [("value", tExp env v), ("forKey", tExp env k)] []

callConstructor :: Env -> D.DataType -> [(D.Def, D.Exp)] -> C.Exp
callConstructor env tp pars = let
	name = D.dataTypeClassName tp
	in case tp of 
		D.TPClass D.TPMStruct _ _ -> 
			if envCStruct env then C.EArr ((map snd. tPars env) pars) else C.CCall 
				(C.Ref (name++ "Make"))
				((map snd. tPars env) pars)
	 	_ -> C.Call 
				(C.Ref name) 
				(createFunName $ name ++ if null pars then "" else "With") 
				(tPars env pars)
				[]

data MaybeValTP = TPGen | TPNum | TPStruct | TPNoMatter | TPBool | TPFloat
maybeVal :: (D.DataType, D.DataType) -> C.Exp -> C.Exp
maybeVal (stp, dtp) e = let 
	tp D.TPGenericWrap{} = TPGen
	tp (D.TPClass D.TPMGeneric _ _) = TPGen
	tp (D.TPClass D.TPMStruct _ _) = TPStruct
	tp D.TPInt{} = TPNum
	tp D.TPUInt{} = TPNum
	tp D.TPFloat{} = TPFloat
	tp D.TPBool{} = TPBool
	tp _ = TPNoMatter
	in case (tp stp, tp dtp) of
		(TPStruct, TPGen) -> C.CCall (C.Ref "wrap") [C.Ref $ D.className $ D.tpClass stp, e]
		(TPGen, TPStruct) -> C.CCall (C.Ref "uwrap") [C.Ref $ D.className $ D.tpClass dtp, e]
		(TPNum, TPGen) -> case e of
			C.IntConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref "numi") [e]
		(TPGen, TPNum) -> C.CCall (C.Ref "unumi") [e]
		(TPFloat, TPGen) -> case e of
			C.FloatConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref "numf") [e]
		(TPGen, TPFloat) -> C.CCall (C.Ref "unumf") [e]
		(TPBool, TPGen) -> case e of
			C.BoolConst _ -> C.ObjCConst e
			_ -> C.CCall (C.Ref "numb") [e]
		(TPGen, TPBool) -> C.CCall (C.Ref "unumb") [e]
		_ -> e
