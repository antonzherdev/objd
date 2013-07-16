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
		
		
		h = [C.Import "objd.h"] 
			++ cImports'
			++ fst dImports' 
			++ [C.EmptyLine] 
			++ map classDecl (cls ++ enums) 
			++ map structDecl structs
			++ [C.EmptyLine] 
			++ concatMap genH classes
		
		structDecl c = C.TypeDefStruct (D.className c) (D.className c)
		enumsImpl = concatMap genEnumImpl enums
		structImpl = concatMap genStructImpl structs 
		stmsImpl = map stmToImpl cls
		classDecl c = C.ClassDecl $ D.className c
		m = if null enumsImpl && null stmsImpl && null structImpl then []
			else [C.Import (name ++ ".h") , C.EmptyLine] ++ snd dImports' ++ enumsImpl ++ stmsImpl ++ structImpl
		genH c
			| isClass c = [stmToInterface c]
			| isStruct c = genStruct c
			| D.isEnum c = genEnumInterface c
			| isTrait c = [genProtocol c]
			| otherwise = [] 
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
		constr = D.classConstructor cl
		staticGetters = (map staticGetterFun .filter (\f -> D.isField f && D.isStatic f)) defs
		
classExtends :: D.Class -> C.Extends
classExtends cl = maybe (C.Extends "NSObject" []) (ext . D.extendsClass) (D.classExtends cl)
	where
		ext ccl
			| D.ClassModTrait `elem` D.classMods ccl = C.Extends "NSObject" [D.className ccl]
			| D.className ccl == "ODObject" = C.Extends "NSObject" []
			| otherwise = C.Extends (D.className ccl) []


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
		mutModes (D.TPClass D.TPMClass _ _) = [C.Retain]
		mutModes (D.TPClass D.TPMEnum _ _) = [C.Retain]
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
genProtocol (D.Class {D.className = name, D.classDefs = defs}) =
	C.Protocol {
		C.interfaceName = name,
		C.interfaceFuns = intefaceFuns defs
	}


{- Implementation -}

stmToImpl :: D.Class -> C.FileStm
stmToImpl cl@D.Class {D.className = clsName, D.classDefs = defs} =
	C.Implementation {
		C.implName = clsName,
		C.implFields = map implField implFields,
		C.implSynthesizes = (map synthesize . filter needProperty) implFields,
		C.implFuns = [implCreate cl constr, implInit cl constr] ++ maybeToList (implInitialize cl) ++ dealoc cl 
			++ implFuns defs ++ staticGetters,
		C.implStaticFields = map implField staticFields
	}
	where
		constr = D.classConstructor cl
		implFields = filter needField defs
		needField f = D.isField f && not (D.isStatic f)
		isStaticField f = D.isStatic f && D.isField f
		staticFields = filter isStaticField defs
		staticGetters = (map staticGetter . filter((D.DefModPrivate `notElem`) . D.defMods)) staticFields
		staticGetter f@D.Def{D.defName = name} = C.ImplFun (staticGetterFun f) [C.Return $ C.Ref $ '_' : name]

synthesize :: D.Def -> C.ImplSynthesize
synthesize D.Def{D.defName = x} = C.ImplSynthesize x ('_' : x)
implField :: D.Def -> C.ImplField
implField D.Def{D.defName = x, D.defType = tp, D.defMods = mods} = C.ImplField ('_' : x) (showDataType tp) ["__weak" | D.DefModWeak `elem` mods]

implCreate :: D.Class -> D.Def -> C.ImplFun
implCreate cl constr@D.Def{D.defPars = constrPars} = let 
		clsName = D.className cl
		pars D.Def{D.defName = name} = (name, C.Ref name)
	in C.ImplFun (createFun clsName constr) [C.Return $
		autorelease $ C.Call (C.Call (C.Ref clsName) "alloc" []) (if null constrPars then "init" else "initWith") (map pars constrPars)
	] 

retain :: C.Exp -> C.Exp
retain f 
	| arc = f
	| otherwise = C.Call f "retain" []			

autorelease :: C.Exp -> C.Exp
autorelease e 
	| arc = e
	| otherwise = C.Call e "autorelease" []

dealoc :: D.Class -> [C.ImplFun]
dealoc cl 
	| arc = []
	| otherwise = [C.ImplFun (C.Fun C.InstanceFun voidTp "dealloc" []) $
		 mapMaybe releaseField (D.classFields cl) ++ [C.Stm $C.Call C.Super "dealloc" []]]
	where 
		releaseField D.Def{D.defName = name, D.defType = D.TPClass{}} = Just $ C.Stm $ C.Call (C.Ref $ '_' : name) "release" []
		releaseField _ = Nothing

implInitialize :: D.Class -> Maybe C.ImplFun 		
implInitialize cl = let 
	fields = filter hasInitialize (D.classFields cl)
	hasInitialize D.Def{D.defBody = D.Nop} = False
	hasInitialize d = D.isField d && D.isStatic d
	in case fields of
		[] -> Nothing
		_ -> Just $ C.ImplFun (C.Fun C.ObjectFun voidTp "initialize" []) (
			((C.Stm $ C.Call C.Super "initialize" []) : map implInitField fields))



implInit :: D.Class -> D.Def -> C.ImplFun
implInit cl constr@D.Def{D.defPars = constrPars}  = C.ImplFun (initFun constr) (
			[C.Set Nothing C.Self (superInit $ D.classExtends cl)]
			++ implInitFields (filter hasField constrPars) (filter hasInit (D.classDefs cl))
			++ [C.Stm C.Nop, C.Return C.Self]
			)
	where
		hasInit D.Def{D.defBody = D.Nop} = False
		hasInit d = D.isField d && not (D.isStatic d)

		hasField f= any ((D.defName f == ) . D.defName) (D.classDefs cl)

		superInit Nothing = C.Call C.Super "init" []
		superInit (Just (D.Extends _ _ [])) = C.Call C.Super "init" []
		superInit (Just (D.Extends _ _ pars)) = C.Call C.Super "initWith" $ map (D.defName *** tExp) pars

		implInitFields :: [D.Def] -> [D.Def] -> [C.Stm]
		implInitFields [] [] = []
		implInitFields co fields = [C.If C.Self (map implConstrField co ++ map implInitField fields) []]
		implConstrField D.Def{D.defName = name, D.defType = tp} = C.Set Nothing (C.Ref $ '_' : name) (implRight tp) 
			where
				implRight D.TPClass{} = retain $ C.Ref name
				implRight _ = C.Ref name

implInitField :: D.Def -> C.Stm
implInitField D.Def{D.defName = name, D.defBody = def, D.defType = tp} = C.Set Nothing (C.Ref $ '_' : name) (tExpTo tp def)
		
implFuns :: [D.Def] -> [C.ImplFun]
implFuns = map stm2ImplFun . filter D.isDef
	where
		stm2ImplFun def@D.Def {D.defName = name, D.defBody = db, D.defMods = mods, D.defType = tp}
			| D.DefModAbstract `elem` mods = C.ImplFun (stm2Fun def) [C.Throw $ C.StringConst $ "Method " ++ name++ " is abstract"]
			| otherwise = C.ImplFun (stm2Fun def) (tStm tp db)
		
		
{- Struct -}
genStruct :: D.Class -> [C.FileStm]
genStruct D.Class {D.className = name, D.classDefs = defs} = [C.Struct name fields', con, eq ] ++ defs' ++ [C.EmptyLine]
	where
		fields = filter D.isField defs
		fields' = map toField fields
		toField D.Def{D.defName = n, D.defType = tp, D.defMods = mods} = C.ImplField n (showDataType tp) ["__weak" | D.DefModWeak `elem` mods]
		con = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple name [], 
			C.cfunName = name ++ "Make", C.cfunPars = map toPar fields, C.cfunExps = 
				[C.Var (C.TPSimple name []) "ret" C.Nop] ++
				map toSet fields ++
				[C.Return $ C.Ref "ret"]
		}
		eq = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = C.TPSimple "BOOL" [], C.cfunName = name ++ "Eq", 
			C.cfunPars = [C.CFunPar (C.TPSimple name []) "s1", C.CFunPar (C.TPSimple name []) "s2"], 
			C.cfunExps = [C.Return $ foldl foldEq C.Nop fields]
		}
		toPar D.Def{D.defName = n, D.defType = tp}= C.CFunPar (showDataType tp) n
		toSet D.Def{D.defName = n} = C.Set Nothing (C.Dot (C.Ref "ret") n) (C.Ref n)
		foldEq :: C.Exp -> D.Def -> C.Exp
		foldEq C.Nop d = eqd d
		foldEq p d = C.BoolOp And p (eqd d)
		eqd :: D.Def -> C.Exp
		eqd D.Def{D.defName = n, D.defType = tp} = equals True (tp, C.Dot (C.Ref "s1") n) (tp, C.Dot (C.Ref "s2") n)

		defs' = (map def' . filter D.isDef) defs
		def' D.Def{D.defName = n, D.defType = tp, D.defPars = pars, D.defMods = mods} = C.CFunDecl{C.cfunMods = [], 
			C.cfunReturnType = showDataType tp, C.cfunName = structDefName name n,
			C.cfunPars =  if D.DefModStatic `notElem` mods then C.CFunPar (C.TPSimple name []) "self" : pars' else pars'}
			where
				pars' = map par' pars
				par' D.Def{D.defName = nn, D.defType = tpp} = C.CFunPar (showDataType tpp) nn


genStructImpl :: D.Class -> [C.FileStm]
genStructImpl D.Class {D.className = name, D.classDefs = defs} = (map def' . filter D.isDef) defs
	where 
		def' D.Def{D.defName = n, D.defType = tp, D.defBody = e, D.defPars = pars, D.defMods = mods} = C.CFun{C.cfunMods = [], 
			C.cfunReturnType = showDataType tp, C.cfunName = structDefName name n,
			C.cfunPars = if D.DefModStatic `notElem` mods then C.CFunPar (C.TPSimple name []) "self" : pars' else pars',
			C.cfunExps = tStm tp e}
			where
				pars' = map par' pars
				par' D.Def{D.defName = nn, D.defType = tpp} = C.CFunPar (showDataType tpp) nn
		

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
		C.interfaceProperties = (map fieldToProperty . filter D.isField) defs',
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
		C.implFields = (map implField . filter D.isField) defs,
		C.implSynthesizes = (map synthesize . filter D.isField) defs,
		C.implFuns = [implCreate cl constr, implInit cl constr, initialize] ++ dealoc cl ++ implFuns defs ++ map itemGetter items ++ [valuesFun],
		C.implStaticFields = map stField items ++ [C.ImplField "values" (C.TPSimple "NSArray*" []) []]
	}]
	where
		items = D.enumItems cl
		defs = filter ((/= "values") . D.defName) $ D.classDefs cl
		constr = D.classConstructor cl
		stField D.Def{D.defName = itemName} = C.ImplField ('_' : itemName) (C.TPSimple (clsName ++ "*") []) []
		itemGetter e@D.Def{D.defName = itemName} = C.ImplFun (enumItemGetterFun clsName e) [C.Return $ C.Ref $ '_' : itemName]
		initialize = C.ImplFun (C.Fun C.ObjectFun voidTp "initialize" []) (
			((C.Stm $ C.Call C.Super "initialize" []) : map initItem items) ++ [setValues])
		initItem :: D.Def -> C.Stm
		initItem D.Def{D.defName = itemName, D.defBody = body} = C.Set Nothing (C.Ref $ '_' : itemName) $ retain $ tExp body
		valuesFun = C.ImplFun enumValuesFun [C.Return $ C.Ref "values"]
		setValues :: C.Stm
		setValues = C.Set Nothing (C.Ref "values") (C.Arr $ map (C.Ref . ('_' : ). D.defName) items)


{- Imports -}
procImports :: D.File -> ([C.FileStm], [C.FileStm])
procImports D.File{D.fileImports = imps} = (h, m)
	where 
		filePossibleWeakImport D.File{D.fileCImports = [], D.fileClasses = cls} = all classPosibleWeakImport cls
		filePossibleWeakImport _ = False
		classPosibleWeakImport D.Class{D.classMods = mods} = D.ClassModStruct `notElem` mods
		classPosibleWeakImport _ = False
		cImport D.File{D.fileName = fn} = C.Import (fn ++ ".h")
		h = concatMap procH imps
		procH file
			| filePossibleWeakImport file = map (C.ClassDecl . D.className) . 
				filter (\ c -> not (D.isStruct c) && not (D.isTrait c) ) $ D.fileClasses file
			| otherwise = [cImport file]
		m = (map cImport . filter filePossibleWeakImport) imps

{- DataType -}
showDataType :: D.DataType -> C.DataType
showDataType (D.TPArr False _) = C.TPSimple "NSArray*" []
showDataType (D.TPArr True _) = C.TPSimple "NSMutableArray*" []
showDataType (D.TPMap False _ _)  = C.TPSimple "NSDictionary*" []
showDataType (D.TPMap True _ _) = C.TPSimple "NSMutableDictionary*" []
showDataType D.TPInt = C.TPSimple "NSInteger" []
showDataType D.TPUInt = C.TPSimple "NSUInteger" []
showDataType D.TPFloat = C.TPSimple "double" []
showDataType D.TPString = C.TPSimple "NSString*" []
showDataType D.TPBool = C.TPSimple "BOOL" []
showDataType (D.TPClass D.TPMStruct _ c) = C.TPSimple (D.className c) []
showDataType (D.TPClass D.TPMClass _ c) 
	| D.className c == "ODObject" = C.TPSimple "NSObject*" []
	| otherwise = C.TPSimple (D.className c ++ "*") []
showDataType (D.TPClass D.TPMEnum _ c) = C.TPSimple (D.className c ++ "*") []
showDataType (D.TPClass D.TPMTrait _ c) = C.TPSimple "id" [D.className c]
showDataType (D.TPClass{}) = idTp
showDataType (D.TPSelf) = idTp
showDataType (D.TPTuple _) = C.TPSimple "CNTuple*" []
showDataType (D.TPOption c) =  showDataType c
showDataType (D.TPFun D.TPVoid d) = C.TPBlock (showDataType d) []
showDataType (D.TPFun (D.TPTuple ss) d) = C.TPBlock (showDataType d) (map showDataType ss)
showDataType (D.TPFun s d) = C.TPBlock (showDataType d) [showDataType s]
showDataType (D.TPGenericWrap (D.TPClass D.TPMStruct _ _)) = idTp
showDataType (D.TPGenericWrap D.TPInt) = idTp
showDataType (D.TPGenericWrap D.TPUInt) = idTp
showDataType (D.TPGenericWrap D.TPFloat) = idTp
showDataType (D.TPGenericWrap D.TPBool) = idTp
showDataType (D.TPGenericWrap c) = showDataType c
showDataType tp = C.TPSimple (show tp) []

{- Exp -}
tPars :: [(D.Def, D.Exp)] -> [(String, C.Exp)]
tPars = map (\(d, e) -> (D.defName d, maybeVal (D.exprDataType e, D.defType d) $ tExp e))

tExpTo :: D.DataType -> D.Exp -> C.Exp 
tExpTo tp e = maybeVal (D.exprDataType e, tp) (tExp e)

tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i
tExp (D.StringConst i) = C.StringConst i
tExp (D.Nil) = C.Nil
tExp (D.BoolConst i) = C.BoolConst i
tExp (D.FloatConst i) = C.FloatConst i
tExp (D.BoolOp Eq l r) = equals True (D.exprDataType l, tExp l) (D.exprDataType r, tExp r) 
tExp (D.BoolOp NotEq l r) = equals False (D.exprDataType l, tExp l) (D.exprDataType r, tExp r) 
tExp (D.BoolOp t l r) = C.BoolOp t (tExpTo D.TPBool l) (tExpTo D.TPBool r)
tExp (D.MathOp t l r) = let 
		l' = tExp l
		r' = tExp r
		ltp = case D.exprDataType l of
			D.TPGenericWrap tt -> tt
			tt -> tt
		{-rtp = D.exprDataType r-}
	in case ltp of
		D.TPArr _ _ -> addObjectToArray l' r'
		D.TPMap _ _ _ -> addKVToMap l' r
		_ -> C.MathOp t (tExpTo ltp l) (tExpTo ltp r)
tExp (D.PlusPlus e) = C.PlusPlus (tExp e)
tExp (D.MinusMinus e) = C.MinusMinus (tExp e)


tExp (D.Dot (D.Self (D.TPClass D.TPMStruct _ c)) (D.Call D.Def {D.defName = name, D.defMods = mods} _ pars)) 
	| D.DefModField `elem` mods = C.Dot (C.Ref "self") name
	| otherwise = C.CCall (structDefName (D.className c) name) (C.Ref "self" : (map snd . tPars) pars)

tExp (D.Dot (D.Self (D.TPClass _ _ c)) (D.Call D.Def{D.defMods = mods, D.defName = name} _ pars)) 
	| D.DefModField `elem` mods = C.Ref $ '_' : name
	| D.DefModStatic `elem` mods = C.Call (C.Ref $ D.className c) name (tPars pars)
	| otherwise = C.Call C.Self name (tPars pars)
tExp (D.Dot l (D.Call D.Def{D.defName = name, D.defMods = mods} _ pars)) 
	| D.DefModField `elem` mods = C.Dot (tExp l) name
	| otherwise = case D.exprDataType l of
		(D.TPGenericWrap tp@(D.TPClass D.TPMStruct _ c)) -> structCall c (tExpTo tp l)
		(D.TPClass D.TPMStruct _ c) -> structCall c (tExp l)
		(D.TPObject D.TPMStruct c) -> C.CCall (structDefName (D.className c) name) ((map snd . tPars) pars)
		_ -> C.Call (tExp l) name (tPars pars)
	where
		 structCall c self = C.CCall (structDefName (D.className c) name) (self : (map snd . tPars) pars)

tExp (D.Self _) = C.Self
tExp (D.Call D.Def{D.defName = name, D.defMods = mods, D.defType = tp} _ pars)
	| D.DefModField `elem` mods = C.Ref $ '_' : name
	| D.DefModLocal `elem` mods && null pars = C.Ref name
	| D.DefModConstructor `elem` mods = case tp of 
		D.TPClass D.TPMStruct _ _ -> C.CCall 
			(name++ "Make")
			((map snd. tPars) pars)
	 	_ ->
			C.Call 
			(C.Ref name) 
			(createFunName $ name ++ if null pars then "" else "With") 
			(tPars pars)
	| D.DefModGlobalVal `elem` mods = C.Ref name
	| D.DefModObject `elem` mods = C.Ref name
	| otherwise = C.CCall name (map snd . tPars $ pars)
tExp (D.If cond t f) = C.InlineIf (tExp cond) (tExp t) (tExp f)
tExp (D.Index e i) = case D.exprDataType e of
	D.TPObject D.TPMEnum _ -> C.Index (C.Call (tExp e)  "values" []) (tExp i)
	D.TPMap _ k _ -> C.Call (tExp e) "optionObjectFor" [("key", tExpTo k i)]
	_ -> C.Index (tExp e) (tExp i)
tExp (D.Lambda pars e rtp) = 
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
		unwrapPar (name, D.TPGenericWrap tp) = C.Var (showDataType tp) name (maybeVal (D.TPGenericWrap tp, tp) $ C.Ref $ name ++ "_")
	in
	C.Lambda (map par' pars) (unwrapPars ++ tStm rtp e) (showDataType rtp)
tExp (D.Arr exps) = C.Arr $ map (tExpToType tpGeneric) exps
tExp (D.Map exps) = C.Map $ map (tExpToType tpGeneric *** tExpToType tpGeneric) exps
tExp (D.Tuple exps) = C.CCall "tuple" $ map (tExpToType tpGeneric) exps
tExp (D.Opt e) = let tp = D.exprDataType e
	in C.Call (C.Ref "CNOption") "opt" [("", maybeVal (tp, D.TPGenericWrap tp) (tExp e))]
tExp (D.None _) = C.Call (C.Ref "CNOption") "none" []
tExp (D.Not e) = C.Not (tExp e)
tExp (D.Negative e) = C.Negative (tExp e)
tExp (D.Cast dtp e) = let stp = D.exprDataType e
	in case (stp, dtp) of
		(D.TPMap False _ _, D.TPMap True _ _) -> C.Call (tExp e) "mutableCopy" []
		(D.TPArr False _, D.TPArr True _) -> C.Call (tExp e) "mutableCopy" []
tExp e@D.ExpDError{} = C.Error $ show e
tExp e@D.ExpLError{} = C.Error $ show e
tExp x = C.Error $ "No tExp for " ++ show x

tpGeneric :: D.DataType
tpGeneric = D.TPClass D.TPMGeneric [] (D.Generic "?")
tExpToType :: D.DataType -> D.Exp -> C.Exp
tExpToType tp e = maybeVal (D.exprDataType e, tp) (tExp e)

tStm :: D.DataType -> D.Exp -> [C.Stm]
tStm _ (D.Nop) = []

tStm _ (D.Braces []) = []
tStm v (D.Braces [x]) = tStm v x
tStm v (D.Braces xs) = concatMap (tStm v) xs

tStm v (D.If cond t f) = [C.If (tExp cond) (tStm v t) (tStm v f)]

tStm _ (D.Set (Just t) l r) = let 
		l' = tExp l
		r' = tExp r
		ltp = D.exprDataType l
		rtp = D.exprDataType r
	in case ltp of
		D.TPArr _ _ -> [C.Set Nothing l' (addObjectToArray l' r')]
		D.TPMap _ _ _ -> [C.Set Nothing l' (addKVToMap l' r)]
		_ -> [C.Set (Just t) l' (maybeVal (rtp, ltp) r')]
tStm _ (D.Set tp l r) = [C.Set tp (tExp l) (maybeVal (D.exprDataType r, D.exprDataType l) (tExp r))]
tStm D.TPVoid (D.Return e) = [C.Stm $ tExp e]
tStm tp (D.Return e) = [C.Return $ tExpToType tp e]
tStm _ (D.Val D.Def{D.defName = name, D.defType = tp, D.defBody = e}) = [C.Var (showDataType tp) name (tExpToType tp e)]
tStm _ (D.Throw e) = [C.Throw $ tExp e]

tStm _ x = [C.Stm $ tExp x]

equals :: Bool -> (D.DataType, C.Exp) -> (D.DataType, C.Exp) -> C.Exp
equals False (D.TPClass D.TPMEnum _ _, e1) (D.TPClass D.TPMEnum _ _, e2) = C.BoolOp NotEq e1 e2
equals True (D.TPClass D.TPMEnum _ _, e1) (D.TPClass D.TPMEnum _ _, e2) = C.BoolOp Eq e1 e2
equals False s1@(D.TPClass{}, _) s2@(D.TPClass{}, _) = C.Not $ equals True s1 s2
equals True (D.TPClass D.TPMStruct _ c, e1) (_, e2) = C.CCall (D.className c ++ "Eq") [e1, e2]
equals True (D.TPClass {}, e1) (D.TPClass _ _ _, e2) = C.Call e1 "isEqual" [("", e2)]
equals True (stp@(D.TPGenericWrap _), e1) (dtp, e2) = C.BoolOp Eq (maybeVal (stp, dtp) e1) e2
equals True (stp, e1) (dtp@(D.TPGenericWrap _), e2) = C.BoolOp Eq e1 (maybeVal (stp, dtp) e2)
equals True (D.TPFloat, e1) (D.TPFloat, e2) = C.CCall "eqf" [e1, e2]
equals False (D.TPFloat, e1) (D.TPFloat, e2) = C.Not $ C.CCall "eqf" [e1, e2]
equals True (_, e1) (_, e2) = C.BoolOp Eq e1 e2
equals False (_, e1) (_, e2) = C.BoolOp NotEq e1 e2

addObjectToArray :: C.Exp -> C.Exp -> C.Exp
addObjectToArray a obj = C.Call a "arrayByAdding" [("object", obj)]

addKVToMap :: C.Exp -> D.Exp -> C.Exp
addKVToMap a (D.Tuple [k, v]) = C.Call a "dictionaryByAdding" [("value", tExp v), ("forKey", tExp k)]


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
		(TPStruct, TPGen) -> C.CCall "val" [e]
		(TPGen, TPStruct) -> C.CCall "uval" [C.Ref $ D.className $ D.tpClass dtp, e]
		(TPNum, TPGen) -> case e of
			C.IntConst _ -> C.ObjCConst e
			_ -> C.CCall "numi" [e]
		(TPGen, TPNum) -> C.CCall "unumi" [e]
		(TPFloat, TPGen) -> case e of
			C.FloatConst _ -> C.ObjCConst e
			_ -> C.CCall "numf" [e]
		(TPGen, TPFloat) -> C.CCall "unumf" [e]
		(TPBool, TPGen) -> case e of
			C.BoolConst _ -> C.ObjCConst e
			_ -> C.CCall "numb" [e]
		(TPGen, TPBool) -> C.CCall "unumb" [e]
		_ -> e
