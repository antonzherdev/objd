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
		cls = filter (\c -> D.isRealClass c && not (D.isStruct c) && not (D.isTrait c)) classes
		structs = filter (\c -> D.isRealClass c && D.isStruct c) classes
		enums = filter D.isEnum classes
		cImports' = map cImport' cImports
		cImport' (D.CImportLib n) = C.ImportLib n
		cImport' (D.CImportUser n) = C.Import n
		dImports' = procImports f

		traits = filter (\c -> D.isRealClass c && D.isTrait c) classes

		h = [C.Import "objd.h"] 
			++ cImports'
			++ fst dImports' 
			++ [C.EmptyLine] 
			++ map classDecl cls
			++ [C.EmptyLine] 
			++ concatMap genStruct structs 
			++ map genProtocol traits
			++ concatMap genEnumInterface enums 
			++ map stmToInterface cls

		enumsImpl = concatMap genEnumImpl enums
		stmsImpl = map stmToImpl cls
		classDecl c = C.ClassDecl $ D.className c
		m = if null enumsImpl && null stmsImpl then []
			else [C.Import (name ++ ".h") , C.EmptyLine] ++ snd dImports' ++ enumsImpl ++ stmsImpl
	in (h, m)


{- Interface -}

stmToInterface :: D.Class -> C.FileStm
stmToInterface (D.Class {D.className = name, D.classExtends = extends, D.classDefs = defs, D.classConstructor = constr}) =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = maybe "NSObject" (D.className . D.extendsClass) extends,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs,
		C.interfaceFuns = [createFun name constr, initFun constr]
			++ intefaceFuns defs ++ staticGetters
	}
	where 
		staticGetters = (map staticGetterFun .filter (\f -> D.isField f && D.isStatic f)) defs

staticGetterFun :: D.Def -> C.Fun
staticGetterFun D.Field{D.defName = name, D.defType = tp} = C.Fun C.ObjectFun (showDataType tp) name []

		
needProperty :: D.Def -> Bool
needProperty v = D.DefModPrivate `notElem` D.defMods v && D.isField v && not (D.isStatic v)


fieldToProperty :: D.Def -> C.Property
fieldToProperty (D.Field {D.defName = name, D.defMods = mods, D.defType = tp, D.fieldAccs = accs}) = C.Property {
	C.propertyName = name,
	C.propertyType = showDataType tp,
	C.propertyModifiers = if D.DefModMutable `elem` mods && not isPrivateWrite
		then C.NonAtomic : mutModes tp ++ weak
		else [C.NonAtomic, C.ReadOnly] ++ weak
}
	where
		weak = if D.DefModWeak `elem` mods then [C.Weak] else []
		mutModes D.TPArr{} = [C.ReadOnly]
		mutModes (D.TPClass D.TPMClass _ _) = [C.Retain]
		mutModes (D.TPClass D.TPMEnum _ _) = [C.Retain]
		mutModes _ = []
		isPrivateWrite = (any (\(D.FieldAccWrite mmm _) -> D.FieldAccModPrivate `elem` mmm). filter isWriteAcc) accs
		isWriteAcc D.FieldAccWrite{} = True
		isWriteAcc _ = False

initFun :: D.Constructor -> C.Fun
initFun [] = C.Fun C.InstanceFun "id" "init" []
initFun decls = C.Fun C.InstanceFun "id" "initWith" (map (funPar . fst) decls)

funPar :: D.Def -> C.FunPar
funPar D.Field {D.defName = name, D.defType = dataType} = C.FunPar name (showDataType dataType) name

createFun :: String -> D.Constructor -> C.Fun
createFun clsName [] = C.Fun C.ObjectFun "id" (createFunName clsName) []
createFun clsName decls = C.Fun C.ObjectFun "id" (createFunName clsName ++ "With") (map (funPar . fst) decls)

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
stmToImpl cl@D.Class {D.className = clsName, D.classDefs = defs, D.classConstructor = constr} =
	C.Implementation {
		C.implName = clsName,
		C.implFields = map implField implFields,
		C.implSynthesizes = (map synthesize . filter needProperty) implFields,
		C.implFuns = [implCreate cl constr, implInit cl constr] ++ dealoc cl ++ implFuns defs ++ staticGetters,
		C.implStaticFields = map implField staticFields
	}
	where
		implFields = filter needField defs
		needField f@D.Field{D.fieldAccs = accs} = (not $ any realReadAcc accs) && not (isStaticField f)
		needField _ = False
		isStaticField f = D.isStatic f && D.isField f
		realReadAcc (D.FieldAccRead _ D.Nop) = False
		realReadAcc (D.FieldAccRead _ _) = True
		realReadAcc _ = False
		staticFields = filter isStaticField defs
		staticGetters = (map staticGetter . filter((D.DefModPrivate `notElem`) . D.defMods)) staticFields
		staticGetter f@D.Field{D.defName = name} = C.ImplFun (staticGetterFun f) [C.Return $ C.Ref $ '_' : name]

synthesize :: D.Def -> C.ImplSynthesize
synthesize D.Field{D.defName = x} = C.ImplSynthesize x ('_' : x)
implField :: D.Def -> C.ImplField
implField D.Field{D.defName = x, D.defType = tp, D.defMods = mods} = C.ImplField ('_' : x) (showDataType tp) ["__weak" | D.DefModWeak `elem` mods]

implCreate :: D.Class -> D.Constructor -> C.ImplFun
implCreate cl constr = let 
		clsName = D.className cl
		pars D.Field{D.defName = name} = (name, C.Ref name)
	in C.ImplFun (createFun clsName constr) [C.Return $
		autorelease $ C.Call (C.Call (C.Ref clsName) "alloc" []) (if null constr then "init" else "initWith") (map (pars . fst) constr)
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
	| otherwise = [C.ImplFun (C.Fun C.InstanceFun "void" "dealloc" []) $ mapMaybe releaseField (D.classDefs cl) ++ [C.Stm $C.Call C.Super "dealloc" []]]
	where 
		releaseField D.Field {D.defName = name, D.defType = D.TPClass{}} = Just $ C.Stm $ C.Call (C.Ref $ '_' : name) "release" []
		releaseField _ = Nothing
		
		

implInit :: D.Class -> D.Constructor -> C.ImplFun
implInit cl constr  = C.ImplFun (initFun constr) (
			[C.Set Nothing C.Self (C.Call C.Super "init" [])]
			++ implInitFields constr (filter hasInit (D.classDefs cl))
			++ [C.Stm C.Nop, C.Return C.Self]
			)
	where
		hasInit D.Field{D.defBody = D.Nop} = False
		hasInit D.Field{} = True
		hasInit _ = False

		implInitFields [] [] = []
		implInitFields co fields = [C.If C.Self (map (implConstrField . fst) co ++ map implInitField fields) []]
		implConstrField D.Field {D.defName = name, D.defType = tp} = C.Set Nothing (C.Ref $ '_' : name) (implRight tp) 
			where
				implRight D.TPClass{} = retain $ C.Ref name
				implRight _ = C.Ref name
		implInitField D.Field {D.defName = name, D.defBody = def} = C.Set Nothing (C.Ref $ '_' : name) (tExp def)
		
implFuns :: [D.Def] -> [C.ImplFun]
implFuns defs = (map stm2ImplFun . filter D.isDef) defs ++ (concatMap accs' . filter D.isField) defs
	where
		stm2ImplFun def@D.Def {D.defBody = db, D.defMods = mods, D.defType = tp} 
			| D.DefModAbstract `elem` mods = C.ImplFun (stm2Fun def) [C.Throw $ C.StringConst $ "Method " ++ D.defName def ++ " is abstract"]
			| otherwise = C.ImplFun (stm2Fun def) (tStm tp db)
		accs' :: D.Def -> [C.ImplFun]
		accs' D.Field{D.defName = name, D.defType = tp, D.fieldAccs = accs} = mapMaybe (acc' name tp) accs
		acc' _ _ (D.FieldAccRead _ D.Nop) = Nothing
		acc' _ _ (D.FieldAccWrite _ D.Nop) = Nothing
		acc' name tp (D.FieldAccRead _ e) = Just $ C.ImplFun 
			(C.Fun C.InstanceFun (showDataType tp) name [])
			(tStm tp e)
		acc' name tp (D.FieldAccWrite _ e) = Just $ C.ImplFun 
			(C.Fun C.InstanceFun "void" "set" [C.FunPar name (showDataType tp) name])
			(tStm D.TPVoid e)

		
{- Struct -}
genStruct :: D.Class -> [C.FileStm]
genStruct D.Class {D.className = name, D.classDefs = defs} = [C.Struct name fields, C.TypeDefStruct name name, con, eq, C.EmptyLine]
	where
		fields = map toField defs
		toField D.Field{D.defName = n, D.defType = tp} = C.StructField (showDataType tp) n
		con = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = name, C.cfunName = name ++ "Make", C.cfunPars = map toPar defs, C.cfunExps = 
			[C.Var name "ret" C.Nop] ++
			map toSet defs ++
			[C.Return $ C.Ref "ret"]
		}
		eq = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = "BOOL", C.cfunName = name ++ "Eq", C.cfunPars = [C.CFunPar name "s1", C.CFunPar name "s2"], C.cfunExps = 
			[C.Return $ foldl foldEq C.Nop defs]
		}
		toPar D.Field{D.defName = n, D.defType = tp} = C.CFunPar (showDataType tp) n
		toSet D.Field{D.defName = n} = C.Set Nothing (C.Dot (C.Ref "ret") n) (C.Ref n)
		foldEq :: C.Exp -> D.Def -> C.Exp
		foldEq C.Nop d = eqd d
		foldEq p d = C.BoolOp And p (eqd d)
		eqd :: D.Def -> C.Exp
		eqd D.Field{D.defName = n, D.defType = tp} = equals True (tp, C.Dot (C.Ref "s1") n) (tp, C.Dot (C.Ref "s2") n)
{- Enum -}

enumAdditionalDefs :: [D.Def]
enumAdditionalDefs = [D.Field "ordinal" D.TPInt D.Nop [] [], D.Field "name" D.TPString D.Nop [] []]

enumConst :: D.Constructor -> D.Constructor
enumConst cst = map (\f -> (f, D.Nop)) enumAdditionalDefs ++ cst

enumValuesFun :: C.Fun
enumValuesFun = C.Fun C.ObjectFun "NSArray*" "values" []

genEnumInterface :: D.Class -> [C.FileStm]
genEnumInterface D.Enum {D.className = name, D.classExtends = extends, D.classDefs = defs, D.enumItems = items } = [
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = maybe "NSObject" (D.className . D.extendsClass) extends,
		C.interfaceProperties = [C.Property "name" "NSString*" [C.NonAtomic, C.ReadOnly],
			C.Property "ordinal" "NSInteger" [C.NonAtomic, C.ReadOnly]
			] ++ (map fieldToProperty . filter D.isField) defs,
		C.interfaceFuns = intefaceFuns (enumAdditionalDefs ++ defs) ++ map (enumItemGetterFun name) items ++ [enumValuesFun]
	}]
	
enumItemGetterFun :: String -> D.EnumItem -> C.Fun
enumItemGetterFun name (D.EnumItem itemName _) = C.Fun C.ObjectFun (name ++ "*") itemName []

genEnumImpl :: D.Class -> [C.FileStm]
genEnumImpl cl@D.Enum {D.className = clsName, D.enumItems = items} = [
	C.Implementation {
		C.implName = clsName,
		C.implFields = (map implField . filter D.isField) defs,
		C.implSynthesizes = (map synthesize . filter D.isField) defs,
		C.implFuns = [implCreate cl constr, implInit cl constr, initialize] ++ dealoc cl ++ implFuns defs ++ map itemGetter items ++ [valuesFun],
		C.implStaticFields = map stField items ++ [C.ImplField "values" "NSArray*" []]
	}]
	where
		defs = enumAdditionalDefs ++ D.classDefs cl
		constr = enumConst (D.classConstructor cl)
		stField (D.EnumItem itemName _) = C.ImplField itemName (clsName ++ "*") []
		itemGetter e@(D.EnumItem itemName _) = C.ImplFun (enumItemGetterFun clsName e) [C.Return $ C.Ref itemName]
		initialize = C.ImplFun (C.Fun C.ObjectFun "void" "initialize" []) (
			((C.Stm $ C.Call C.Super "initialize" []) : snd ( mapAccumL initItem 0 items)) ++ [setValues])
		initItem :: Int -> D.EnumItem -> (Int, C.Stm)
		initItem n (D.EnumItem itemName pars) = (n + 1, C.Set Nothing (C.Ref itemName) $ retain $ C.Call (C.Ref clsName) (createFunName clsName ++ "With") ([
			("ordinal", C.IntConst n),
			("name", C.StringConst itemName)] ++ map initPar pars) )
		initPar (D.Field{D.defName = fname}, e) = (fname, tExp e)
		valuesFun = C.ImplFun enumValuesFun [C.Return $ C.Ref "values"]
		setValues :: C.Stm
		setValues = C.Set Nothing (C.Ref "values") (C.Arr $ map (C.Ref . D.enumFieldName) items)


{- Imports -}
procImports :: D.File -> ([C.FileStm], [C.FileStm])
procImports D.File{D.fileImports = imps} = (h, m)
	where 
		filePossibleWeakImport D.File{D.fileCImports = [], D.fileClasses = cls} = all classPosibleWeakImport cls
		filePossibleWeakImport _ = False
		classPosibleWeakImport D.Class{D.classMods = mods} = not $ D.ClassModStruct `elem` mods
		classPosibleWeakImport D.Enum{} = True
		classPosibleWeakImport _ = False
		cImport D.File{D.fileName = fn} = C.Import (fn ++ ".h")
		h = concatMap procH imps
		procH file
			| filePossibleWeakImport file = map (C.ClassDecl . D.className) $ D.fileClasses file
			| otherwise = [cImport file]
		m = (map cImport . filter filePossibleWeakImport) imps

{- DataType -}
showDataType :: D.DataType -> String
showDataType D.TPArr{} = "NSArray*"
showDataType D.TPMap{} = "NSDictionary*"
showDataType D.TPInt = "NSInteger"
showDataType D.TPUInt = "NSUInteger"
showDataType D.TPFloat = "CGFloat"
showDataType D.TPString = "NSString*"
showDataType D.TPBool = "BOOL"
showDataType (D.TPClass D.TPMStruct _ c) = D.className c
showDataType (D.TPClass D.TPMClass _ c) = D.className c ++ "*"
showDataType (D.TPClass D.TPMEnum _ c) = D.className c ++ "*"
showDataType (D.TPClass _ _ _) = "id"
showDataType (D.TPSelf) = "id"
showDataType (D.TPOption c) = showDataType c
showDataType (D.TPFun D.TPVoid d) = showDataType d ++ "(^)" ++ "()"
showDataType (D.TPFun s d) = showDataType d ++ "(^)" ++ "(" ++ showDataType s ++ ")"
showDataType (D.TPGenericWrap (D.TPClass D.TPMStruct _ _)) = "id"
showDataType (D.TPGenericWrap D.TPInt) = "id"
showDataType (D.TPGenericWrap D.TPUInt) = "id"
showDataType (D.TPGenericWrap D.TPFloat) = "id"
showDataType (D.TPGenericWrap D.TPBool) = "id"
showDataType (D.TPGenericWrap c) = showDataType c
showDataType tp = show tp

{- Exp -}
tPars :: [(D.Def, D.Exp)] -> [(String, C.Exp)]
tPars = map (\(d, e) -> (D.defName d, maybeVal (D.exprDataType e, D.defType d) $ tExp e))

tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i
tExp (D.StringConst i) = C.StringConst i
tExp (D.Nil) = C.Nil
tExp (D.BoolConst i) = C.BoolConst i
tExp (D.FloatConst i) = C.FloatConst i
tExp (D.BoolOp Eq l r) = equals True (D.exprDataType l, tExp l) (D.exprDataType r, tExp r) 
tExp (D.BoolOp NotEq l r) = equals False (D.exprDataType l, tExp l) (D.exprDataType r, tExp r) 
tExp (D.BoolOp t l r) = C.BoolOp t (tExp l) (tExp r)
tExp (D.MathOp t l r) = let 
		l' = tExp l
		r' = tExp r
		ltp = D.exprDataType l
		{-rtp = D.exprDataType r-}
	in case ltp of
		D.TPArr _ -> addObjectToArray l' r'
		D.TPMap _ _ -> addKVToMap l' r
		_ -> C.MathOp t (tExp l) (tExp r)
tExp (D.PlusPlus e) = C.PlusPlus (tExp e)
tExp (D.MinusMinus e) = C.MinusMinus (tExp e)


tExp (D.Dot (D.Self _) (D.Call D.Field {D.defName = r} _ [])) = C.Ref $ '_' : r
tExp (D.Dot l (D.Call D.Field {D.defName = r} _ [])) = C.Dot (tExp l) r
tExp (D.Dot l (D.Call D.Def{D.defName = name} _ pars)) = C.Call (tExp l) name (tPars pars)

tExp (D.Self _) = C.Self
tExp (D.Call D.Field {D.defName = r} _ []) = C.Ref $ '_' : r
tExp (D.Call D.Def{D.defName = name, D.defMods = mods} _ pars)
	| D.DefModLocal `elem` mods && null pars = C.Ref name
	| D.DefModConstructor `elem` mods = C.Call 
		(C.Ref name) 
		(createFunName $ name ++ if null pars then "" else "With") 
		(tPars pars)
	| D.DefModStructConstructor `elem` mods = C.CCall 
		(name++ "Make")
		(map (tExp . snd) pars)
	| D.DefModEnumList `elem` mods = C.Call (C.Ref name) "values" []
	| D.DefModVal `elem` mods = C.Ref name
	| otherwise = C.CCall name (map (tExp . snd) pars)
tExp (D.If cond t f) = C.InlineIf (tExp cond) (tExp t) (tExp f)
tExp (D.Index e i) = case D.exprDataType e of
	D.TPMap _ _ -> C.Call (tExp e) "optionObjectFor" [("key", tExp i)]
	_ -> C.Index (tExp e) (tExp i)
tExp (D.Lambda pars e rtp) = 
	let 
		isNeedUnwrap :: D.DataType -> Bool
		isNeedUnwrap (D.TPGenericWrap (D.TPClass D.TPMStruct _ _)) = True
		isNeedUnwrap (D.TPGenericWrap D.TPInt) = True
		isNeedUnwrap (D.TPGenericWrap D.TPBool) = True
		isNeedUnwrap (D.TPGenericWrap D.TPFloat) = True
		isNeedUnwrap (D.TPGenericWrap D.TPUInt) = True
		isNeedUnwrap _ = False
		par' (name, tp) 
			| isNeedUnwrap tp = (name ++ "_", "id")
			| otherwise = (name, showDataType tp)
		unwrapPars :: [C.Stm]
		unwrapPars = (map unwrapPar. filter (isNeedUnwrap . snd)) pars
		unwrapPar ::(String, D.DataType) -> C.Stm
		unwrapPar (name, D.TPGenericWrap tp) = C.Var (showDataType tp) name (maybeVal (D.TPGenericWrap tp, tp) $ C.Ref $ name ++ "_")
	in
	C.Lambda (map (par') pars) (unwrapPars ++ tStm rtp e) (showDataType rtp)
tExp (D.Arr exps) = C.Arr $ map (tExpToType tpGeneric) exps
tExp (D.Map exps) = C.Map $ map (tExpToType tpGeneric *** tExpToType tpGeneric) exps
tExp (D.Tuple exps) = C.CCall "tuple" $ map (tExpToType tpGeneric) exps
tExp (D.Opt e) = let tp = D.exprDataType e
	in C.Call (C.Ref "CNOption") "opt" [("", maybeVal (tp, D.TPGenericWrap tp) (tExp e))]
tExp (D.None _) = C.Call (C.Ref "CNOption") "none" []


tExp e@(D.Error _ _) = error$ show e
tExp x = error $ "No tExp for " ++ show x

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
		D.TPArr _ -> [C.Set Nothing l' (addObjectToArray l' r')]
		D.TPMap _ _ -> [C.Set Nothing l' (addKVToMap l' r)]
		_ -> [C.Set (Just t) l' (maybeVal (rtp, ltp) r')]
tStm _ (D.Set tp l r) = [C.Set tp (tExp l) (maybeVal (D.exprDataType r, D.exprDataType l) (tExp r))]
tStm D.TPVoid (D.Return e) = [C.Stm $ tExp e]
tStm tp (D.Return e) = [C.Return $ (tExpToType tp e)]
tStm _ (D.Val D.Def{D.defName = name, D.defType = tp, D.defBody = e}) = [C.Var (showDataType tp) name (tExpToType tp e)]
tStm _ x = [C.Stm $ tExp x]

equals :: Bool -> (D.DataType, C.Exp) -> (D.DataType, C.Exp) -> C.Exp
equals False s1@(D.TPClass{}, _) s2@(D.TPClass{}, _) = C.Not $ equals True s1 s2
equals True (D.TPClass D.TPMStruct _ c, e1) (_, e2) = C.CCall (D.className c ++ "Eq") [e1, e2]
equals True (D.TPClass _ _ _, e1) (D.TPClass _ _ _, e2) = C.Call e1 "isEqual" [("", e2)]
equals True (_, e1) (_, e2) = C.BoolOp Eq e1 e2
equals False (_, e1) (_, e2) = C.BoolOp NotEq e1 e2

addObjectToArray :: C.Exp -> C.Exp -> C.Exp
addObjectToArray a obj = C.Call a "arrayByAdding" [("object", obj)]

addKVToMap :: C.Exp -> D.Exp -> C.Exp
addKVToMap a (D.Tuple [k, v]) = C.Call a "dictionaryByAdding" [("value", tExp v), ("forKey", tExp k)]


data MaybeValTP = TPGen | TPNum | TPStruct | TPNoMatter | TPBool
maybeVal :: (D.DataType, D.DataType) -> C.Exp -> C.Exp
maybeVal (stp, dtp) e = let 
	tp D.TPGenericWrap{} = TPGen
	tp (D.TPClass D.TPMGeneric _ _) = TPGen
	tp (D.TPClass D.TPMStruct _ _) = TPStruct
	tp D.TPInt{} = TPNum
	tp D.TPUInt{} = TPNum
	tp D.TPFloat{} = TPNum
	tp D.TPBool{} = TPBool
	tp _ = TPNoMatter
	in case (tp stp, tp dtp) of
		(TPStruct, TPGen) -> C.CCall "val" [e]
		(TPGen, TPStruct) -> C.CCall "uval" [C.Ref $ D.className $ D.tpClass dtp, e]
		(TPNum, TPGen) -> case e of
			C.IntConst _ -> C.ObjCConst e
			C.FloatConst _ -> C.ObjCConst e
			_ -> C.CCall "numi" [e]
		(TPGen, TPNum) -> C.CCall "unumi" [e]
		(TPBool, TPGen) -> case e of
			C.BoolConst _ -> C.ObjCConst e
			_ -> C.CCall "numb" [e]
		(TPGen, TPBool) -> C.CCall "unumb" [e]
		_ -> e
