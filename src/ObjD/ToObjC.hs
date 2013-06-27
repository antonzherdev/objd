module ObjD.ToObjC (
	fieldToProperty,
	stmToInterface,
	stmToImpl,
	toObjC
) where

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
			++ concatMap genStruct structs 
			++ map genProtocol traits
			++ concatMap genEnumInterface enums 
			++ map stmToInterface cls

		enumsImpl = concatMap genEnumImpl enums
		stmsImpl = map stmToImpl cls
		m = if null enumsImpl && null stmsImpl then []
			else [C.Import (name ++ ".h") , C.EmptyLine] ++ snd dImports' ++ enumsImpl ++ stmsImpl
	in (h, m)


{- Interface -}

stmToInterface :: D.Class -> C.FileStm
stmToInterface (D.Class {D.className = name, D.classExtends = extends, D.classDefs = defs, D.classConstructor = constr}) =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = maybe "NSObject" D.className extends,
		C.interfaceProperties = (map fieldToProperty . filter needProperty) defs,
		C.interfaceFuns = [createFun name constr, initFun constr]
			++ intefaceFuns defs
	}
		
needProperty :: D.Def -> Bool
needProperty v = D.DefModPrivate `notElem` D.defMods v && D.isField v


fieldToProperty :: D.Def -> C.Property
fieldToProperty (D.Field {D.defName = name, D.defMods = mods, D.defType = tp, D.fieldAccs = accs}) = C.Property {
	C.propertyName = name,
	C.propertyType = showDataType tp,
	C.propertyModifiers = if D.DefModMutable `elem` mods && not isPrivateWrite
		then C.NonAtomic : mutModes tp 
		else [C.NonAtomic, C.ReadOnly]
}
	where
		mutModes D.TPArr{} = [C.ReadOnly]
		mutModes D.TPClass{} = [C.Retain]
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
intefaceFuns = map stm2Fun . filter D.isDef

stm2Fun :: D.Def -> C.Fun
stm2Fun D.Def{D.defName = name, D.defPars = pars, D.defType = tp, D.defMods = mods} =
	C.Fun {
		C.funType = if D.DefModStatic `elem` mods then C.ObjectFun else C.InstanceFun, 
		C.funReturnType = showDataType tp, 
		C.funName = name, 
		C.funPars = map par pars}
	where
		par (D.Def nm _ ttp _ _) = C.FunPar nm (showDataType ttp) nm

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
		C.implFuns = [implCreate cl constr, implInit cl constr] ++ dealoc cl ++ implFuns defs,
		C.implStaticFields = []
	}
	where
		implFields = filter needField defs
		needField D.Field{D.fieldAccs = accs} = not $ any realReadAcc accs
		needField _ = False
		realReadAcc (D.FieldAccRead _ D.Nop) = False
		realReadAcc (D.FieldAccRead _ _) = True
		realReadAcc _ = False
	

synthesize :: D.Def -> C.ImplSynthesize
synthesize D.Field{D.defName = x} = C.ImplSynthesize x ('_' : x)
implField :: D.Def -> C.ImplField
implField D.Field{D.defName = x, D.defType = tp} = C.ImplField ('_' : x) (showDataType tp)

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
			| D.DefModAbstract `elem` mods = C.ImplFun (stm2Fun def) [C.Throw $ C.StringConst $ "Method " ++ D.defName def ++ " is abstact"]
			| otherwise = C.ImplFun (stm2Fun def) (tStm (D.isVoid tp) db)
		accs' :: D.Def -> [C.ImplFun]
		accs' D.Field{D.defName = name, D.defType = tp, D.fieldAccs = accs} = mapMaybe (acc' name tp) accs
		acc' _ _ (D.FieldAccRead _ D.Nop) = Nothing
		acc' _ _ (D.FieldAccWrite _ D.Nop) = Nothing
		acc' name tp (D.FieldAccRead _ e) = Just $ C.ImplFun 
			(C.Fun C.InstanceFun (showDataType tp) name [])
			(tStm False e)
		acc' name tp (D.FieldAccWrite _ e) = Just $ C.ImplFun 
			(C.Fun C.InstanceFun "void" "set" [C.FunPar name (showDataType tp) name])
			(tStm True e)

		
{- Struct -}
genStruct :: D.Class -> [C.FileStm]
genStruct D.Class {D.className = name, D.classDefs = defs} = [C.Struct name fields, C.TypeDefStruct name name, con, C.EmptyLine]
	where
		fields = map toField defs
		toField D.Field{D.defName = n, D.defType = tp} = C.StructField (showDataType tp) n
		con = C.CFun{C.cfunMods = [C.CFunStatic, C.CFunInline], C.cfunReturnType = name, C.cfunName = name ++ "Make", C.cfunPars = map toPar defs, C.cfunExps = 
			[C.Var name "ret" C.Nop] ++
			map toSet defs ++
			[C.Return $ C.Ref "ret"]
		}
		toPar D.Field{D.defName = n, D.defType = tp} = C.CFunPar (showDataType tp) n
		toSet D.Field{D.defName = n} = C.Set Nothing (C.Dot (C.Ref "ret") n) (C.Ref n)
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
		C.interfaceExtends = maybe "NSObject" D.className extends,
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
		C.implStaticFields = map stField items ++ [C.ImplField "NSArray*" "values"]
	}]
	where
		defs = enumAdditionalDefs ++ D.classDefs cl
		constr = enumConst (D.classConstructor cl)
		stField (D.EnumItem itemName _) = C.ImplField (clsName ++ "*") itemName
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
procImports D.File{D.fileImports = imps} = (map to imps, [])
	where 
		to D.File{D.fileName = fn} = C.Import (fn ++ ".h")

{- DataType -}
showDataType :: D.DataType -> String
showDataType D.TPArr{} = "NSArray*"
showDataType D.TPInt = "NSInteger"
showDataType D.TPUInt = "NSUInteger"
showDataType D.TPFloat = "CGFloat"
showDataType D.TPString = "NSString*"
showDataType D.TPBool = "BOOL"
showDataType (D.TPTrait _) = "id"
showDataType (D.TPGeneric _) = "id"
showDataType (D.TPSelf) = "id"
showDataType (D.TPFun s d) = showDataType d ++ "(^)" ++ "(" ++ showDataType s ++ ")"
showDataType tp = show tp

{- Exp -}
tPars :: [(D.Def, D.Exp)] -> [(String, C.Exp)]
tPars = map (D.defName *** tExp)

tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i
tExp (D.Nil) = C.Nil
tExp (D.BoolConst i) = C.BoolConst i
tExp (D.FloatConst a b) = C.FloatConst a b
tExp (D.BoolOp t l r) = C.BoolOp t (tExp l) (tExp r)
tExp (D.MathOp t l r) = C.MathOp t (tExp l) (tExp r)
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
		(C.Ref $ name) 
		(createFunName $ name ++ if null pars then "" else "With") 
		(tPars pars)
	| D.DefModStructConstructor `elem` mods = C.CCall 
		(name++ "Make")
		(map (tExp . snd) pars)
	| D.DefModEnumList `elem` mods = C.Call (C.Ref $ name) "values" []
	| otherwise = C.CCall name (map (tExp . snd) pars)
tExp (D.If cond t f) = C.InlineIf (tExp cond) (tExp t) (tExp f)
tExp (D.Index e i) = C.Index (tExp e) (tExp i)

tExp x = error $ "No tExp for " ++ show x

tStm :: Bool -> D.Exp -> [C.Stm]
tStm _ (D.Nop) = []

tStm _ (D.Braces []) = []
tStm v (D.Braces [x]) = tStm v x
tStm v (D.Braces xs) = concatMap (tStm v) xs

tStm v (D.If cond t f) = [C.If (tExp cond) (tStm v t) (tStm v f)]

tStm _ (D.Set tp l r) = [C.Set tp (tExp l) (tExp r)]
tStm False (D.Return e) = [C.Return $ tExp e]
tStm _ (D.Return e) = [C.Stm $ tExp e]
tStm _ x = [C.Stm $ tExp x]
