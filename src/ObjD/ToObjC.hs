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
arc = False

toObjC :: D.File -> ([C.FileStm], [C.FileStm])
toObjC D.File{D.fileName = name, D.fileClasses = classes, D.fileCImports = cImports} = 
	let 
		cls = filter (\c -> D.isClass c && not (D.isStruct c)) classes
		structs = filter (\c -> D.isClass c && D.isStruct c) classes
		enums = filter D.isEnum classes
		imps = map toImport cImports
		toImport (D.CImportLib n) = C.ImportLib n
		toImport (D.CImportUser n) = C.Import n
	in ( [C.ImportLib "Foundation/Foundation.h"] ++ imps ++ [C.EmptyLine] ++ concatMap genStruct structs 
		++ concatMap genEnumInterface enums 
		++ map stmToInterface cls, 
		[C.Import (name ++ ".h") , C.EmptyLine] ++ concatMap genEnumImpl enums ++ map stmToImpl cls)


{- Interface -}

stmToInterface :: D.Class -> C.FileStm
stmToInterface (D.Class {D.className = name, D.classExtends = extends, D.classDefs = defs, D.classConstructor = constr}) =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = maybe "NSObject" D.className extends,
		C.interfaceProperties = (map fieldToProperty . filter needDef) defs,
		C.interfaceFuns = [createFun name constr, initFun constr]
			++ intefaceFuns defs
	}
	where
		needDef v = D.DefModPrivate `notElem` (D.defMods v) && D.isField v



fieldToProperty :: D.Def -> C.Property
fieldToProperty (D.Field {D.defName = name, D.defMods = mods, D.defType = tp}) = C.Property {
	C.propertyName = name,
	C.propertyType = showDataType tp,
	C.propertyModifiers = if D.DefModMutable `elem` mods && D.DefModPrivateWrite `notElem` mods 
		then C.NonAtomic : mutModes tp 
		else [C.NonAtomic, C.ReadOnly]
}
	where
		mutModes D.TPArr{} = [C.ReadOnly]
		mutModes D.TPClass{} = [C.Retain]
		mutModes _ = []

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
		par (D.Par nm ttp _) = C.FunPar nm (showDataType ttp) nm

{- Implementation -}

stmToImpl :: D.Class -> C.FileStm
stmToImpl cl@D.Class {D.className = clsName, D.classDefs = defs, D.classConstructor = constr} =
	C.Implementation {
		C.implName = clsName,
		C.implFields = (map implField . filter D.isField) defs,
		C.implSynthesizes = (map synthesize . filter D.isField) defs,
		C.implFuns = [implCreate cl constr, implInit cl constr] ++ dealoc cl ++ implFuns defs,
		C.implStaticFields = []
	}
	

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
autorelease :: C.Exp -> C.Exp
autorelease e 
	| arc = C.Call e "autorelease" []
	| otherwise = e

dealoc :: D.Class -> [C.ImplFun]
dealoc cl 
	| arc = [C.ImplFun (C.Fun C.InstanceFun "void" "dealloc" []) $ mapMaybe releaseField (D.classDefs cl) ++ [C.Stm $C.Call C.Super "dealloc" []]]
	| otherwise = []
	where 
		releaseField D.Field {D.defName = name, D.defType = D.TPClass{}} = Just $ C.Stm $ C.Call (C.Ref $ '_' : name) "release" []
		releaseField _ = Nothing
		
		

implInit :: D.Class -> D.Constructor -> C.ImplFun
implInit cl constr  = C.ImplFun (initFun constr) (
			[C.Set C.Self (C.Call C.Super "init" [])]
			++ implInitFields constr (filter hasInit (D.classDefs cl))
			++ [C.Stm C.Nop, C.Return C.Self]
			)
	where
		hasInit D.Field{D.defBody = D.Nop} = False
		hasInit D.Field{} = True
		hasInit _ = False

		implInitFields [] [] = []
		implInitFields co fields = [C.If C.Self (map (implConstrField . fst) co ++ map implInitField fields) []]
		implConstrField D.Field {D.defName = name, D.defType = tp} = C.Set (C.Ref $ '_' : name) (implRight tp) 
			where
				implRight D.TPClass{} = C.Call (C.Ref name) "retain" []
				implRight _ = C.Ref name
		implInitField D.Field {D.defName = name, D.defBody = def} = C.Set (C.Ref $ '_' : name) (tExp def)
		
implFuns :: [D.Def] -> [C.ImplFun]
implFuns = map stm2ImplFun . filter D.isDef
	where
		stm2ImplFun def@D.Def {D.defBody = db, D.defMods = mods, D.defType = tp} 
			| D.DefModAbstract `elem` mods = C.ImplFun (stm2Fun def) [C.Throw $ C.StringConst $ "Method " ++ D.defName def ++ " is abstact"]
			| otherwise = C.ImplFun (stm2Fun def) (tStm (D.isVoid tp) db)
		
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
		toSet D.Field{D.defName = n} = C.Set (C.Dot (C.Ref "ret") n) (C.Ref n)
{- Enum -}

enumAdditionalDefs :: [D.Def]
enumAdditionalDefs = [D.Field "ordinal" D.TPInt D.Nop [], D.Field "name" D.TPString D.Nop []]

enumConst :: D.Constructor -> D.Constructor
enumConst cst = map (\f -> (f, D.Nop)) enumAdditionalDefs ++ cst

genEnumInterface :: D.Class -> [C.FileStm]
genEnumInterface D.Enum {D.className = name, D.classExtends = extends, D.classDefs = defs, D.enumItems = items } = [
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = maybe "NSObject" D.className extends,
		C.interfaceProperties = [C.Property "name" "NSString*" [C.NonAtomic, C.ReadOnly],
			C.Property "ordinal" "NSInteger" [C.NonAtomic, C.ReadOnly]
			] ++ (map fieldToProperty . filter D.isField) defs,
		C.interfaceFuns = intefaceFuns (enumAdditionalDefs ++ defs) ++ map (enumItemGetterFun name) items
	}]
	
enumItemGetterFun :: String -> D.EnumItem -> C.Fun
enumItemGetterFun name (D.EnumItem itemName _) = C.Fun C.ObjectFun (name ++ "*") itemName []

genEnumImpl :: D.Class -> [C.FileStm]
genEnumImpl cl@D.Enum {D.className = clsName, D.enumItems = items} = [
	C.Implementation {
		C.implName = clsName,
		C.implFields = (map implField . filter D.isField) defs,
		C.implSynthesizes = (map synthesize . filter D.isField) defs,
		C.implFuns = [implCreate cl constr, implInit cl constr, initialize] ++ dealoc cl ++ implFuns defs ++ map itemGetter items,
		C.implStaticFields = map stField items
	}]
	where
		defs = enumAdditionalDefs ++ D.classDefs cl
		constr = enumConst (D.classConstructor cl)
		stField (D.EnumItem itemName _) = C.ImplField (clsName ++ "*") itemName
		itemGetter e@(D.EnumItem itemName _) = C.ImplFun (enumItemGetterFun clsName e) [C.Return $ C.Ref itemName]
		initialize = C.ImplFun (C.Fun C.ObjectFun "void" "initialize" []) (
			(C.Stm $ C.Call C.Super "initialize" []) : snd ( mapAccumL initItem 0 items))
		initItem :: Int -> D.EnumItem -> (Int, C.Stm)
		initItem n (D.EnumItem itemName pars) = (n + 1, C.Set (C.Ref itemName) $ retain $ C.Call (C.Ref clsName) (createFunName clsName ++ "With") ([
			("ordinal", C.IntConst n),
			("name", C.StringConst itemName)] ++ map initPar pars) )
		retain f 
			| arc = C.Call f "retain" []
			| otherwise = f
		initPar (D.Field{D.defName = fname}, e) = (fname, tExp e)
			


{- DataType -}
showDataType :: D.DataType -> String
showDataType D.TPArr{} = "NSArray*"
showDataType D.TPInt = "NSInteger"
showDataType D.TPFloat = "CGFloat"
showDataType D.TPString = "NSString*"
showDataType D.TPBool = "BOOL"
showDataType tp = show tp

{- Exp -}
tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i
tExp (D.BoolConst i) = C.BoolConst i
tExp (D.FloatConst a b) = C.FloatConst a b
tExp (D.Eq l r) = C.Eq (tExp l) (tExp r)
tExp (D.NotEq l r) = C.NotEq (tExp l) (tExp r)

tExp (D.Dot (D.Self _) (D.Call D.Field {D.defName = r} [])) = C.Ref $ '_' : r
tExp (D.Dot l (D.Call D.Field {D.defName = r} [])) = C.Dot (tExp l) r
tExp (D.Dot l (D.Call D.Def{D.defName = name} pars)) = C.Call (tExp l) name (map (first D.parName . second tExp) pars)

tExp (D.Self _) = C.Self
tExp (D.Call D.Field {D.defName = r} []) = C.Ref $ '_' : r
tExp (D.Call D.DefStub{D.defName = name} pars) = C.CCall name (map (tExp . snd) pars)
tExp (D.ParRef D.Par {D.parName = r}) = C.Ref r

tExp x = error $ "No tExp for " ++ show x

tStm :: Bool -> D.Exp -> [C.Stm]
tStm _ (D.Nop) = []

tStm _ (D.Braces []) = []
tStm v (D.Braces [x]) = tStm v x
tStm v (D.Braces xs) = concatMap (tStm v) xs

tStm v (D.If cond t f) = [C.If (tExp cond) (tStm v t) (tStm v f)]

tStm _ (D.Set l r) = [C.Set (tExp l) (tExp r)]
tStm False (D.Return e) = [C.Return $ tExp e]
tStm _ (D.Return e) = [C.Stm $ tExp e]
tStm _ x = [C.Stm $ tExp x]
