module ObjD.ToObjC (
	fieldToProperty,
	stmToInterface,
	stmToImpl,
	toObjC
) where

import           Control.Arrow
import           Data.Char
import qualified ObjC.Struct   as C
import qualified ObjD.Link   as D


toObjC :: D.File -> ([C.FileStm], [C.FileStm])
toObjC D.File{D.fileName = name, D.fileClasses = classes, D.fileCImports = cImports} = 
	let 
		cls = filter D.isClass classes
		imps = map toImport cImports
		toImport (D.CImportLib n) = C.ImportLib n
		toImport (D.CImportUser n) = C.Import n
	in ( [C.ImportLib "Foundation/Foundation.h"] ++ imps ++ [C.EmptyLine] ++ map stmToInterface cls, 
		[C.Import (name ++ ".h") , C.EmptyLine] ++ map stmToImpl cls)


{- Interface -}

stmToInterface :: D.Class -> C.FileStm
stmToInterface (D.Class {D.className = name, D.classExtends = extends, D.classDefs = defs, D.classConstructor = constr}) =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = maybe "NSObject" D.className extends,
		C.interfaceProperties = (map fieldToProperty . filter D.isField) defs,
		C.interfaceFuns = [createFun name constr, initFun constr]
			++ intefaceFuns defs
	}



fieldToProperty :: D.Def -> C.Property
fieldToProperty (D.Field {D.defName = name, D.isFieldMutable = mut, D.defType = tp}) = C.Property {
	C.propertyName = name,
	C.propertyType = showDataType tp,
	C.propertyModifiers = if mut then [C.NonAtomic] ++ (mutModes tp) else [C.ReadOnly, C.NonAtomic]
}
	where
		mutModes D.TPArr{} = [C.ReadOnly]
		mutModes D.TPClassRef{} = [C.Retain]
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
stm2Fun D.Def{D.defName = name, D.defPars = pars, D.defType = tp} =
	C.Fun {C.funType = C.InstanceFun, C.funReturnType = showDataType tp, C.funName = name, C.funPars = map par pars}
	where
		par (D.Par nm ttp _) = C.FunPar nm (showDataType ttp) nm

{- Implementation -}

stmToImpl :: D.Class -> C.FileStm
stmToImpl (D.Class {D.className = clsName, D.classDefs = defs, D.classConstructor = constr}) =
	C.Implementation {
		C.implName = clsName,
		C.implFields = (map implField . filter D.isField) defs,
		C.implSynthesizes = (map synthesize . filter D.isField) defs,
		C.implFuns = [implCreate] ++ [implInit] ++ implFuns defs
	}
	where
		synthesize D.Field{D.defName = x} = C.ImplSynthesize x ('_' : x)
		implField D.Field{D.defName = x, D.defType = tp} = C.ImplField ('_' : x) (showDataType tp)
		implInit = C.ImplFun (initFun constr) (
			[C.Set C.Self (C.Call C.Super "init" [])]
			++ implInitFields constr (filter hasInit defs)
			++ [C.Nop, C.Return C.Self]
			)
		hasInit D.Field{D.defBody = D.Nop} = False
		hasInit D.Field{} = True
		hasInit _ = False

		implInitFields [] [] = []
		implInitFields co fields = [C.If C.Self (map (implConstrField . fst) co ++ map implInitField fields) []]
		implConstrField D.Field {D.defName = name} = C.Set (C.Ref $ '_' : name) (C.Ref name)
		implInitField D.Field {D.defName = name, D.defBody = def} = C.Set (C.Ref $ '_' : name) (tExp def)

		implCreate = C.ImplFun (createFun clsName constr) [C.Return
				(C.Call
					(C.Call
						(C.Call (C.Ref clsName) "alloc" [])
						(if null constr then "init" else "initWith")
						(map (pars . fst) constr)
					)
					"autorelease"
					[]
				)
			]
		pars D.Field{D.defName = name} = (name, C.Ref name)
		implFuns = map stm2ImplFun . filter D.isDef
		stm2ImplFun def@D.Def {D.defBody = db} = C.ImplFun (stm2Fun def) (tStm db)

{- DataType -}
showDataType :: D.DataType -> String
showDataType D.TPArr{} = "NSArray*"
showDataType D.TPInt = "NSInteger"
showDataType D.TPFloat = "CGFloat"
showDataType tp = show tp

{- Exp -}
tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i
tExp (D.Eq l r) = C.Eq (tExp l) (tExp r)
tExp (D.NotEq l r) = C.NotEq (tExp l) (tExp r)

tExp (D.Dot D.Self (D.Call D.Field {D.defName = r} [])) = C.Ref $ '_' : r
tExp (D.Dot l (D.Call D.Field {D.defName = r} [])) = C.Dot (tExp l) r
tExp (D.Dot l (D.Call D.Def{D.defName = name} pars)) = C.Call (tExp l) name (map (first D.parName . second tExp) pars)

tExp (D.Self) = C.Self
tExp (D.Call D.Field {D.defName = r} []) = C.Ref $ '_' : r
tExp (D.Call D.DefStub{D.defName = name} pars) = C.CCall name (map (tExp . snd) pars)
tExp (D.ParRef D.Par {D.parName = r}) = C.Ref r

tExp x = error $ "No tExp for " ++ show x

tStm :: D.Exp -> [C.Stm]
tStm (D.Nop) = []

tStm (D.Braces []) = []
tStm (D.Braces [x]) = tStm x
tStm (D.Braces xs) = concatMap tStm xs

tStm (D.If cond t f) = [C.If (tExp cond) (tStm t) (tStm f)]

tStm (D.Set l r) = [C.Set (tExp l) (tExp r)]
tStm (D.Return e) = [C.Return $ tExp e]
tStm x = [C.Stm $ tExp x]
