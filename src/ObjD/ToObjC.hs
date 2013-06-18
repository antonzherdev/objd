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
toObjC D.File{D.fileClasses = classes} = 
	let cls = filter D.isClass classes
	in (map stmToInterface cls, map stmToImpl cls)


{- Interface -}

stmToInterface :: D.Class -> C.FileStm
stmToInterface (D.Class {D.className = name, D.classFields = fields, D.classExtends = extends, D.classDefs = defs, D.classConstructor = constr}) =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = maybe "NSObject" D.className extends,
		C.interfaceProperties = map fieldToProperty fields,
		C.interfaceFuns = [createFun name constr, initFun constr]
			++ intefaceFuns defs
	}



fieldToProperty :: D.Field -> C.Property
fieldToProperty (D.Field {D.fieldName = name, D.isFieldMutable = mut, D.fieldType = tp}) = C.Property {
	C.propertyName = name,
	C.propertyType = show tp,
	C.propertyModifiers = case mut of
		False -> [C.ReadOnly, C.NonAtomic]
		True -> [C.NonAtomic]
}

initFun :: D.Constructor -> C.Fun
initFun [] = C.Fun C.InstanceFun "id" "init" []
initFun decls = C.Fun C.InstanceFun "id" "initWith" (map (funPar . fst) decls)

funPar :: D.Field -> C.FunPar
funPar D.Field {D.fieldName = name, D.fieldType = dataType} = C.FunPar name (show dataType) name

createFun :: String -> D.Constructor -> C.Fun
createFun clsName [] = C.Fun C.ObjectFun "id" (createFunName clsName) []
createFun clsName decls = C.Fun C.ObjectFun "id" (createFunName clsName ++ "With") (map (funPar . fst) decls)

createFunName :: String -> String
createFunName (x1:x2:xs)
	| isUpper x1 && isUpper x2  =  createFunName(x2 : xs)
	| isUpper x1 = toLower x1 : x2 : xs
	| otherwise = x1 : x2 : xs


intefaceFuns :: [D.Def] -> [C.Fun]
intefaceFuns = map stm2Fun 

stm2Fun :: D.Def -> C.Fun
stm2Fun D.Def{D.defName = name, D.defPars = pars, D.defType = tp} =
	C.Fun {C.funType = C.InstanceFun, C.funReturnType = show tp, C.funName = name, C.funPars = map par pars}
	where
		par (D.Par nm tp _) = C.FunPar nm (show tp) nm

{- Implementation -}

stmToImpl :: D.Class -> C.FileStm
stmToImpl (D.Class {D.className = clsName, D.classFields = fields, D.classDefs = defs, D.classConstructor = constr}) =
	C.Implementation {
		C.implName = clsName,
		C.implFields = [],
		C.implSynthenyzes = map synthenize fields,
		C.implFuns = [implCreate] ++ [implInit] ++ implFuns defs
	}
	where
		synthenize D.Field{D.fieldName = x} = C.ImplSynthenyze x ('_' : x)
		implInit = C.ImplFun (initFun constr) (
			[C.Set C.Self (C.Call C.Super "init" [])]
			++ implInitFields constr (filter hasInit fields)
			++ [C.Nop, C.Return C.Self]
			)
		hasInit D.Field{D.fieldInit = D.Nop} = False
		hasInit _ = True

		implInitFields [] [] = []
		implInitFields co fields = [C.If C.Self (map (implConstrField . fst) co ++ map implInitField fields) []]
		implConstrField D.Field {D.fieldName = name} = C.Set (C.Ref $ '_' : name) (C.Ref name)
		implInitField D.Field {D.fieldName = name, D.fieldInit = def} = C.Set (C.Ref $ '_' : name) (tExp def)

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
		pars D.Field{D.fieldName = name} = (name, C.Ref name)
		implFuns = map stm2ImplFun
		stm2ImplFun def@D.Def {D.defBody = db} = C.ImplFun (stm2Fun def) (tStm db)
{- Exp -}
tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i
tExp (D.Eq l r) = C.Eq (tExp l) (tExp r)
tExp (D.NotEq l r) = C.NotEq (tExp l) (tExp r)

tExp (D.Dot D.Self (D.FieldRef D.Field {D.fieldName = r} )) = C.Ref $ '_' : r
tExp (D.Dot l (D.FieldRef D.Field {D.fieldName = r} )) = C.Dot (tExp l) r
tExp (D.Dot l (D.Call D.Def{D.defName = name} pars)) = C.Call (tExp l) name (map (first D.parName . second tExp) pars)

tExp (D.Self) = C.Self
tExp (D.FieldRef D.Field {D.fieldName = r}) = C.Ref $ '_' : r
tExp (D.ParRef D.Par {D.parName = r}) = C.Ref $ r

tExp x = error $ "No tExp for " ++ show x

tStm :: D.Exp -> [C.Stm]
tStm (D.Nop) = []

tStm (D.Braces []) = []
tStm (D.Braces [x]) = tStm x
tStm (D.Braces xs) = concatMap tStm xs

tStm (D.If cond t f) = [C.If (tExp cond) (tStm t) (tStm f)]

tStm (D.Set l r) = [C.Set (tExp l) (tExp r)]

tStm x = [C.Stm $ tExp x]
