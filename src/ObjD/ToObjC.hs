module ObjD.ToObjC (
	fieldToProperty,
	stmToInterface,
	stmToImpl,
	toObjC
) where

import           Control.Arrow
import           Data.Char
import qualified ObjC.Struct   as C
import qualified ObjD.Index    as I
import qualified ObjD.Struct   as D


toObjC :: I.File -> [D.FileStm] -> ([C.FileStm], [C.FileStm])
toObjC idx stms = (map(stmToInterface idx) stms, map(stmToImpl idx) stms)

{- Interface -}

stmToInterface :: I.File -> D.FileStm -> C.FileStm
stmToInterface idx (D.Class {D.className = name, D.classFields = fields, D.classExtends = extends, D.classBody = body}) =
	C.Interface {
		C.interfaceName = name,
		C.interfaceExtends = case extends of
			D.ExtendsNone -> "NSObject"
			D.Extends e -> e,
		C.interfaceProperties = map (fieldToProperty cls) fields ++ bodyProps body,
		C.interfaceFuns = [createFun name fields, initFun fields]
			++ intefaceFuns cls body
	}
	where
		cls = I.getClass name idx
		bodyProps = map (fieldToProperty cls) . bodyDecls



fieldToProperty :: I.Class -> D.Decl -> C.Property
fieldToProperty idx (D.Decl {D.declName = name, D.declMutableType = mut}) = C.Property {
	C.propertyName = name,
	C.propertyType = show (I.getFieldType name idx),
	C.propertyModifiers = case mut of
		D.Val -> [C.ReadOnly, C.NonAtomic]
		D.Var -> [C.NonAtomic]
}

initFun :: [D.Decl] -> C.Fun
initFun [] = C.Fun C.InstanceFun "id" "init" []
initFun decls = C.Fun C.InstanceFun "id" "initWith" (map funPar decls)

funPar :: D.Decl -> C.FunPar
funPar D.Decl {D.declName = name, D.declDataType = dataType} = C.FunPar name (show dataType) name

createFun :: String -> [D.Decl] -> C.Fun
createFun clsName [] = C.Fun C.ObjectFun "id" (createFunName clsName) []
createFun clsName decls = C.Fun C.ObjectFun "id" (createFunName clsName ++ "With") (map funPar decls)

createFunName :: String -> String
createFunName (x1:x2:xs)
	| isUpper x1 && isUpper x2  =  createFunName(x2 : xs)
	| isUpper x1 = toLower x1 : x2 : xs
	| otherwise = x1 : x2 : xs


bodyDecls :: [D.ClassStm] -> [D.Decl]
bodyDecls = map toDecl . filter isDecl
	where
		isDecl (D.DeclStm _ ) = True
		isDecl _ = False
		toDecl (D.DeclStm d) = d


intefaceFuns :: I.Class -> [D.ClassStm] -> [C.Fun]
intefaceFuns idx = map (stm2Fun idx) . filter D.isDef

stm2Fun :: I.Class ->  D.ClassStm -> C.Fun
stm2Fun idx def@D.Def{D.defName = name, D.defPars = pars} =
	C.Fun {C.funType = C.InstanceFun, C.funReturnType = show (I.getFieldType (D.stmName def) idx), C.funName = name, C.funPars = map par pars}
	where
		par (D.Par nm tp) = C.FunPar nm tp nm

{- Implementation -}

stmToImpl :: I.File -> D.FileStm -> C.FileStm
stmToImpl idx (D.Class {D.className = clsName, D.classFields = fields, D.classBody = body}) =
	C.Implementation {
		C.implName = clsName,
		C.implFields = [],
		C.implSynthenyzes = map synthenize (fields ++ bodyDecls body),
		C.implFuns = [implCreate] ++ [implInit] ++ implFuns body
	}
	where
		cls = I.getClass clsName idx
		synthenize D.Decl{D.declName = x} = C.ImplSynthenyze x ('_' : x)
		implInit = C.ImplFun (initFun  fields) (
			[C.Set C.Self (C.Call C.Super "init" [])]
			++ implInitFields ( fields ++ bodyDecls body)
			++ [C.Nop, C.Return C.Self]
			)

		implInitFields :: [D.Decl] -> [C.Stm]
		implInitFields [] = []
		implInitFields decls = [C.If C.Self (map implInitField decls) []]
		implInitField D.Decl {D.declName = name, D.declDef = D.Nop} = C.Set (C.Ref $ '_' : name) (C.Ref name)
		implInitField D.Decl {D.declName = name, D.declDef = def} = C.Set (C.Ref $ '_' : name) (tExp def)

		implCreate = C.ImplFun (createFun clsName  fields) [C.Return
				(C.Call
					(C.Call
						(C.Call (C.Ref clsName) "alloc" [])
						(if null fields then "init" else "initWith")
						(map pars fields)
					)
					"autorelease"
					[]
				)
			]
		pars D.Decl{D.declName = name} = (name, C.Ref name)
		implFuns = map stm2ImplFun . filter D.isDef
		stm2ImplFun def@D.Def {D.defBody = db} = C.ImplFun (stm2Fun cls def) (tStm db)
{- Exp -}
tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i
tExp (D.Eq l r) = C.Eq (tExp l) (tExp r)
tExp (D.NotEq l r) = C.NotEq (tExp l) (tExp r)

tExp (D.Dot l (D.Ref r)) = C.Dot (tExp l) r
tExp (D.Dot l (D.Call name pars)) = C.Call (tExp l) name (map (second tExp) pars)

tExp (D.Self) = C.Self
tExp (D.Ref r) = C.Ref r

tExp x = error $ "No tExp for " ++ show x

tStm :: D.Exp -> [C.Stm]
tStm (D.Nop) = []

tStm (D.Braces []) = []
tStm (D.Braces [x]) = tStm x
tStm (D.Braces xs) = concatMap tStm xs

tStm (D.If cond t f) = [C.If (tExp cond) (tStm t) (tStm f)]

tStm (D.Set l r) = [C.Set (tExp l) (tExp r)]

tStm x = [C.Stm $ tExp x]
