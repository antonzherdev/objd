module ObjD.ToObjC (
	fieldToProperty,
	stmToInterface,
	stmToImpl,
	toObjC
) where

import Data.Char
import qualified ObjC.Struct as C
import qualified ObjD.Struct as D
import qualified ObjD.Text as D


toObjC :: [D.Statement] -> ([C.Statement], [C.Statement])
toObjC stms = (map(stmToInterface) stms, map(stmToImpl) stms)

{- Interface -}

stmToInterface :: D.Statement -> C.Statement
stmToInterface (D.Class {D.className = name, D.classFields = fields, D.extends = extends, D.classBody = body}) = C.Interface {
	C.interfaceName = name,
	C.extends = case extends of 
		D.ExtendsNone -> "NSObject"
		D.Extends e -> e,
	C.properties = map (fieldToProperty) fields ++ bodyProps body,
	C.funs = [createFun name fields, initFun fields]
		++ (intefaceFuns body)
}


fieldToProperty :: D.Decl -> C.Property
fieldToProperty (D.Decl {D.declName = name, D.declDataType = dataType, D.declMutableType = mut}) = C.Property {
	C.propertyName = name,
	C.propertyType = show dataType,
	C.propertyModifiers = case mut of 
		D.Val -> [C.ReadOnly, C.NonAtomic]
		D.Var -> [C.NonAtomic]
}

initFun :: [D.Decl] -> C.Fun
initFun [] = C.Fun C.InstanceFun "id" "init" []
initFun decls = C.Fun C.InstanceFun "id" "initWith" (map (funPar) decls)
funPar D.Decl {D.declName = name, D.declDataType = dataType} = C.FunPar name (show dataType) name

createFun :: String -> [D.Decl] -> C.Fun
createFun clsName [] = C.Fun C.ObjectFun "id" (createFunName clsName) []
createFun clsName decls = C.Fun C.ObjectFun "id" ((createFunName clsName) ++ "With") (map (funPar) decls)

createFunName :: String -> String
createFunName (x1:x2:xs) 
	| isUpper x1 && isUpper x2  =  createFunName(x2 : xs)
	| isUpper x1 = (toLower x1) : x2 : xs
	| otherwise = x1 : x2 : xs


bodyDecls = map toDecl . filter isDecl
	where 
		isDecl (D.DeclStm _ ) = True
		isDecl _ = False
		toDecl (D.DeclStm d) = d
bodyProps = map (fieldToProperty) . bodyDecls 

isDef (D.Def _ _ _ _) = True
isDef _ = False

intefaceFuns :: [D.Stm] -> [C.Fun]
intefaceFuns = map stm2Fun . filter isDef

stm2Fun :: D.Stm -> C.Fun
stm2Fun D.Def {D.defName = name, D.defPars = pars, D.defRetType = ret, D.defBody = body} = 
	C.Fun {C.funType = C.InstanceFun, C.funReturnType = show ret, C.funName = name, C.funPars = map par pars}
	where
		par (D.Par name tp) = C.FunPar name tp name

{- Implementation -}

stmToImpl :: D.Statement -> C.Statement
stmToImpl (D.Class {D.className = name, D.classFields = fields, D.classBody = body}) = C.Implementation {
	C.implName = name,
	C.implFields = [],
	C.implSynthenyzes = map (synthenize) (fields ++ (bodyDecls body)),
	C.implFuns = 
		[implCreate name fields] ++
		[implInit fields body] ++
		implFuns body
}

synthenize D.Decl{D.declName = x} = C.ImplSynthenyze x ("_" ++ x)

implInit decls body = C.ImplFun (initFun decls) (
	[C.Set C.Self (C.Call C.Super "init" [])]
	++ implInitFields (decls ++ (bodyDecls body))
	++ [C.Nop, C.Return C.Self]
	)

implInitFields :: [D.Decl] -> [C.Stm]
implInitFields [] = []
implInitFields decls = [C.If C.Self (map (implInitField) decls) []]
implInitField D.Decl {D.declName = name, D.declDef = D.Nop} = C.Set (C.Ref $ "_" ++ name) (C.Ref name)
implInitField D.Decl {D.declName = name, D.declDef = def} = C.Set (C.Ref $ "_" ++ name) (tExp def)

implCreate clsName decls = C.ImplFun (createFun clsName decls) [C.Return 
		(C.Call
			(C.Call
				(C.Call (C.Ref clsName) "alloc" [])
				(if null decls then "init" else "initWith")
				(map (pars) decls)
			)
			"autorelease"
			[]
		)
	]
	where 
		pars D.Decl{D.declName = name} = (name, C.Ref name)

implFuns = map stm2ImplFun . filter isDef

stm2ImplFun :: D.Stm -> C.ImplFun
stm2ImplFun def@D.Def {D.defBody = body} = 
	C.ImplFun (stm2Fun def) (tStm body)
{- Exp -}
tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i
tExp (D.Eq l r) = C.Eq (tExp l) (tExp r)
tExp (D.NotEq l r) = C.NotEq (tExp l) (tExp r)

tExp (D.Dot l (D.Ref r _)) = C.Dot (tExp l) r
tExp (D.Dot l (D.Call name pars)) = C.Call (tExp l) name (map (\(nm, e) -> (nm, tExp e)) pars)

tExp (D.Self) = C.Self
tExp (D.Ref r _) = C.Ref r

tExp x = error $ "No tExp for " ++ show x

tStm :: D.Exp -> [C.Stm]
tStm (D.Nop) = []

tStm (D.Braces []) = []
tStm (D.Braces [x]) = tStm x
tStm (D.Braces xs) = concatMap tStm xs

tStm (D.If cond t f) = [C.If (tExp cond) (tStm t) (tStm f)]

tStm (D.Set l r) = [C.Set (tExp l) (tExp r)]

tStm x = [C.Stm $ tExp x]
