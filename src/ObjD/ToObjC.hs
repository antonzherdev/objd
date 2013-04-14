module ObjD.ToObjC (
	fieldToProperty,
	stmToInterface,
	stmToImpl,
	toObjC
) where

import Data.Char
import qualified ObjC.Struct as C
import qualified ObjD.Struct as D

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
}


fieldToProperty :: D.Decl -> C.Property
fieldToProperty (D.Decl {D.declName = name, D.declDataType = dataType, D.declMutableType = mut}) = C.Property {
	C.propertyName = name,
	C.propertyType = dataType,
	C.propertyModifiers = case mut of 
		D.Val -> [C.ReadOnly, C.NonAtomic]
		D.Var -> [C.NonAtomic]
}

initFun :: [D.Decl] -> C.Fun
initFun [] = C.Fun C.InstanceFun "id" "init" []
initFun decls = C.Fun C.InstanceFun "id" "initWith" (map (funPar) decls)
funPar D.Decl {D.declName = name, D.declDataType = dataType} = C.FunPar name dataType name

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

{- Implementation -}

stmToImpl :: D.Statement -> C.Statement
stmToImpl (D.Class {D.className = name, D.classFields = fields, D.classBody = body}) = C.Implementation {
	C.implName = name,
	C.implFields = [],
	C.implSynthenyzes = map (synthenize) (fields ++ (bodyDecls body)),
	C.implFuns = 
		[implCreate name fields] ++
		[implInit fields body]
}

synthenize D.Decl{D.declName = x} = C.ImplSynthenyze x ("_" ++ x)

implInit decls body = C.ImplFun (initFun decls) (
	[C.Set "self" (C.Call C.Super "init" [])]
	++ implInitFields (decls ++ (bodyDecls body))
	++ [C.Nop, C.Return C.Self]
	)

implInitFields :: [D.Decl] -> [C.Stm]
implInitFields [] = []
implInitFields decls = [C.If C.Self (map (implInitField) decls) []]
implInitField D.Decl {D.declName = name, D.declDef = D.Nop} = C.Set ("_" ++ name) (C.Ref name)
implInitField D.Decl {D.declName = name, D.declDef = def} = C.Set ("_" ++ name) (tExp def)

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

{- Exp -}
tExp :: D.Exp -> C.Exp
tExp (D.IntConst i) = C.IntConst i