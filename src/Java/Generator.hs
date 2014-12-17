module Java.Generator ( 
	toJava
)where

import Control.Monad.Writer
import Ex.String
import Data.Maybe
import Data.List
import qualified ObjD.Link.Struct as D
import qualified Java.Struct as J


toJava :: D.File -> [J.File]
toJava file@D.File{D.fileClasses = classes} =
	map (genFile file) $ filter (\cls -> not (D.isType cls) && not (D.isStub cls)) classes

genFile :: D.File -> D.Class -> J.File
genFile D.File{D.filePackage = D.Package{D.packageName = package}} cls =
	let 
		env = defaultEnv cls
		(cls', Wrt{wrtImports = clImps}) = runWriter $ genClass env
	in J.File {
		J.fileIsTest = envIsTest env,
		J.filePackage = package,
		J.fileImports = ["objd", "lang", "*"] : (filter (\p -> package /= init p && (["objd", "lang"] /= init p) ) $ nub clImps),
		J.fileClass = cls'}


genClass :: Env -> Writer Wrt J.Class
genClass env = do
	let 
		cl = envClass env
		isEnum = D.isEnum cl 
		defs = filter (\f -> not (D.isSpecial f) && not (D.isEnumItem f) && not (D.isInline f && D.isPrivate f)) 
			$ if D.isTrait cl then filter (not . D.isConstructor) (D.classDefs cl) else
				filter (D.isDefAbstract) (D.classDefs cl) ++ filter (not . D.isDefAbstract) (D.classDefsWithTraits cl)
		trMod D.ClassModAbstract = Just J.ClassModAbstract
		trMod D.ClassModFinal = Just J.ClassModFinal
		trMod _ = Nothing
		staticInitFields = filter (\ f -> D.isField f && not (D.isEnumItem f) && D.defBody f /= D.Nop && D.DefModStatic `elem` D.defMods f) (D.classDefs cl)
		genStaticSet d = do
			e' <- genExp env $ D.defBody d
			return $ J.Set Nothing (J.Ref $ D.defName d) e'
	defs' <- mapM (genDef env) defs
	gens' <- mapM (genGeneric env) $ D.classGenerics cl
	ext' <- case D.mainExtendsRef (D.classExtends cl) of
		Just e -> if D.isBaseClass (fst e) then return Nothing else fmap Just (genExtendsRef env e)
		Nothing -> return Nothing
	impls' <- mapM (genExtendsRef env) $ filter (not . D.isBaseClass . fst) $ D.traitExtendsRefs $ D.classExtends cl
	enumItems' <- mapM (genEnumItem env) (if isEnum then filter D.isEnumItem (D.classDefs cl) else [])
	staticInitFields' <- mapM genStaticSet staticInitFields
	return J.Class {
		J.classMods = [J.ClassModVisibility J.Public] ++ mapMaybe trMod (D.classMods cl),
		J.classType = if D.isTrait cl then J.ClassTypeInterface else if isEnum then J.ClassTypeEnum else J.ClassTypeClass,
		J.className = className cl,
		J.classGenerics = gens',
		J.classExtends = if isEnum then Nothing else ext',
		J.classImplements = impls',
		J.classEnumItems = enumItems',
		J.classDefs = join defs' ++ [J.StaticConstructor staticInitFields' | not (null staticInitFields')]
	}

genEnumItem :: Env -> D.Def -> Writer Wrt J.EnumItem
genEnumItem env D.Def{D.defName = name, D.defBody = D.Dot _ (D.Call _ _ (_:_:pars) _)} = do
	pars' <- (mapM (genExp env) . map snd) pars
	return $ J.EnumItem name pars'

genGeneric :: Env -> D.Class -> Writer Wrt J.Generic
genGeneric env cl = do
	exts <- mapM (genExtendsRef env) $ filter (not . D.isBaseClass . fst) $ D.extendsRefs $ D.classExtends cl
	return J.Generic {
		J.genericName = className cl,
		J.genericExtends = exts
	}

genExtendsRef :: Env -> D.ExtendsRef -> Writer Wrt J.TP
genExtendsRef env (cl, gens) = do
	tellImportClass cl
	gens' <- mapM (genTp env) gens
	return $ J.TPRef gens' (className cl) 

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Defs
 -----------------------------------------------------------------------------------------------------------------------------------------------}

defName :: D.Def -> String
defName d 
	| D.DefModDef `elem` D.defMods d = let 
		def = fromMaybe (D.defName d ++ concatMap (cap . D.defName) (D.defPars d)) $ D.genName (D.defAnnotations d)
		in case D.defPars d of
			[] -> case D.defName d of
				"hash" -> "hashCode"
				"dealloc" -> "finalize"
				"description" -> "toString"
				"toString" -> "_toString"
				"null" -> "nil"
				_ -> def
			[p] -> case (D.defName d, D.defName p) of
				("isEqual", "to") -> "equals"
				_ -> def
			_ -> def
	| otherwise = D.defName d

overrideAnnotation :: J.DefAnnotation
overrideAnnotation = J.DefAnnotation "Override" []

testAnnotation :: J.DefAnnotation
testAnnotation = J.DefAnnotation "Test" []

genDef :: Env -> D.Def -> Writer Wrt [J.Def]
genDef env d =
 	let 
 		isClTest = envIsTest env
 		cl = envClass env
 		genMod D.DefModPrivate = Just $ J.DefModVisability J.Private
 		genMod D.DefModProtected = Just $ J.DefModVisability J.Protected
 		genMod D.DefModPublic = Just $ J.DefModVisability J.Public
 		genMod D.DefModStatic = Just J.DefModStatic
 		genMod D.DefModAbstract = Just J.DefModAbstract
 		genMod _ = Nothing
 		constrSet dd = let nm = defName dd in J.Set Nothing (J.Dot J.This (J.Ref nm)) (J.Ref nm)
 		body = if isTrait then D.Nop else D.defBody d	
 		mods = mapMaybe genMod (D.defMods d)
 		name = defName d
 		isTrait = D.isTrait cl
 		genSet dd = do
			e' <- genExp env (D.defBody dd)
 			return $ J.Set Nothing (J.Dot J.This $ J.Ref (D.defName dd)) e'
 	in if D.isField d then 
 			do
	 			tp <- genTp env $ D.defType d
	 			return $ [J.Field {
		 				J.defAnnotations = [],
		 				J.defMods = mods ++ [J.DefModFinal | D.DefModMutable `notElem` D.defMods d] ++ [J.DefModVolatile | D.DefModVolatile `elem` D.defMods d],
		 				J.defName = name,
		 				J.defTp = tp,
		 				J.defExp = J.Nop
			 		} | not isTrait] ++ [J.Def {
		 				J.defAnnotations = [overrideAnnotation],
		 				J.defMods = mods,
		 				J.defGenerics = [],
		 				J.defName = name,
		 				J.defTp = tp,
		 				J.defPars = [],
		 				J.defThrows = [],
		 				J.defStms = [J.Return $ J.Ref name]
		 			}| D.DefModOverride `elem` D.defMods d]
 		else if D.isConstructor d then
 			do
 				let 
 					isEnum = D.isEnum cl
 					callSuperConstructor = if isEnum then return [] else case D.extendsClass $ D.classExtends cl of
			 			Just (D.ExtendsClass _ []) -> return []
			 			Just (D.ExtendsClass _ pars) -> do
			 				pars' <- mapM (genExp env . snd) pars
			 				return [J.Stm $ J.Call "super" [] pars']
			 			_ -> return []
 				super <- callSuperConstructor
 				let 
 					fields = filter (\ f -> 
	 					D.isField f && D.defBody f /= D.Nop && D.DefModConstructorField `notElem` D.defMods f && D.DefModStatic `notElem` D.defMods f) 
	 					(D.classDefsWithTraits cl)
 				sets <- mapM genSet fields
 				pars' <- mapM (genPar env) $ if isEnum then (tail $ tail $ D.defPars d) else D.defPars d
 				return [J.Constructor {
	 				J.defAnnotations = [],
	 				J.defMods = if isEnum then J.DefModVisability J.Private : filter (not . J.isDefModVisibility) mods else mods,
	 				J.defPars = pars',
	 				J.defStms = super ++ (map constrSet . filter ((D.DefModConstructorField `elem` ). D.defMods)) (D.classDefs cl) ++ sets
	 			}]
 		else 
 			do
 				tp <- genTp env $ D.defType d
 				stms' <- getStms env body
 				gens' <- mapM (genGeneric env) $ maybe [] D.defGenericsClasses $ D.defGenerics d
 				pars' <- mapM (genPar env) $ D.defPars d
 				let 
 					isTest = isClTest && (D.containsAnnotationWithClassName "objd.test.Test" $ D.defAnnotations d)
 					isFinalize = name == "finalize"
 				when(isTest) $ tellImport ["org", "junit", "Test"]
 				return [J.Def {
	 				J.defAnnotations = [overrideAnnotation| D.DefModOverride `elem` D.defMods d] ++ [testAnnotation | isTest],
	 				J.defMods = mods,
	 				J.defGenerics = gens',
	 				J.defName = name,
	 				J.defTp = tp,
	 				J.defPars = pars',
	 				J.defThrows = if isFinalize then ["Throwable"] else [],
	 				J.defStms = if isFinalize then (J.Stm $ J.Dot J.Super $ J.Call "finalize" [] []) : stms' else stms'
	 			} | not isTrait || (D.DefModPublic `elem` D.defMods d)]

genPar :: Env -> D.Def -> Writer Wrt J.DefPar
genPar env D.Def{D.defName = nm, D.defType = tp} = do
	tp' <- genTp env tp
	return ([J.DefModFinal], tp', nm)

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - DataType
 -----------------------------------------------------------------------------------------------------------------------------------------------}
genTp :: Env -> D.DataType -> Writer Wrt J.TP
genTp env (D.TPClass tp gens cl) 
	| className (envClass env) == className cl = do
		gens' <- mapM (genTp env) gens
		let fn = D.fullGenClassName cl
		return $ if fn == D.fullGenClassName (envClass env) then J.TPRef gens' (className cl) else J.TPRef gens' fn
	| otherwise = do
		when (tp /= D.TPMGeneric && tp /= D.TPMType) $ tellImportClass cl
		gens' <- mapM (genTp env) gens
		return $ J.TPRef gens' (className cl) 
genTp _ (D.TPNumber _ 1) = return $ J.tpRef "byte"
genTp _ (D.TPNumber _ 8) = return $ J.tpRef "long"
genTp _ (D.TPNumber _ _) = return $ J.tpRef "int"
genTp _ (D.TPGenericWrap _ (D.TPNumber _ 1)) = return $ J.tpRef "Byte"
genTp _ (D.TPGenericWrap _ (D.TPNumber _ 8)) = return $ J.tpRef "Long"
genTp _ (D.TPGenericWrap _ (D.TPNumber _ _)) = return $ J.tpRef "Integer"
genTp _ (D.TPFloatNumber 4) = return $ J.tpRef "float"
genTp _ (D.TPFloatNumber 8) = return $ J.tpRef "double"
genTp _ (D.TPFloatNumber _) = return $ J.tpRef "double"
genTp _ (D.TPGenericWrap _ (D.TPFloatNumber 4)) = return $ J.tpRef "Float"
genTp _ (D.TPGenericWrap _ (D.TPFloatNumber 8)) = return $ J.tpRef "Double"
genTp _ (D.TPGenericWrap _ (D.TPFloatNumber _)) = return $ J.tpRef "Float"
genTp _ D.TPVoid = return $ J.tpRef "void"
genTp _ (D.TPGenericWrap _ D.TPVoid) = return $ J.tpRef "Void"
genTp _ D.TPChar = return $ J.tpRef "char"
genTp _ (D.TPGenericWrap _ D.TPChar) = return $ J.tpRef "Character"
genTp _ D.TPBool = return $ J.tpRef "boolean"
genTp _ (D.TPGenericWrap _ D.TPBool) = return $ J.tpRef "Boolean"

genTp _ (D.TPFun [] D.TPVoid) = return $ J.TPRef [] "P0"
genTp env (D.TPFun [stp] D.TPVoid) = do 
	stp' <- genTp env $ D.wrapGeneric stp
	return $ J.TPRef [stp'] "P"
genTp env (D.TPFun stps D.TPVoid) = do
	stps' <- mapM (genTp env . D.wrapGeneric) stps
	return $ J.TPRef stps' ("P" ++ show (length stps))
genTp env (D.TPFun [] dtp) = do 
	dtp' <- genTp env $ D.wrapGeneric dtp
	return $ J.TPRef [dtp'] "F0"
genTp env (D.TPFun [stp] dtp) = do 
	stp' <- genTp env $ D.wrapGeneric stp
	dtp' <- genTp env $ D.wrapGeneric dtp
	return $ J.TPRef [stp', dtp'] "F"
genTp env (D.TPFun stps dtp) = do 
	stps' <- mapM (genTp env . D.wrapGeneric) stps
	dtp' <- genTp env $ D.wrapGeneric dtp
	return $ J.TPRef (stps'  ++ [dtp']) ("F" ++ show (length stps))

genTp env (D.TPGenericWrap _ w) = genTp env w
genTp _ D.TPString = return $ J.tpRef "String"
genTp env (D.TPEArr n tp)  = do
	tp' <- genTp env tp
	return $ J.TPArr tp' n
genTp _ D.TPAny = return $ J.tpRef "Object"
genTp env (D.TPArr _ tp) = do
	tellImport ["objd", "collection", "ImArray"]
	tp' <- genTp env tp
	return $ J.TPRef [tp'] "ImArray"
genTp env (D.TPTuple tps) = do
	tps' <- mapM (genTp env) tps
	return $ J.TPRef tps' (tupleClassName $ length tps)
genTp _ (D.TPSelf cl) = return $ J.TPRef (map (J.tpRef . className) (D.classGenerics cl) ) (className cl)
genTp env (D.TPMap key value) = do
	tellImport ["objd", "collection", "ImHashMap"]
	key' <- genTp env key
	value' <- genTp env value
	return $ J.TPRef [key', value'] "ImHashMap"
genTp env (D.TPOption _ tp) = genTp env tp
genTp _ D.TPAnyGeneric = return $ J.tpRef "Object"
genTp _ (D.TPPointer _) = return $ J.tpRef "Pointer"
genTp _ (D.TPNil) = return $ J.tpRef "Object"
genTp _ (D.TPUnknown e) = return $ J.TPUnknown e

genTp _ tp = error $ "genTp: " ++ show tp

tupleClassName :: Int -> String
tupleClassName 2 = "Tuple"
tupleClassName i = "Tuple" ++ show i
{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Stm
 -----------------------------------------------------------------------------------------------------------------------------------------------}

data Env = Env {envClass :: D.Class, envIsTest :: Bool, envInnerClass :: Bool}
defaultEnv :: D.Class -> Env
defaultEnv cls = Env{envClass = cls, envIsTest = D.containsAnnotationWithClassName "objd.test.Test" $ D.classAnnotations cls, 
	envInnerClass = False}

data Wrt = Wrt {wrtStms :: [J.Stm], wrtImports :: [J.Import]}
instance Monoid Wrt where
	mempty = Wrt {wrtStms = [], wrtImports = []}
	l `mappend` r = Wrt {wrtStms = wrtStms l ++ wrtStms r, wrtImports = nub (wrtImports l ++ wrtImports r)}

tellStms :: [J.Stm] -> Writer Wrt ()
tellStms stms = tell Wrt{wrtStms = stms, wrtImports = []}
tellImport :: J.Import -> Writer Wrt ()
tellImport imp = tell Wrt{wrtStms = [], wrtImports = [imp]}
tellImports :: [J.Import] -> Writer Wrt ()
tellImports imps = tell Wrt{wrtStms = [], wrtImports = imps}
tellImportClass :: D.Class -> Writer Wrt ()
tellImportClass cl = case splitOn '.' $ D.genClassName cl of
		[nm] -> tellImport $ D.packageName (D.classPackage cl) ++ [nm]
		fullNm -> tellImport fullNm
		
className :: D.Class -> String
className cl = let 
	nm = D.genClassName cl
	idxs = elemIndices '.' nm
	in case idxs of
		[] -> nm
		_ -> drop (1 + last idxs) nm



genExp :: Env -> D.Exp -> Writer Wrt J.Exp
genExp _ D.Nop = return J.Nop
genExp _ (D.None _) = return J.Null
genExp _ D.Nil = return J.Null
genExp _ (D.StringConst s) = return $ J.StringConst s
genExp _ (D.BoolConst s) = return $ J.BoolConst s
genExp _ (D.IntConst s) = return $ J.IntConst s
genExp _ (D.FloatConst s) = return $ J.FloatConst s	
genExp env (D.Dot l (D.Is dtp)) 
	| D.isTpGeneric dtp = return $ J.BoolConst False
	| otherwise = do 
		l' <- genExp env l
		dtp' <- genTp env dtp
		return $ J.InstanceOf l' dtp'
genExp env (D.Dot l (D.As dtp)) = do 
	l' <- genExp env l
	dtp' <- genTp env dtp
	return $ J.Dot (J.Ref "Util") $ J.Call "as" [dtp'] [J.Dot (J.Ref $ D.dataTypeGenClassName dtp) (J.Ref "class"), l']
genExp env (D.Dot l (D.CastDot dtp)) = do 
	l' <- genExp env l
	dtp' <- genTp env dtp
	return $ cast dtp' l'
genExp env (D.Dot (D.Call objDef _ [] []) (D.Call constr _ pars gens))
	| D.DefModConstructor `elem` D.defMods constr 
		|| (D.defName constr == "apply" && D.DefModStub `elem` D.defMods constr && D.DefModStatic `elem` D.defMods constr) 
		= do
			case D.unwrapGeneric $ D.defType objDef of
				D.TPObject _ cl -> tellImportClass cl 
				_ -> return ()
			pars' <- mapM (genParExp env) pars
			gens' <- mapM (genTp env) gens
			return $ J.New [] (D.defName objDef) gens' pars'
genExp env (D.Dot (D.Self stp) r@(D.Call d _ pars gens) ) 
	| D.DefModStatic `elem` D.defMods d = do
		r' <- genExp env r
		return $ J.Dot (J.Ref $ D.dataTypeGenClassName stp) r'
	| not (null gens) && not (null pars) = genExp env r >>= \e' -> return $ J.Dot J.This e'
	| not (null pars) = genExp env r
genExp env (D.Dot l r@(D.Call d _ _ _)) 
	| D.DefModStruct `elem` D.defMods d = do
		l' <- genExp env l
		r' <- genExp env r
		return $ case r' of
			J.Call cnm cgens cpars ->
				J.Dot 
					(J.Ref $ D.dataTypeGenClassName $ D.exprDataType l) 
					(J.Call cnm cgens (if D.DefModStatic `elem` D.defMods d then cpars else l':cpars))
			_ -> J.Dot l' r'
genExp env (D.Dot l r) = do
		l' <- genExp env l
		r' <- genExp env r
		return $ case D.unwrapGeneric (D.exprDataType l) of
			D.TPString -> J.Dot (J.New [] "StringEx" [] [l']) r'
			_ -> J.Dot l' r'
genExp env (D.Index l r) = do
	l' <- genExp env l
	r' <- genExp env r
	return $ J.Index l' r'
genExp _ (D.Call d@D.Def{D.defMods = mods} _ [] []) 
	| D.DefModChangedInLambda `elem` mods = return $ J.Dot (J.Ref $ D.defName d) (J.Ref "value")
	| D.DefModField `elem` mods || D.DefModLocal `elem` mods = return $ J.Ref $ defName d
	| D.DefModObject `elem` mods = do
		let (D.TPObject _ cl) = D.defType d
		tellImportClass cl 
		return $ J.Ref $ className cl
genExp env (D.Call constr _ pars gens) 
	| D.DefModConstructor `elem` D.defMods constr 
		= do
			pars' <- mapM (genParExp env) pars
			gens' <- mapM (genTp env) gens
			return $ J.New [] (D.dataTypeGenClassName $ D.defType constr) gens' pars'
genExp env (D.Call d _ pars gens) = do
	pars' <- mapM (genParExp env) pars
	gens' <- mapM (genTp env) gens
	return $ J.Call (defName d) gens' pars'
genExp env (D.Lambda pars e dtp) = do
	stms <- getStms env{envInnerClass = True} e
	dtp' <- if dtp == D.TPVoid then return (J.tpRef "void") else genTp env (D.wrapGeneric dtp)
	let funPar (nm, tp) = genTp env (D.wrapGeneric tp) >>= \tp' -> return ([J.DefModFinal], tp', nm)
	pars' <- mapM funPar pars
	let 
		def = J.Def {
			J.defAnnotations = [overrideAnnotation],
			J.defMods = [J.DefModVisability J.Public],
			J.defGenerics = [],
			J.defName = "apply",
			J.defTp = dtp',
			J.defPars = pars',
			J.defThrows = [],
			J.defStms = stms
		}
		clNm = (if dtp == D.TPVoid then "P" else "F") ++ if length pars == 1 then "" else show (length pars)
	return $ J.New [def] clNm (map (\(_, t, _) -> t) pars' ++ [dtp' | dtp /= D.TPVoid]) []
genExp Env{envInnerClass = False} (D.Self _) = return J.This
genExp Env{envInnerClass = True} (D.Self tp) = return  $ J.Dot (J.Ref $ D.dataTypeGenClassName tp) J.This
genExp _ (D.Super _) = return J.Super
genExp env (D.MathOp tp l r) = do
	l' <- genExp env l
	r' <- genExp env r
	return $ J.MathOp tp l' r'
genExp env (D.Not e) = genExp env e >>= return . J.Not
genExp env (D.Negative e) = genExp env e >>= return . J.Negative
genExp env (D.PlusPlus e) = genExp env e >>= return . J.PlusPlus
genExp env (D.MinusMinus e) = genExp env e >>= return . J.MinusMinus
genExp env (D.LambdaCall e) = do
	e' <- genExp env e
	return $ J.Dot e' $ J.Call "apply" [] []
genExp env (D.BoolOp tp l r) = do
	l' <- genExp env l
	r' <- genExp env r
	let
		bool = J.BoolOp tp l' r'
		eqs = (if tp == Eq then id else J.Not) $ J.Dot l' (J.Call "equals" [] [r']) 
		eq = case (l, r) of
			(D.None _, _) -> bool
			(_, D.None _) -> bool
			_ -> case D.exprDataType l of
				D.TPNumber{} -> bool
				D.TPClass _ _ cl -> if D.isEnum cl then bool else eq2
				_ ->  eq2
		eq2 = case D.exprDataType r of
				D.TPNumber{} -> bool
				D.TPClass _ _ cl -> if D.isEnum cl then bool else eqs
				_ ->  eqs
		in return $ case tp of 
			ExactEq -> J.BoolOp Eq l' r'
			ExactNotEq -> J.BoolOp NotEq l' r'
			Eq -> eq
			NotEq -> eq
			_ -> bool
genExp env (D.NonOpt _ _ e) = genExp env e
genExp env e@(D.If cond t f) = do
	cond' <- genExp env cond
	let 
		(t', tstms) = runWriter $ genExp env t
		(f', fstms) = runWriter $ genExp env f
		in case (t', f') of
			(J.Nop, J.Nop) -> do 
				tellGenStms env e
				return J.Nop
			(J.Nop, _) -> do 
				tell tstms{wrtStms = [J.If cond' (wrtStms tstms) []]}
				tell fstms
				return f'
			(_, J.Nop) -> do 
				tell tstms
				tell fstms{wrtStms = [J.If (J.Not cond') (wrtStms fstms) []]}
				return t'
			_ -> do 
				tell tstms
				tell fstms
				return $ J.InlineIf cond' t' f'
genExp env e@D.Throw{} = do 
	tellGenStms env e 
	return J.Nop
genExp env e@D.NPE = do 
	tellGenStms env e 
	return J.Nop
genExp env (D.Some _ e) = genExp env e
genExp env (D.Return _ e) = genExp env e
genExp env (D.Cast (D.TPArr _ tp) (D.Arr [])) = do
	tp' <- genTp env tp
	tellImport ["objd", "collection", "ImArray"]
	return $ J.Dot (J.Ref "ImArray") (J.Call "empty" [tp'] [])
genExp env (D.Arr exps) = do
	exps' <- mapM (genExp env) exps
	tellImport ["objd", "collection", "ImArray"]
	return $ J.Dot (J.Ref "ImArray") (J.Call "fromObjects" [] exps')
genExp env (D.Map exps) = do
	keys' <- mapM (genExp env . fst) exps
	values' <- mapM (genExp env . snd) exps
	let 
		comp (x:xs) (y:ys) = x:y:comp xs ys
		comp _ _ = []
	tellImport ["objd", "collection", "ImHashMap"]
	return $ J.Dot (J.Ref "ImHashMap") (J.Call "fromObjects" [] $ comp keys' values')
genExp env (D.StringBuild pars lastString) = do
	pars' <- mapM (par' . snd) pars
	return $ J.Dot (J.Ref  "String") $ J.Call "format" [] (J.StringConst format : join pars')
	where
		format = concatMap (\(prev, e) -> prev ++ stringFormatForType (D.exprDataType e) ) pars ++ lastString
		par' expr = do
			expr' <- genExp env expr
			return $ stringExpressionsForTp (D.exprDataType expr) expr'
genExp env (D.Tuple exps) = do
	exps' <- mapM (genExp env) exps
	gens' <- mapM (genTp env . D.wrapGeneric . D.exprDataType) exps
	return $ J.New [] (tupleClassName $ length exps') gens' exps'
genExp env (D.Weak x) = genExp env x
genExp env (D.Braces [x]) = genExp env x
genExp env (D.Braces exps) = do
	stms <- mapM (getStms env) (init exps)
	tellStms $ join stms
	genExp env $ last exps
genExp env (D.NullDot _ _ e) = genExp env e
genExp _ (D.Null _) = return J.Null
genExp env (D.Cast dtp l) = do 
	l' <- genExp env l
	dtp' <- genTp env dtp
	return $ cast dtp' l'
genExp _ e = return $ J.ExpError $ "Unknown " ++ show e

genParExp :: Env -> (D.Def, D.Exp) -> Writer Wrt J.Exp 
genParExp env (_, e) = genExp env e 

cast :: J.TP -> J.Exp -> J.Exp
cast tp@(J.TPRef [] _) e = J.Cast tp e
cast tp@(J.TPRef _ r) e 
	| J.hasTpAnyGeneric tp = J.Cast (J.TPRef [] r) e
	| otherwise = J.Cast tp $ J.Cast (J.TPRef [] r) e
cast tp e = J.Cast tp e

stringFormatForType :: D.DataType -> String
stringFormatForType (D.TPNumber False 8) = "%d"
stringFormatForType (D.TPNumber True 8) = "%d"
stringFormatForType (D.TPNumber False 0) = "%d"
stringFormatForType (D.TPNumber True 0) = "%d"
stringFormatForType (D.TPNumber False _)= "%d"
stringFormatForType (D.TPNumber True _)= "%d"
stringFormatForType (D.TPChar)= "%d"
stringFormatForType (D.TPFloatNumber _) = "%f"
stringFormatForType D.TPBool = "%d"
stringFormatForType D.TPPointer{} = "%p"
stringFormatForType (D.TPEArr n tp)  = "[" ++ strs ", " (replicate n (stringFormatForType tp)) ++ "]"
stringFormatForType _ = "%s"

stringExpressionsForTp :: D.DataType -> J.Exp -> [J.Exp]
stringExpressionsForTp rtp ref = (case rtp of
			D.TPEArr n etp -> concatMap (\j -> stringExpressionsForTp etp $ J.Index ref (J.IntConst j)) [0..n - 1]
			--D.TPNumber False 0 -> [C.ShortCast (C.TPSimple "long" []) ref]
			--D.TPNumber True 0 -> [C.ShortCast (C.TPSimple "unsigned long" []) ref]
			_ -> [ref]
			)

tellGenStms :: Env -> D.Exp -> Writer Wrt ()
tellGenStms env e = 
	let (stms', Wrt{wrtImports = imps}) = runWriter $ genStm env e
	in tell Wrt{wrtStms = stms', wrtImports = imps}

genExpStm :: Env -> D.Exp -> (J.Exp -> J.Stm) -> Writer Wrt [J.Stm]
genExpStm env e f = let 
		(e', Wrt{wrtStms = stms, wrtImports = imps}) = runWriter $ genExp env e 
		addFs ss = mapLast addF ss
		addF (J.If cond tt ff) = J.If cond (addFs tt) (addFs ff)
		addF (J.Stm ee) = f ee
		addF (J.Braces ee) = J.Braces $ addFs ee
		addF ee = ee
	in do
		tellImports imps 
		return $ if e' == J.Nop then addFs stms else stms ++ [f e']

getStms :: Env -> D.Exp -> Writer Wrt [J.Stm]
getStms env e = case e of
	D.Braces bs -> mapM (genStm env) bs >>= \s -> return $ join s
	b -> genStm env b

genStm :: Env -> D.Exp -> Writer Wrt [J.Stm]
genStm _ D.Nop = return []
genStm _ (D.None _) = return []
genStm _ D.Break = return [J.Break]
genStm env (D.Braces bs) = do
	bs' <- mapM (genStm env) bs
	return [J.Braces $ join bs']
genStm _ (D.Return _ D.Nil) = return $ [J.Return J.Nop]
genStm env (D.Return _ e) = genExpStm env e J.Return 
genStm env (D.NullDot _ _ c) = genStm env c
genStm env (D.If cond t f) = do
	t' <- getStms env t
	f' <- getStms env f
	genExpStm env cond (\cond' -> J.If cond' t' f')
genStm env (D.While cond w) = do
	w' <- getStms env w
	genExpStm env cond (\cond' -> J.While cond' w')
genStm env (D.Synchronized cond w) = do
	w' <- getStms env w
	genExpStm env cond (\cond' -> J.Synchronized cond' w')
genStm env (D.Do cond w) = do
	w' <- getStms env w
	genExpStm env cond (\cond' -> J.Do cond' w')
genStm env (D.Throw e) = genExpStm env e (\e' -> J.Throw $ J.New [] "RuntimeException" [] [e'])
genStm _ D.NPE = return [J.Throw $ J.New [] "NullPointerException" [] []]
genStm env (D.Set tp l r) = let
	(l', Wrt{wrtStms = lstm, wrtImports = limps}) = runWriter $ genExp env l
	(r', Wrt{wrtStms = rstm, wrtImports = rimps}) = runWriter $ genExp env r
	in do
		tellImports limps
		tellImports rimps
		return $ lstm ++ rstm ++ [J.Set tp l' r']
genStm env (D.Val True d) = do
	t <- genStm env $ D.defBody d
	v <- declareVal env d 
	return $ v J.Nop:t
genStm env (D.Val False d) = do
	v <- declareVal env d
	if D.defBody d == D.Nop then return [v J.Nop] else genExpStm env (D.defBody d) v
genStm env (D.Weak x) = genStm env x
genStm env e = genExpStm env e J.Stm 


declareVal :: Env -> D.Def -> Writer Wrt (J.Exp -> J.Stm)
declareVal env d
	| D.DefModChangedInLambda `elem` D.defMods d = do
		tp <- genTp env $ D.wrapGeneric $ D.defType d
		let 
			clnm = if D.DefModVolatile `elem` D.defMods d then "MutVolatile" else "Mut"
			new e = J.New [] clnm [tp] $ case e of
				J.Nop -> []
				_ -> [e]
		return $ \e -> J.Val [J.DefModFinal] (J.TPRef [tp] clnm) (D.defName d) (new e)
	| otherwise = do
		tp <- genTp env $ D.defType d
		return $ \e -> J.Val 
			([J.DefModFinal | D.DefModMutable `notElem` D.defMods d] ++ [J.DefModVolatile | D.DefModVolatile `elem` D.defMods d])
			tp (D.defName d) e
