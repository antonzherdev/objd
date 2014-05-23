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
genFile D.File{D.filePackage = D.Package{D.packageName = package}, D.fileImports = imps} cls =
	let (cls', Wrt{wrtImports = clImps}) = runWriter $ genClass cls
	in J.File {
		J.fileIsTest = D.containsAnnotationWithClassName "test.Test" $ D.classAnnotations cls,
		J.filePackage = package,
		J.fileImports = ["objd", "lang", "*"] : (filter (\p -> package /= init p && ["objd", "lang"] /= init p) $ nub $ clImps ++ map genImport imps),
		J.fileClass = cls'}

genImport :: D.Import -> J.Import
genImport (D.ImportClass cl) = D.packageName (D.classPackage cl) ++ [D.className cl] 
genImport (D.ImportObjectDefs cl) = D.packageName (D.classPackage cl) ++ [D.className cl, "*"]

genClass :: D.Class -> Writer Wrt J.Class
genClass cl = do
	let 
		isEnum = D.isEnum cl 
		defs = filter (\f -> not (D.isSpecial f) && not (D.isEnumItem f)) 
			$ if D.isTrait cl then filter (not . D.isConstructor) (D.classDefs cl) else
				filter (D.isDefAbstract) (D.classDefs cl) ++ filter (not . D.isDefAbstract) (D.classDefsWithTraits cl)
		trMod D.ClassModAbstract = Just J.ClassModAbstract
		trMod D.ClassModFinal = Just J.ClassModFinal
		trMod _ = Nothing
		staticInitFields = filter (\ f -> D.isField f && not (D.isEnumItem f) && D.defBody f /= D.Nop && D.DefModStatic `elem` D.defMods f) (D.classDefs cl)
		genStaticSet d = do
			e' <- genExp defaultEnv $ D.defBody d
			return $ J.Set Nothing (J.Ref $ D.defName d) e'
	defs' <- mapM (genDef cl) defs
	gens' <- mapM genGeneric $ D.classGenerics cl
	ext' <- case D.mainExtendsRef (D.classExtends cl) of
		Just e -> if D.isBaseClass (fst e) then return Nothing else fmap Just (genExtendsRef e)
		Nothing -> return Nothing
	impls' <- mapM genExtendsRef $ filter (not . D.isBaseClass . fst) $ D.traitExtendsRefs $ D.classExtends cl
	enumItems' <- mapM genEnumItem (if isEnum then filter D.isEnumItem (D.classDefs cl) else [])
	staticInitFields' <- mapM genStaticSet staticInitFields
	return J.Class {
		J.classMods = [J.ClassModVisibility J.Public] ++ mapMaybe trMod (D.classMods cl),
		J.classType = if D.isTrait cl then J.ClassTypeInterface else if isEnum then J.ClassTypeEnum else J.ClassTypeClass,
		J.className = D.className cl,
		J.classGenerics = gens',
		J.classExtends = if isEnum then Nothing else ext',
		J.classImplements = impls',
		J.classEnumItems = enumItems',
		J.classDefs = join defs' ++ [J.StaticConstructor staticInitFields' | not (null staticInitFields')]
	}

genEnumItem :: D.Def -> Writer Wrt J.EnumItem
genEnumItem D.Def{D.defName = name, D.defBody = D.Dot _ (D.Call _ _ (_:_:pars) _)} = do
	pars' <- (mapM (genExp defaultEnv) . map snd) pars
	return $ J.EnumItem name pars'

genGeneric :: D.Class -> Writer Wrt J.Generic
genGeneric cl = do
	exts <- mapM genExtendsRef $ filter (not . D.isBaseClass . fst) $ D.extendsRefs $ D.classExtends cl
	return J.Generic {
		J.genericName = D.className cl,
		J.genericExtends = exts
	}

genExtendsRef :: D.ExtendsRef -> Writer Wrt J.TP
genExtendsRef (cl, gens) = do
	tellImportClass cl
	gens' <- mapM genTp gens
	return $ J.TPRef gens' (D.className cl) 

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Defs
 -----------------------------------------------------------------------------------------------------------------------------------------------}

defName' :: D.Def -> String
defName' d = case D.defPars d of
	[] -> case D.defName d of
		"hash" -> "hashCode"
		"description" -> "toString"
		"isEqual" -> "equals"
		o -> o
	_ -> fromMaybe (D.defName d ++ concatMap (cap . D.defName) (D.defPars d)) $ 
		D.findAnnotationWithClassName "objd.gen.GenName" (D.defAnnotations d) >>= (\a -> case a of
			D.Annotation _ [(_, D.StringConst s)] -> Just s
			_ -> Nothing)

overrideAnnotation :: J.DefAnnotation
overrideAnnotation = J.DefAnnotation "Override" []

genDef :: D.Class -> D.Def -> Writer Wrt [J.Def]
genDef cl d =
 	let 
 		genMod D.DefModPrivate = Just $ J.DefModVisability J.Private
 		genMod D.DefModProtected = Just $ J.DefModVisability J.Protected
 		genMod D.DefModPublic = Just $ J.DefModVisability J.Public
 		genMod D.DefModStatic = Just J.DefModStatic
 		genMod D.DefModAbstract = Just J.DefModAbstract
 		genMod _ = Nothing
 		constrSet dd = let nm = defName' dd in J.Set Nothing (J.Dot J.This (J.Ref nm)) (J.Ref nm)
 		body = if D.isTrait cl then D.Nop else D.defBody d	
 		mods = mapMaybe genMod (D.defMods d)
 		name = defName' d
 		genSet dd = do
			e' <- genExp defaultEnv (D.defBody dd)
 			return $ J.Set Nothing (J.Dot J.This $ J.Ref (D.defName dd)) e'
 	in if D.isField d then 
 			do
	 			tp <- genTp $ D.defType d
	 			return $ [J.Field {
		 				J.defAnnotations = [],
		 				J.defMods = mods ++ [J.DefModFinal | D.DefModMutable `notElem` D.defMods d] ++ [J.DefModVolatile | D.DefModVolatile `elem` D.defMods d],
		 				J.defName = name,
		 				J.defTp = tp,
		 				J.defExp = J.Nop
			 		}] ++ [J.Def {
		 				J.defAnnotations = [overrideAnnotation],
		 				J.defMods = mods,
		 				J.defGenerics = [],
		 				J.defName = name,
		 				J.defTp = tp,
		 				J.defPars = [],
		 				J.defStms = [J.Return $ J.Ref name]
		 			}| D.DefModOverride `elem` D.defMods d]
 		else if D.isConstructor d then
 			do
 				let 
 					isEnum = D.isEnum cl
 					callSuperConstructor = if isEnum then return [] else case D.extendsClass $ D.classExtends cl of
			 			Just (D.ExtendsClass _ []) -> return []
			 			Just (D.ExtendsClass _ pars) -> do
			 				pars' <- mapM (genExp defaultEnv . snd) pars
			 				return [J.Stm $ J.Call "super" [] pars']
			 			_ -> return []
 				super <- callSuperConstructor
 				let 
 					fields = filter (\ f -> 
	 					D.isField f && D.defBody f /= D.Nop && D.DefModConstructorField `notElem` D.defMods f && D.DefModStatic `notElem` D.defMods f) 
	 					(D.classDefs cl)
 				sets <- mapM genSet fields
 				pars' <- mapM genPar $ if isEnum then (tail $ tail $ D.defPars d) else D.defPars d
 				return [J.Constructor {
	 				J.defAnnotations = [],
	 				J.defMods = if isEnum then J.DefModVisability J.Private : filter (not . J.isDefModVisibility) mods else mods,
	 				J.defPars = pars',
	 				J.defStms = super ++ (map constrSet . filter ((D.DefModConstructorField `elem` ). D.defMods)) (D.classDefs cl) ++ sets
	 			}]
 		else 
 			do
 				tp <- genTp $ D.defType d
 				stms' <- getStms defaultEnv body
 				gens' <- mapM genGeneric $ maybe [] D.defGenericsClasses $ D.defGenerics d
 				pars' <- mapM genPar $ D.defPars d
 				return [J.Def {
	 				J.defAnnotations = [overrideAnnotation| D.DefModOverride `elem` D.defMods d],
	 				J.defMods = mods,
	 				J.defGenerics = gens',
	 				J.defName = name,
	 				J.defTp = tp,
	 				J.defPars = pars',
	 				J.defStms = stms'
	 			}]

genPar :: D.Def -> Writer Wrt J.DefPar
genPar D.Def{D.defName = nm, D.defType = tp} = do
	tp' <- genTp tp
	return ([J.DefModFinal], tp', nm)

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - DataType
 -----------------------------------------------------------------------------------------------------------------------------------------------}
genTp :: D.DataType -> Writer Wrt J.TP
genTp (D.TPClass tp gens cl) = do
	when (tp /= D.TPMGeneric && tp /= D.TPMType) $ tellImportClass cl
	gens' <- mapM genTp gens
	return $ J.TPRef gens' (D.className cl) 
genTp (D.TPNumber _ 1) = return $ J.tpRef "byte"
genTp (D.TPNumber _ 8) = return $ J.tpRef "long"
genTp (D.TPNumber _ _) = return $ J.tpRef "int"
genTp (D.TPGenericWrap _ (D.TPNumber _ 1)) = return $ J.tpRef "Byte"
genTp (D.TPGenericWrap _ (D.TPNumber _ 8)) = return $ J.tpRef "Long"
genTp (D.TPGenericWrap _ (D.TPNumber _ _)) = return $ J.tpRef "Integer"
genTp (D.TPFloatNumber 8) = return $ J.tpRef "double"
genTp (D.TPFloatNumber _) = return $ J.tpRef "double"
genTp (D.TPGenericWrap _ (D.TPFloatNumber 8)) = return $ J.tpRef "Double"
genTp (D.TPGenericWrap _ (D.TPFloatNumber _)) = return $ J.tpRef "Float"
genTp D.TPVoid = return $ J.tpRef "void"
genTp (D.TPGenericWrap _ D.TPVoid) = return $ J.tpRef "Void"
genTp D.TPChar = return $ J.tpRef "char"
genTp (D.TPGenericWrap _ D.TPChar) = return $ J.tpRef "Character"
genTp D.TPBool = return $ J.tpRef "boolean"
genTp (D.TPGenericWrap _ D.TPBool) = return $ J.tpRef "Boolean"

genTp (D.TPFun [] D.TPVoid) = return $ J.TPRef [] "P0"
genTp (D.TPFun [stp] D.TPVoid) = do 
	stp' <- genTp $ D.wrapGeneric stp
	return $ J.TPRef [stp'] "P"
genTp (D.TPFun stps D.TPVoid) = do
	stps' <- mapM (genTp . D.wrapGeneric) stps
	return $ J.TPRef stps' ("P" ++ show (length stps))
genTp (D.TPFun [] dtp) = do 
	dtp' <- genTp $ D.wrapGeneric dtp
	return $ J.TPRef [dtp'] "F0"
genTp (D.TPFun [stp] dtp) = do 
	stp' <- genTp $ D.wrapGeneric stp
	dtp' <- genTp $ D.wrapGeneric dtp
	return $ J.TPRef [stp', dtp'] "F"
genTp (D.TPFun stps dtp) = do 
	stps' <- mapM (genTp . D.wrapGeneric) stps
	dtp' <- genTp $ D.wrapGeneric dtp
	return $ J.TPRef (stps'  ++ [dtp']) ("F" ++ show (length stps))

genTp (D.TPGenericWrap _ w) = genTp w
genTp D.TPString = return $ J.tpRef "String"
genTp (D.TPEArr n tp)  = do
	tp' <- genTp tp
	return $ J.TPArr tp' n
genTp D.TPAny = return $ J.tpRef "Object"
genTp (D.TPArr _ tp) = do
	tp' <- genTp tp
	return $ J.TPRef [tp'] "ImArray"
genTp (D.TPTuple tps) = do
	tps' <- mapM genTp tps
	return $ J.TPRef tps' (tupleClassName $ length tps)
genTp (D.TPSelf cl) = return $ J.TPRef (map (J.tpRef . D.className) (D.classGenerics cl) ) (D.className cl)
genTp (D.TPMap key value) = do
	key' <- genTp key
	value' <- genTp value
	return $ J.TPRef [key', value'] "ImHashMap"
genTp (D.TPOption _ tp) = genTp tp
genTp D.TPAnyGeneric = return J.TPAnyGeneric
genTp (D.TPPointer _) = return $ J.tpRef "Pointer"
genTp (D.TPNil) = return $ J.tpRef "Object"
genTp (D.TPUnknown e) = return $ J.TPUnknown e

genTp tp = error $ "genTp: " ++ show tp

tupleClassName :: Int -> String
tupleClassName 2 = "Tuple"
tupleClassName i = "Tuple" ++ show i
{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Stm
 -----------------------------------------------------------------------------------------------------------------------------------------------}

data Env = Env {envInnerClass :: Bool}
defaultEnv :: Env
defaultEnv = Env{envInnerClass = False}

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
tellImportClass cl = tellImport $ D.packageName (D.classPackage cl) ++ [D.className cl]

genExp :: Env -> D.Exp -> Writer Wrt J.Exp
genExp _ D.Nop = return J.Nop
genExp _ (D.None _) = return J.Null
genExp _ D.Nil = return J.Null
genExp _ (D.StringConst s) = return $ J.StringConst s
genExp _ (D.BoolConst s) = return $ J.BoolConst s
genExp _ (D.IntConst s) = return $ J.IntConst s
genExp _ (D.FloatConst s) = return $ J.FloatConst s
genExp env (D.Dot l (D.Is dtp)) = do 
	l' <- genExp env l
	dtp' <- genTp dtp
	return $ J.InstanceOf l' dtp'
genExp env (D.Dot l (D.As dtp)) = do 
	l' <- genExp env l
	dtp' <- genTp dtp
	return $ J.Dot (J.Ref "Util") $ J.Call "as" [dtp'] [J.Dot (J.Ref $ D.dataTypeClassName dtp) (J.Ref "class"), l']
genExp env (D.Dot l (D.CastDot dtp)) = do 
	l' <- genExp env l
	dtp' <- genTp dtp
	return $ J.Cast dtp' l'
genExp env (D.Cast dtp l) = do 
	l' <- genExp env l
	dtp' <- genTp dtp
	return $ J.Cast dtp' l'
genExp env (D.Dot (D.Call objDef _ [] []) (D.Call constr _ pars gens))
	| D.DefModConstructor `elem` D.defMods constr 
		|| (D.defName constr == "apply" && D.DefModStub `elem` D.defMods constr && D.DefModStatic `elem` D.defMods constr) 
		= do
			pars' <- mapM ((genExp env) . snd) pars
			gens' <- mapM genTp gens
			return $ J.New [] (D.defName objDef) gens' pars'
genExp env (D.Dot (D.Self stp) r@(D.Call d _ pars gens) ) 
	| D.DefModStatic `elem` D.defMods d = do
		r' <- genExp env r
		return $ J.Dot (J.Ref $ D.dataTypeClassName stp) r'
	| not (null gens) && not (null pars) = genExp env r >>= \e' -> return $ J.Dot J.This e'
	| not (null pars) = genExp env r
genExp env (D.Dot l r) = do
	l' <- genExp env l
	r' <- genExp env r
	return $ J.Dot l' r'
genExp env (D.Index l r) = do
	l' <- genExp env l
	r' <- genExp env r
	return $ J.Index l' r'
genExp _ (D.Call d@D.Def{D.defMods = mods} _ [] []) 
	| D.DefModChangedInLambda `elem` mods = return $ J.Dot (J.Ref $ D.defName d) (J.Ref "value")
	| D.DefModField `elem` mods || D.DefModLocal `elem` mods = return $ J.Ref $ D.defName d
	| D.DefModObject `elem` mods = do
		let (D.TPObject _ cl) = D.defType d
		tellImportClass cl 
		return $ J.Ref $ D.defName d
genExp env (D.Call constr _ pars gens) 
	| D.DefModConstructor `elem` D.defMods constr 
		= do
			pars' <- mapM ((genExp env) . snd) pars
			gens' <- mapM genTp gens
			return $ J.New [] (D.dataTypeClassName $ D.defType constr) gens' pars'
genExp env (D.Call d _ pars gens) = do
	pars' <- mapM ((genExp env) . snd) pars
	gens' <- mapM genTp gens
	return $ J.Call (defName' d) gens' pars'
genExp env (D.Lambda pars e dtp) = do
	stms <- getStms env{envInnerClass = True} e
	dtp' <- if dtp == D.TPVoid then return (J.tpRef "void") else genTp (D.wrapGeneric dtp)
	let funPar (nm, tp) = genTp (D.wrapGeneric tp) >>= \tp' -> return ([J.DefModFinal], tp', nm)
	pars' <- mapM funPar pars
	let 
		def = J.Def {
			J.defAnnotations = [overrideAnnotation],
			J.defMods = [J.DefModVisability J.Public],
			J.defGenerics = [],
			J.defName = "apply",
			J.defTp = dtp',
			J.defPars = pars',
			J.defStms = stms
		}
		clNm = (if dtp == D.TPVoid then "P" else "F") ++ if length pars == 1 then "" else show (length pars)
	return $ J.New [def] clNm (map (\(_, t, _) -> t) pars' ++ [dtp' | dtp /= D.TPVoid]) []
genExp Env{envInnerClass = False} (D.Self _) = return J.This
genExp Env{envInnerClass = True} (D.Self tp) = return  $ J.Dot (J.Ref $ D.dataTypeClassName tp) J.This
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
genExp env (D.Arr exps) = do
	exps' <- mapM (genExp env) exps
	return $ J.Dot (J.Ref "ImArray") (J.Call "fromObjects" [] exps')
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
	gens' <- mapM (genTp . D.exprDataType) exps
	return $ J.New [] (tupleClassName $ length exps') gens' exps'
genExp env (D.Weak x) = genExp env x
genExp env (D.Braces [x]) = genExp env x
genExp env (D.Braces exps) = do
	stms <- mapM (getStms env) (init exps)
	tellStms $ join stms
	genExp env $ last exps
genExp env (D.NullDot _ _ e) = genExp env e
genExp _ e = return $ J.ExpError $ "Unknown " ++ show e



stringFormatForType :: D.DataType -> String
stringFormatForType (D.TPNumber False 8) = "%ld"
stringFormatForType (D.TPNumber True 8) = "%lu"
stringFormatForType (D.TPNumber False 0) = "%ld"
stringFormatForType (D.TPNumber True 0) = "%lu"
stringFormatForType (D.TPNumber False _)= "%d"
stringFormatForType (D.TPNumber True _)= "%u"
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
genStm _ D.Break = return [J.Break]
genStm env (D.Braces bs) = do
	bs' <- mapM (genStm env) bs
	return [J.Braces $ join bs']
genStm _ (D.Return _ D.Nil) = return $ [J.Return J.Nop]
genStm env (D.Return _ e) = genExpStm env e J.Return 
genStm env (D.If cond t f) = do
	t' <- getStms env t
	f' <- getStms env f
	genExpStm env cond (\cond' -> J.If cond' t' f')
genStm env (D.While cond w) = do
	w' <- getStms env w
	genExpStm env cond (\cond' -> J.While cond' w')
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
	v <- declareVal d 
	return $ v J.Nop:t
genStm env (D.Val False d) = do
	v <- declareVal d
	if D.defBody d == D.Nop then return [v J.Nop] else genExpStm env (D.defBody d) v
genStm env (D.Weak x) = genStm env x
genStm env e = genExpStm env e J.Stm 


declareVal :: D.Def -> Writer Wrt (J.Exp -> J.Stm)
declareVal d
	| D.DefModChangedInLambda `elem` D.defMods d = do
		tp <- genTp $ D.wrapGeneric $ D.defType d
		let 
			clnm = if D.DefModVolatile `elem` D.defMods d then "MutVolatile" else "Mut"
			new e = J.New [] clnm [tp] $ case e of
				J.Nop -> []
				_ -> [e]
		return $ \e -> J.Val [J.DefModFinal] (J.TPRef [tp] clnm) (D.defName d) (new e)
	| otherwise = do
		tp <- genTp $ D.defType d
		return $ \e -> J.Val 
			([J.DefModFinal | D.DefModMutable `notElem` D.defMods d] ++ [J.DefModVolatile | D.DefModVolatile `elem` D.defMods d])
			tp (D.defName d) e
