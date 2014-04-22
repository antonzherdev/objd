module Java.Generator ( 
	toJava
)where

import Control.Monad.Writer
import Ex.String
import Data.Maybe
import Data.List
import ObjD.Link as D
import Java.Struct as J

toJava :: D.File -> [J.File]
toJava file@D.File{D.fileClasses = classes} =
	map (genFile file) $ filter (\cls -> not (D.isType cls) && not (D.isStub cls)) classes

genFile :: D.File -> D.Class -> J.File
genFile D.File{D.filePackage = D.Package{D.packageName = package}, D.fileImports = imps} cls =
	let (cls', clImps) = runWriter $ genClass cls
	in J.File {
		J.fileIsTest = D.containsAnnotationWithClassName "test.Test" $ D.classAnnotations cls,
		J.filePackage = package,
		J.fileImports = clImps ++ map genImport imps,
		J.fileClass = cls'}

genImport :: D.Import -> J.Import
genImport (D.ImportClass cl) = D.packageName (D.classPackage cl) ++ [D.className cl] 
genImport (D.ImportObjectDefs cl) = D.packageName (D.classPackage cl) ++ [D.className cl, "*"]

genClass :: D.Class -> Writer [J.Import] J.Class
genClass cl = do
	let defs = filter (\f -> not (D.isSpecial f) && not (D.isInline f)) 
			$ if D.isTrait cl then filter (not . D.isConstructor) (D.classDefs cl) else D.classDefsWithTraits cl
	defs' <- mapM (genDef cl) defs
	return J.Class {
		J.classVisibility = J.Public,
		J.classType = if D.isTrait cl then J.ClassTypeInterface else J.ClassTypeClass,
		J.className = D.className cl,
		J.classGenerics = map genGeneric $ D.classGenerics cl,
		J.classExtends = D.mainExtendsRef (D.classExtends cl) >>= \e -> if D.isBaseClass (fst e) then Nothing else Just (genExtendsRef e),
		J.classImplements = map genExtendsRef $ filter (not . D.isBaseClass . fst) $ D.traitExtendsRefs $ D.classExtends cl,
		J.classDefs = defs'
	}
		

genGeneric :: D.Class -> J.Generic
genGeneric cl = J.Generic {
	J.genericName = D.className cl,
	J.genericExtends = map genExtendsRef $ filter (not . D.isBaseClass . fst) $ D.extendsRefs $ D.classExtends cl
}

genExtendsRef :: D.ExtendsRef -> J.TP
genExtendsRef (cl, gens) = J.TPRef (map genTp gens) (D.className cl) 

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - Defs
 -----------------------------------------------------------------------------------------------------------------------------------------------}

fullDefName :: D.Def -> String
fullDefName d = D.defName d ++ concatMap (cap . D.defName) (D.defPars d)

overrideAnnotation :: J.DefAnnotation
overrideAnnotation = J.DefAnnotation "Override" []

genDef :: D.Class -> D.Def -> Writer [J.Import] J.Def
genDef cl d =
 	let 
 		genMod D.DefModPrivate = Just $ J.DefModVisability J.Private
 		genMod D.DefModProtected = Just $ J.DefModVisability J.Protected
 		genMod D.DefModPublic = Just $ J.DefModVisability J.Public
 		genMod D.DefModStatic = Just J.DefModStatic
 		genMod D.DefModAbstract = Just J.DefModAbstract
 		genMod _ = Nothing
 		constrSet D.Def{D.defName = nm} = J.Set Nothing (J.Dot J.This (J.Ref nm)) (J.Ref nm)
 		callSuperConstructor = case D.extendsClass $ D.classExtends cl of
 			Just (ExtendsClass _ []) -> return []
 			Just (ExtendsClass _ pars) -> do
 				pars' <- mapM superPar pars
 				return [J.Stm $ J.Call "super" [] pars']
 			_ -> return []
 			where superPar p = do
 				let  (e', Wrt{wrtImports = imps}) = runWriter $ genExp defaultEnv $ snd p
 				tell imps
 				return e'
 		mods = mapMaybe genMod (D.defMods d)
 	in if D.isField d then 
 			do 
 				let (e', Wrt{wrtImports = imps}) = runWriter $ genExp defaultEnv $ D.defBody d	
 				tell imps
	 			return J.Field {
	 				J.defAnnotations = [],
	 				J.defMods = mods ++ [J.DefModFinal | D.DefModMutable `notElem` D.defMods d],
	 				J.defName = D.defName d,
	 				J.defTp = genTp $ D.defType d,
	 				J.defExp = e'
		 		}
 		else if D.isConstructor d then
 			do
 				super <- callSuperConstructor
 				return J.Constructor {
	 				J.defAnnotations = [],
	 				J.defMods = mods,
	 				J.defPars = map genPar $ D.defPars d,
	 				J.defStms = super ++ (map constrSet . filter ((D.DefModConstructorField `elem` ). D.defMods)) (D.classDefs cl)
	 			}
 		else 
 			do
 				let (stms', Wrt{wrtImports = imps}) = runWriter $ getStms defaultEnv $ D.defBody d
 				tell imps
 				return J.Def {
	 				J.defAnnotations = [overrideAnnotation| D.DefModOverride `elem` D.defMods d],
	 				J.defMods = mods,
	 				J.defName = fullDefName d,
	 				J.defTp = genTp $ D.defType d,
	 				J.defPars = map genPar $ D.defPars d,
	 				J.defStms = stms'
	 			}

genPar :: D.Def -> J.DefPar
genPar D.Def{D.defName = nm, D.defType = tp} = (genTp tp, nm)

{-----------------------------------------------------------------------------------------------------------------------------------------------
 - DataType
 -----------------------------------------------------------------------------------------------------------------------------------------------}
genTp :: D.DataType -> J.TP
genTp (D.TPClass _ gens cl) = J.TPRef (map genTp gens) (D.className cl) 
genTp (D.TPNumber _ 1) = J.tpRef "byte"
genTp (D.TPNumber _ 8) = J.tpRef "long"
genTp (D.TPNumber _ _) = J.tpRef "int"
genTp (D.TPGenericWrap _ (D.TPNumber _ 1)) = J.tpRef "Byte"
genTp (D.TPGenericWrap _ (D.TPNumber _ 8)) = J.tpRef "Long"
genTp (D.TPGenericWrap _ (D.TPNumber _ _)) = J.tpRef "Integer"
genTp (D.TPFloatNumber 8) = J.tpRef "double"
genTp (D.TPFloatNumber _) = J.tpRef "float"
genTp (D.TPGenericWrap _ (D.TPFloatNumber 8)) = J.tpRef "Double"
genTp (D.TPGenericWrap _ (D.TPFloatNumber _)) = J.tpRef "Float"
genTp D.TPVoid = J.tpRef "void"
genTp (D.TPGenericWrap _ D.TPVoid) = J.tpRef "Void"
genTp D.TPChar = J.tpRef "char"
genTp (D.TPGenericWrap _ D.TPChar) = J.tpRef "Character"
genTp D.TPBool = J.tpRef "boolean"
genTp (D.TPGenericWrap _ D.TPBool) = J.tpRef "Boolean"

genTp (D.TPFun (D.TPTuple [stp]) D.TPVoid) = J.TPRef [genTp $ D.wrapGeneric stp] "P"
genTp (D.TPFun (D.TPTuple stps) D.TPVoid) = J.TPRef (map (genTp . D.wrapGeneric) stps) ("P" ++ show (length stps))
genTp (D.TPFun stp D.TPVoid) = J.TPRef [genTp $ D.wrapGeneric stp] "P"
genTp (D.TPFun (D.TPTuple [stp]) dtp) = J.TPRef [genTp $ D.wrapGeneric stp, genTp $ D.wrapGeneric dtp] "F"
genTp (D.TPFun (D.TPTuple stps) dtp) = J.TPRef (map (genTp . D.wrapGeneric) stps ++ [genTp $ D.wrapGeneric dtp]) ("F" ++ show (length stps))
genTp (D.TPFun stp dtp) = J.TPRef [genTp $ D.wrapGeneric stp, genTp $ D.wrapGeneric dtp] "F"

genTp (D.TPGenericWrap _ w) = genTp w
genTp D.TPString = J.tpRef "String"
genTp (D.TPEArr n tp)  = J.TPArr (genTp tp) n
genTp D.TPAny = J.tpRef "Object"
genTp (D.TPArr _ tp) = J.TPRef [genTp tp] "ImArray"
genTp (D.TPTuple tps) = J.TPRef (map genTp tps) ("Tuple" ++ show (length tps))
genTp (D.TPSelf cl) = J.TPRef (map (J.tpRef . D.className) (D.classGenerics cl) ) (D.className cl)
genTp (D.TPMap key value) = J.TPRef [genTp key, genTp value] "HashMap"
genTp (D.TPOption _ tp) = genTp tp
genTp D.TPAnyGeneric = J.TPAnyGeneric
genTp (D.TPPointer _) = J.tpRef "Pointer"
genTp (D.TPNil) = J.tpRef "Object"

genTp tp = error $ "genTp: " ++ show tp

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

tellImport :: J.Import -> Writer Wrt ()
tellImport imp = tell Wrt{wrtStms = [], wrtImports = [imp]}
tellImports :: [J.Import] -> Writer Wrt ()
tellImports imps = tell Wrt{wrtStms = [], wrtImports = imps}

genExp :: Env -> D.Exp -> Writer Wrt J.Exp
genExp _ D.Nop = return J.Nop
genExp _ (D.None _) = return J.Null
genExp _ D.Nil = return J.Null
genExp _ (D.StringConst s) = return $ J.StringConst s
genExp env (D.Dot (D.Call objDef _ [] []) (D.Call constr _ pars gens))
	| D.DefModConstructor `elem` D.defMods constr 
		|| (D.defName constr == "apply" && D.DefModStub `elem` D.defMods constr && D.DefModStatic `elem` D.defMods constr) 
		= do
			pars' <- mapM ((genExp env) . snd) pars
			return $ J.New [] $ J.Call (D.defName objDef) (map genTp gens) pars'
genExp env (D.Dot (Self _) r@(D.Call _ _ pars _) ) 
	| not (null pars) = genExp env r
genExp env (D.Dot l r) = do
	l' <- genExp env l
	r' <- genExp env r
	return $ J.Dot l' r'
genExp _ (D.Call d _ [] []) 
	| D.DefModField `elem` D.defMods d || D.DefModLocal `elem` D.defMods d = return $ J.Ref $ D.defName d
genExp env (D.Call d _ pars gens) = do
	pars' <- mapM ((genExp env) . snd) pars
	return $ J.Call (fullDefName d) (map genTp gens) pars'
genExp env (D.Lambda pars e dtp) = do
	stms <- getStms env{envInnerClass = True} e
	let 
		def = J.Def {
			J.defAnnotations = [overrideAnnotation],
			J.defMods = [J.DefModVisability J.Public],
			J.defName = "apply",
			J.defTp = if dtp == D.TPVoid then J.tpRef "void" else genTp (D.wrapGeneric dtp),
			J.defPars = map funPar pars,
			J.defStms = stms
		}
		funPar (nm, tp) = (genTp (D.wrapGeneric tp), nm)
		clNm = (if dtp == D.TPVoid then "P" else "F") ++ if length pars == 1 then "" else show (length pars)
	return $ J.New [def] $ J.Call clNm (map (genTp . D.wrapGeneric . snd) pars ++ [genTp (D.wrapGeneric dtp) | dtp /= D.TPVoid]) []
genExp Env{envInnerClass = False} (D.Self _) = return J.This
genExp Env{envInnerClass = True} (D.Self tp) = return  $ J.Dot (J.Ref $ D.dataTypeClassName tp) J.This
genExp env (D.MathOp tp l r) = do
	l' <- genExp env l
	r' <- genExp env r
	return $ J.MathOp tp l' r'
genExp env (D.BoolOp tp l r) = do
	l' <- genExp env l
	r' <- genExp env r
	let
		bool = J.BoolOp tp l' r'
		eq = case (l, r) of
			(D.None _, _) -> bool
			(_, D.None _) -> bool
			_ -> case (D.exprDataType l, D.exprDataType r) of
				_ -> J.Dot l' (J.Call "equals" [] [r'])
		in return $ case tp of 
			ExactEq -> J.BoolOp Eq l' r'
			ExactNotEq -> J.BoolOp NotEq l' r'
			Eq -> eq
			NotEq -> eq
			_ -> bool
genExp env (NonOpt _ _ e) = genExp env e
genExp env e@(D.If cond t f) = do
	cond' <- genExp env cond
	let 
		(t', tstms) = runWriter $ genExp env t
		(f', fstms) = runWriter $ genExp env f
		in if t' == J.Nop || f' == J.Nop then (do 
			tellGenStms env e
			return J.Nop
		) else (do 
			tell tstms
			tell fstms
			return $ J.InlineIf cond' t' f'
		)
genExp env e@D.Throw{} = do 
	tellGenStms env e 
	return J.Nop
genExp env (D.Some _ e) = genExp env e
genExp env (D.Return False e) = genExp env e
genExp env (D.Arr exps) = do
	tellImport ["java", "util", "Arrays"]
	exps' <- mapM (genExp env) exps
	return $ J.Dot (J.Ref "Arrays") (J.Call "asList" [] exps')
genExp _ e = return $ J.ExpError $ "Unknown " ++ show e


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
genStm env (D.Braces bs) = do
	bs' <- mapM (genStm env) bs
	return [J.Braces $ join bs']
genStm env (D.Return _ e) = genExpStm env e J.Return 
genStm env (D.If cond t f) = do
	t' <- getStms env t
	f' <- getStms env f
	genExpStm env cond (\cond' -> J.If cond' t' f')
genStm env (D.Throw e) = genExpStm env e (\e' -> J.Throw $ J.New [] $ J.Call "RuntimeException" [] [e'])
genStm env (D.Set tp l r) = let
	(l', Wrt{wrtStms = lstm, wrtImports = limps}) = runWriter $ genExp env l
	(r', Wrt{wrtStms = rstm, wrtImports = rimps}) = runWriter $ genExp env r
	in do
		tellImports limps
		tellImports rimps
		return $ lstm ++ rstm ++ [J.Set tp l' r']
genStm env e = genExpStm env e J.Stm 

