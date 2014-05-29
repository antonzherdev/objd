module ObjD.Link.Expr (
	exprTo, exprToSome, expr
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Extends
import 			 ObjD.Link.Conversion
import 			 ObjD.Link.Inline
import 			 ObjD.Link.Option
import 			 ObjD.Link.Call
import 			 ObjD.Link.Pointer
import 			 ObjD.Link.PatternMatching
import 			 ObjD.Link.String
import 			 ObjD.Link.Composition
import           Data.Maybe
import           Ex.String
import 			 Control.Arrow
import qualified ObjD.Struct         as D


exprTo :: Env -> DataType -> D.Exp -> Exp
exprTo env tp e = implicitConvertsion env tp $ expr env{envTp = tp} e

exprToSome :: Env ->D.Exp -> Exp
exprToSome env e =  expr env{envTp = baseDataType env} e

expr :: Env -> D.Exp -> Exp
expr env (D.If cond t D.Nop) = let
	cond' = exprTo (envAddSuffix env "c") TPBool cond 
	env' = optChecking (envAddSuffix env "t") cond'
	in If cond' (expr (fst env') t) Nop
expr env (D.If cond t f)
	| envTp env == TPVoid = let
		cond' = exprTo (envAddSuffix env "c") TPBool cond 
		env' = optChecking env cond'
		in If cond' (expr (envAddSuffix (fst env') "t") t) (expr (envAddSuffix (snd env') "f") f)
	| otherwise = let 
		cond' = exprTo env TPBool cond 
		env' = optChecking env cond'
		t' = expr (envAddSuffix (fst env') "t") t
		f' = expr (envAddSuffix (snd env') "f") f
		dt = exprDataType t'
		df = exprDataType f'
		retTp = firstCommonSuperDataType env dt df
	in If cond' (maybeCast retTp t') (maybeCast retTp f')
expr env (D.While cond t) = let
	cond' = exprTo env TPBool cond 
	env' = fst $ optChecking env{envTp = TPVoid} cond'
	in While cond' (expr env' t)
expr env (D.Synchronized cond t) = Synchronized (exprToSome env cond) (expr env t)
expr env (D.Try e f) = Try (expr env e) (expr env f)
expr env (D.Do cond t) = Do (exprTo env TPBool cond) (expr env{envTp = TPVoid} t)
expr env (D.Weak e) = insertWeak (expr env e)
expr _ (D.Braces []) = Nop
expr env (D.Braces es) = Braces $ bracesRec env 0 es
	where
		bracesRec :: Env -> Int -> [D.Exp] -> [Exp]
		bracesRec _  _ [] = []
		bracesRec env' n [x] = [expr env'{envVarSuffix = envVarSuffix env ++ ('_' : show n)} x]
		bracesRec env' n (x@D.Val{}:xs) = let
			x'@(Val m d) = expr env'{envVarSuffix = envVarSuffix env ++ ('_' : show n), envTp = TPVoid} x 
			env'' = (envAddVals [d] env')
			xs' = (bracesRec env'' (n + 1) xs)
			in 
				if DefModMutable `elem` defMods d then 
					let 
						existsSetInLambda = any (isJust . setsInLambda) parLambdas
						parLambdas = forExp isLambda (Braces xs')
						isLambda ee@Lambda{} = [ee]
						isLambda _ = []
						setsInLambda (Lambda _ lambdaExpr _) = forExp isSet lambdaExpr
						isSet ee@(Set _ (Call dd _ _ _) _) = if d == dd then Just ee else Nothing
						isSet ee@(PlusPlus (Call dd _ _ _)) = if d == dd then Just ee else Nothing
						isSet ee@(MinusMinus (Call dd _ _ _)) = if d == dd then Just ee else Nothing
						isSet _ = Nothing
						d' = d{defMods = DefModChangedInLambda :defMods d}
						ch (Call dd t [] [])
							| dd == d = Just $ Call d' t [] []
						ch _ = Nothing
					in if existsSetInLambda then (Val m d'):map (mapExp ch) xs' else x':xs'
				else x':xs'
		bracesRec env' n (x:xs) = let 
				x' = expr env'{envVarSuffix = envVarSuffix env ++ ('_' : show n), envTp = TPVoid} x
				env'' = case x' of
					(Set _ (Call d _ [] _) r) -> case unwrapGeneric (defType d) of
						TPOption cl tpl -> case unwrapGeneric (exprDataType r) of
							TPOption cr _ -> if cr == cl then env' else envChangeDefTp env' d (TPOption cr tpl)
							_ -> env'
						_ -> env'
					_ -> env'
				xs' = (bracesRec env'' (n + 1) xs)
			in x':xs'
expr _ D.Nop = Nop
expr _ (D.IntConst i) = IntConst i
expr _ (D.StringConst i) = StringConst i
expr _ D.Nil = Nil
expr _ (D.BoolConst i) = BoolConst i
expr _ (D.FloatConst s) = FloatConst s
expr env (D.BoolOp tp a b)
	| tp == Or || tp == And = let
		a' = exprTo (envAddSuffix env "a") TPBool a
		env' = optChecking env a'
		env'' = envAddSuffix (if tp == Or then snd env' else fst env') "b"
	in BoolOp tp a' (exprTo env'' TPBool b)
expr env (D.BoolOp tp a b) 
	| tp == Eq || tp == NotEq = let
		a' = exprToSome (envAddSuffix env "a") a
		b' = exprToSome (envAddSuffix env "b") b
		atp = unwrapGeneric $ exprDataType a'
		btp = unwrapGeneric $ exprDataType b'
	in case (a', b') of
		(Nil, _) -> compareWithNil env tp (b', btp)
		(_, Nil) -> compareWithNil env tp (a', atp)
		_ -> case (atp, btp) of
			(TPOption False _, TPOption False _) -> compareOptions env tp (a', atp) (b', btp)
			(TPOption False _, _) -> compareOptionWithNonOption env tp (a', atp) (b', btp)
			(_, TPOption False _) -> compareOptionWithNonOption env tp (b', btp) (a', atp)
			_ -> BoolOp tp  a' b'
expr env (D.BoolOp tp a b) = BoolOp tp (exprToSome (envAddSuffix env "a") a) (exprToSome (envAddSuffix env "b") b)
expr env (D.MathOp tp a b) = 
	let 
		aa = exprToSome (envAddSuffix env "a") a
		ltp = exprDataType aa
		math = MathOp tp aa (exprToSome (envAddSuffix env "b") b)
		cll = D.Call (literalDefName $ show tp) (Just [(Nothing, b)]) []
		callOp =  Dot aa $ exprCall (envAddSuffix env "b") (Just ltp) $ cll
	in case unwrapGeneric ltp of
		TPNumber{} -> math
		TPFloatNumber{} -> math
		TPString{} -> math
		TPPointer{} -> math
		_ -> callOp 
expr env d@(D.Dot a b) = let
	aa = case a of
		D.Call {} -> exprCall (envAddSuffix env "l") Nothing a
		_ -> exprToSome (envAddSuffix env "l") a
	aTp' = case exprDataType aa of
		TPOption True t -> t
		TPPointer t -> t
		TPGenericWrap _ (TPOption True t) -> t
		t -> t
	aTp = case exprDataType aa of
		TPOption True t -> t
		TPGenericWrap _ (TPOption True t) -> t
		t -> t
	bb = exprCall (envAddSuffix env "r") (Just aTp')  b
	def = case aa of
			ExpDError s _ -> ExpDError s d
			_ -> case bb of
				Dot l r -> maybeInlineCall env $ Dot (Dot aa l) r
				_ -> maybeInlineCall env $ Dot aa bb
	in case unwrapGeneric aTp of
		TPOption{} -> linkOptionCall env (a, aa) b
		TPPointer{} -> fromMaybe (Arrow aa bb) $ linkPointerCall env (a, aa) b
		TPObject _ Class{className = "Pointer"} -> fromMaybe def $ linkPointerStatic env b
		_ -> def
expr env d@(D.NullDot _ _) = linkNullDot env d
expr env (D.Set tp a b) = 
	let 
		aa = exprToSome env a
		ltp = case aa of
			NullDot _ r _ -> exprDataType r
			_ -> exprDataType aa
		simpleSet = if isNothing tp then set Nothing aa math
			else case unwrapGeneric ltp of
				TPNumber{} -> set tp aa math
				TPFloatNumber{} -> set tp aa math
				TPString{} -> set tp aa math
				TPPointer{} -> set tp aa math
				_ -> set Nothing aa callOp

		set tp' (NullDot dl dr _) r = 
			let 
				dltp = exprDataType dl
				tmp = tmpVal env "" dltp dl
				ref = if isElementaryExpression dl then dl else callRef tmp
				f = If (BoolOp NotEq ref (None dltp) ) (Set tp' (Dot (nonOpt env False ref) dr) r) Nop
			in  if isElementaryExpression dl then f else Braces [declareVal env tmp, f]
		set tp' l r = if isSimpleExpression r then Set tp' l r else multilineSet env tp' l r

		rtp = case unwrapGeneric ltp of
			 t@TPPointer{} -> if isJust tp then int else t
			 t -> t
		math = exprTo env rtp b 
		callOp = Dot aa $ exprCall env (Just ltp) $ D.Call (literalDefName $ show $ fromJust tp) (Just [(Nothing, b)]) []
		lcall = case aa of
				Dot _ c@(Call {}) -> Just c
				Arrow _ c@(Call {}) -> Just c
				NullDot _ c@(Call {}) _ -> Just c
				c@Call {} -> Just c
				_ -> Nothing
		isSelfSet = case aa of 
			Dot Self{} _ -> True
			_ -> False
		bToProcCall = if isJust tp then D.MathOp (fromJust tp) a b else b
		callSet ldef ref = exprCall env (Just $ unoptionIfChecked $ exprDataType ref) $ D.Call "set" (Just [(Just $ defName ldef, bToProcCall)]) []
	in if isNothing lcall then Set tp (ExpLError "Left is not def" aa) $ expr env b
		else case fromJust lcall of
			Call ldef _ [] _ -> 
				if DefModMutable `elem` defMods ldef then simpleSet 
				else if envInit env && isSelfSet then simpleSet
				else case aa of
					Dot ref _ ->  Dot ref $ callSet ldef ref
					NullDot ref _ _ ->  nullDot env ref $ callSet ldef ref
					_ -> exprCall env Nothing $ D.Call "set" (Just [(Just $ defName ldef, bToProcCall)]) []
			_ -> Set tp (ExpLError "Unassinable left" aa) (expr env b)
expr env (D.PlusPlus e) = PlusPlus (exprToSome env e)
expr env (D.MinusMinus e) = MinusMinus (exprToSome env e)
expr env D.Self = self env
expr env D.Super = Super $ fromMaybe (error $ "No super data type for " ++ show (envSelf env)) $ superType $ envSelf env
expr env r@D.Call{} = maybeInlineCall env $ exprCall env Nothing r
expr env (D.Index e i) = let
	e' = exprToSome env e
	obf = expr env $ D.Dot e (D.Call "apply" (Just [(Nothing, i)]) [])
	in case exprDataType e' of
		TPClass{} -> obf
		(TPGenericWrap _ TPClass{}) -> obf
		_ -> Index e' $ expr env i 
expr env l@(D.Lambda pars e) = 
	if all (isJust.snd) pars then 
		let 
			pars' = map (second (dataType env . fromJust)) pars
			env' =  envAddVals (map (uncurry localVal) pars') env
			mapEnvTp = case envTp env' of
				TPFun _ d -> d
				d -> d
			e' = expr env'{envTp = mapEnvTp} e
			tp = exprDataType e'
		in Lambda pars' (maybeAddReturn env tp e') tp
	else ExpDError "Not all types are defined in lambda" l

expr env (D.Val name tp body mods) = let
	tp' = fmap (dataType env) tp
	body' = expr env{envTp = fromMaybe (baseDataType env) tp'} body
	tp'' = unwrapGeneric $ fromMaybe (exprDataType body') tp'
	body'' = if isJust tp then implicitConvertsion env tp'' body' else body'
	mods' = DefModLocal : [DefModMutable | D.DefModMutable `elem` mods] ++ [DefModWeak | D.DefModWeak `elem` mods] ++ [DefModVolatile | D.DefModVolatile `elem` mods] 
	def' = Def{defName = name, defType = tp'', defMods = mods', defPars = [], 
		defBody = if isTpOption tp'' then body'' else case body'' of
			Nop -> ExpError $ name ++ ": no initialiazation value for non-option"
			_ -> body'', 
		defGenerics = Nothing, defAnnotations = []}
	in declareVal env def'
expr _ (D.Arr []) = Arr []
expr env (D.Arr [e]) = Arr [expr env e]
expr env (D.Arr exprs) = 
	let 
		exprs' = map (expr env) exprs
		tps = map exprDataType exprs'
		commonTp = reduceDataTypes env tps 
	in Arr $ map (\(e, tp) -> if tp == commonTp then e else maybeCast commonTp e) (zip exprs' tps)
expr env (D.Tuple items) = Tuple $ map (expr env) items
expr env (D.Throw e) = Throw (expr env e)
expr env (D.Return e) = Return  True (expr env e)
expr env (D.Not e) = Not (expr env e)
expr env (D.Negative e) = Negative (expr env e)
expr _ D.Break = Break
expr env c@D.Case{} = linkCase env c
expr env s@D.StringBuild {} = linkStringBuild env s
expr env ex@D.FuncOp{} = linkFuncOp env ex
-- expr x = error $ "No expr for " ++ show x
