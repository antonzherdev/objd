module ObjD.Link.Option (
	nonOpt, nullDot, linkOptionCall, linkOptionAlt, optChecking, compareWithNil, compareOptions, compareOptionWithNonOption, linkNullDot
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Conversion
import 			 ObjD.Link.Call
import           Ex.String
import qualified ObjD.Struct         as D

nonOpt :: Env -> Bool -> Exp -> Exp
nonOpt _ False e = NonOpt False e e
nonOpt env True e = let
	tp = exprDataType e
	tp' = unoptionHard $ tp
	rt w = If (BoolOp Eq w (None tp')) NPE w
	val = tmpVal env "n" tp e
	in NonOpt True e $ if isElementaryExpression e then rt e else Braces [Val False val, rt (callRef val)]

nullDot :: Env -> Exp -> Exp -> Exp
nullDot env l r = let
	ltp = exprDataType l
	ltp' = unoptionHard ltp
	rtp = exprDataType r
	val = tmpVal env "u" ltp l
	f l' = If (BoolOp NotEq l' (None ltp')) (Some True $ Dot (nonOpt env False l') r) (None rtp)
	uw 
		| isElementaryExpression l = f l
		| otherwise = Braces [Val False val, f (callRef val)]
	in NullDot l r uw
linkOptionCall :: Env -> (D.Exp, Exp) -> D.Exp -> Exp
linkOptionCall env (_, leftExp) (D.Call "get" Nothing []) = nonOpt env True $ leftExp
linkOptionCall env (_ ,leftExp) e@(D.Call "cast" Nothing [tp]) = case tp of
	D.DataTypeOption{} -> Cast (dataType env tp) leftExp 
	_ -> ExpDError ("Cast option to non-option: " ++ show tp) e
linkOptionCall env (l, _) (D.Call "getOr" (Just [(_, alt)]) []) = 
	let
		l' = envExprCompile env{envVarSuffix = envVarSuffix env ++ "_e1"} l
		tp = unoptionHard $ exprDataType l'
		alt'' = linkOptionAlt env alt tp
	in linkOrElse env (tp, False) (l, l') alt''
linkOptionCall env (l, _) (D.Call "or" (Just [(_, alt)]) []) = 
	let
		l' = envExprCompile env{envVarSuffix = envVarSuffix env ++ "_e1"} l
		tp = unoptionHard $ exprDataType l'
		alt'' = linkOptionAlt env alt (option False tp)
	in linkOrElse env (tp, True) (l, l') alt''
linkOptionCall env (_, l') (D.Call fname (Just [(_, e)]) []) 
	| fname == "map" || fname == "for" || fname == "flatMap" = let
		aTp = unoptionHard $ exprDataType l'
		tmp :: Def
		tmp = localValE mapVarName (option True aTp) l'
		mapVarName = case e of
			D.Lambda [(name, _)] _ -> name 
			_ -> "_"
		mapExpr = case e of
			D.Lambda _ le -> le
			_ -> e
		mapExpr' = envExprCompile (envAddVals [tmp] env) mapExpr
		mapExpr'' = case unwrapGeneric $ exprDataType mapExpr' of
			TPFun{}-> case mapExpr' of
				Lambda{} -> mapExpr'
				_ -> dotCall env mapExpr' "apply" [] [callRef tmp] 
			_ -> mapExpr'
		bTp = case fname of
			"for" -> TPVoid
			"map" -> exprDataType mapExpr''
			"flatMap" -> unoptionHard $ exprDataType mapExpr''
	in	Braces [
			declareVal env tmp, 
			(case unwrapGeneric $ envTp env of
				TPVoid -> If (BoolOp NotEq (callRef tmp) (None aTp) ) 
					mapExpr'' Nop
				_ -> If (BoolOp NotEq (callRef tmp) (None aTp) ) 
					(implicitConvertsion env (option False bTp) mapExpr'')
					(None $ wrapGeneric $ bTp))
		]			
linkOptionCall _ _ e = ExpDError ("Unknown option operation: " ++ show e) e

linkOptionAlt :: Env -> D.Exp -> DataType -> Exp
linkOptionAlt env alt tp = implicitConvertsion env tp $ envExprCompile env{envVarSuffix = envVarSuffix env ++ "_e2"} alt'
	where alt' = case alt of
			(D.Lambda [] a) -> a
			_ -> alt

linkOrElse :: Env -> (DataType, Bool) -> (D.Exp, Exp) -> Exp -> Exp
linkOrElse env _ ((D.NullDot dl dr), (NullDot dl' dr' _)) alt =
	let
		l'' = Dot (nonOpt env False dl') dr'
		dltp = exprDataType dl'
		tmp = tmpVal env "" dltp dl'
	in if isElementaryExpression dl' then If (BoolOp NotEq dl' (None dltp) ) (envExprCompile env (D.Dot (D.Dot dl (D.Call "get" Nothing [])) dr)) alt
		else Braces[
			declareVal env tmp,
			If (BoolOp NotEq (callRef tmp) (None dltp)) l'' alt
		]
linkOrElse env (tp, isOptionAlt) (_, l') alt = 
	let 
		tmp = tmpVal env "" (option False tp) $ implicitConvertsion env (option False tp) l'
		e r = If (BoolOp NotEq r (None tp)) ((if isOptionAlt then id else nonOpt env False) r) alt
	in if isElementaryExpression l' then e l' else Braces[declareVal env tmp, e (callRef tmp)]

linkNullDot :: Env -> D.Exp -> Exp
linkNullDot env d@(D.NullDot a b) = let
	aa = case a of
		D.Call {} -> exprCall (envAddSuffix env "l") Nothing a
		_ -> envExprCompileToSome (envAddSuffix env "l") a
	aTp = exprDataType aa
	bb = case aTp of
		TPOption _ tp -> exprCall (envAddSuffix env "r") (Just tp) b
		TPGenericWrap _ (TPOption _ tp) -> exprCall (envAddSuffix env "r") (Just tp) b
		tp -> ExpDError ("Null safe operation for the non-nullable datatype " ++ show tp) b	
	in case aa of
		ExpDError s _ -> ExpDError s d
		_ -> case bb of
			Dot l r -> nullDot env (nullDot (envAddSuffix env "i") aa l) r
			_ -> nullDot env aa bb

optChecking :: Env -> Exp -> (Env, Env)
optChecking env e =  rec (env, env) e
	where
		mapEnvDef en d = case unwrapGeneric $ defType d of
			TPOption False tp -> envChangeDefTp en d (TPOption True tp)
			_ -> en
		mapEnv en (Call d _ [] _) = mapEnvDef en d
		mapEnv en (Dot (Self _) (Call d _ [] _)) = mapEnvDef en d
		mapEnv en _ = en
		rec :: (Env, Env) -> Exp -> (Env, Env)
		rec en (BoolOp And a b) = rec (rec en a) b
		rec (l, r) (BoolOp Eq ee (None _)) = (l, mapEnv r ee)
		rec (l, r) (BoolOp NotEq ee (None _)) = (mapEnv l ee, r)
		rec (l, r) (BoolOp Eq (None _) ee) = (l, mapEnv r ee)
		rec (l, r) (BoolOp NotEq (None _) ee) = (mapEnv l ee, r)
		rec en _ = en

compareWithNil :: Env -> BoolTp -> (Exp, DataType) -> Exp 
compareWithNil _ btp (e, etp) = case etp of
	TPOption _ tp -> BoolOp btp e (None tp)
	_ -> ExpLError "Non-option compares with nil" e

compareOptions :: Env -> BoolTp -> (Exp, DataType) -> (Exp, DataType) -> Exp
compareOptions env btp (l, ltp) (r, rtp) = let
	comp l' r' = case btp of
		Eq -> BoolOp Or 
			(BoolOp ExactEq l' r') 
			(BoolOp And 
				(BoolOp And (BoolOp NotEq l' (None $ unoptionHard ltp)) (BoolOp NotEq r' (None $ unoptionHard rtp))) 
				(BoolOp Eq l' r') )
		NotEq -> BoolOp And 
			(BoolOp ExactNotEq l' r') 
			(BoolOp Or 
				(BoolOp Or (BoolOp Eq l' (None $ unoptionHard ltp)) (BoolOp Eq r' (None $ unoptionHard rtp))) 
				(BoolOp NotEq l' r') )
	isSimpleL = isElementaryExpression l
	isSimpleR = isElementaryExpression r
	lval = tmpVal env "_l" ltp l
	rval = tmpVal env "_r" rtp r
	ml = Braces $
		[Val False lval | not isSimpleL]
		++ [Val False rval | not isSimpleR]
		++ [comp (if isSimpleL then l else callRef lval) (if isSimpleR then r else callRef rval)]
	in if isSimpleL && isSimpleR then comp l r else ml

compareOptionWithNonOption :: Env -> BoolTp -> (Exp, DataType) -> (Exp, DataType) -> Exp
compareOptionWithNonOption env btp (opt, optTp) (nonopt, _) = let 
	comp opt' nonopt' = case btp of
		Eq -> BoolOp And (BoolOp NotEq opt' (None $ unoptionHard optTp)) (BoolOp Eq opt' nonopt') 
		NotEq -> BoolOp Or (BoolOp Eq opt' (None $ unoptionHard optTp)) (BoolOp NotEq opt' nonopt') 
	isSimpleOpt = isElementaryExpression opt
	val = tmpVal env "" optTp opt
	ml = Braces [Val False val,
		comp (callRef val) nonopt]
	in if isSimpleOpt then comp opt nonopt else ml
