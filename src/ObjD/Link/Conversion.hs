module ObjD.Link.Conversion (
	implicitConvertsion, lambdaImplicitParameters, 
	maybeAddReturn, addReturn, addReturnBy, maybeCast, declareVal, multilineSet,
	envExprCompileTo, envExprCompileToSome
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Extends
import           Data.List
import qualified ObjD.Struct         as D
import           Ex.String
--import Debug.Trace

envExprCompileTo :: Env -> DataType -> D.Exp -> Exp
envExprCompileTo env tp e = implicitConvertsion env tp $ envExprCompile env{envTp = tp} e

envExprCompileToSome :: Env -> D.Exp -> Exp
envExprCompileToSome env e =  envExprCompile env{envTp = baseDataType env} e


declareVal :: Env -> Def -> Exp
declareVal env d 
	| isSimpleExpression (defBody d) = Val False d
	| otherwise = Val True mappedDef
	where
		mappedDef = d{defBody =  multilineSet env Nothing (callRef mappedDef) (defBody d)}

multilineSet :: Env -> Maybe MathTp -> Exp -> Exp -> Exp
multilineSet env tp l r = addReturnBy s True r
	where
		s _ e = Set tp l $ implicitConvertsion env (exprDataType r) e
		


maybeAddReturn :: Env -> DataType -> Exp -> Exp
--maybeAddReturn _ tp _ | trace ("r " ++ show tp) False = undefined
maybeAddReturn _ TPVoid e = e
maybeAddReturn env tp e  = let
	mbNil ee = case unwrapGeneric $ exprDataType e of
		TPVoid -> case e of
			Braces es -> Braces (es ++ [Return False ee]) 
			_ -> Braces (e : [Return False ee]) 
		_ -> addReturn env True tp e
	in case tp of
		TPGenericWrap _ t@(TPClass TPMGeneric _ _) -> mbNil (None t)
		TPClass TPMGeneric _ _ -> mbNil (None tp)
		TPVoid -> mbNil Nil
		TPGenericWrap _ TPVoid -> mbNil (None TPVoid)
		TPGenericWrap _ t@TPUnknown{} -> mbNil (None t)
		TPUnknown{} -> mbNil (None tp)
		_ -> addReturn env True tp e

addReturn :: Env -> Bool -> DataType -> Exp -> Exp 
addReturn env hard tp ee = addReturnBy defBy hard ee
	where
		defBy h e = Return h $ implicitConvertsion env tp e


addReturnBy :: (Bool -> Exp -> Exp) -> Bool -> Exp -> Exp
addReturnBy by hard (Weak e) = Weak (addReturnBy by hard e)
addReturnBy by hard (If cond t f) = If cond (addReturnBy by hard t) (addReturnBy by hard f)
addReturnBy by hard (Synchronized r b) = Synchronized r (addReturnBy by hard b)
addReturnBy by hard (Try e f) = Try (addReturnBy by hard e) f
addReturnBy _ True e@(Braces []) = ExpLError "Return empty braces" e
addReturnBy by hard (Braces es) = Braces $ map (addReturnBy by False) (init es) ++ [addReturnBy by hard (last es)]
addReturnBy _ True Nop = ExpLError "Return NOP" Nop
addReturnBy _ _ e@(Throw _) = e
addReturnBy by _ (Return _ e) = by True e
addReturnBy _ _ e@While{} = e
addReturnBy by True e = by False e
addReturnBy _ _ e = e


maybeCast :: DataType -> Exp -> Exp
maybeCast _ e@Throw{} = e
maybeCast _ e@NPE = e
maybeCast _ e@(Braces []) = e
maybeCast tp (If c t f) = If c (maybeCast tp t) (maybeCast tp f) 
maybeCast tp (Braces x) = Braces $ (init x) ++ [maybeCast tp $ last x]
maybeCast TPNil e = e
maybeCast (TPGenericWrap _ TPNil) Nil = None TPNil
maybeCast (TPGenericWrap _ TPVoid) Nil = None TPNil
maybeCast (TPGenericWrap _ TPNil) e = e
maybeCast _ Nil = Nil
maybeCast (TPOption ch t) e = 
	let tp = unwrapGeneric $ exprDataType e
	in case tp of
		TPOption _ r -> if unwrapGeneric r == unwrapGeneric t then e else Cast tp e
		_ -> Some ch e
maybeCast tp e 
	| unwrapGeneric tp == unwrapGeneric (exprDataType e) = e
	| otherwise = Cast tp e

implicitConvertsion :: Env -> DataType -> Exp -> Exp
implicitConvertsion _ _ Nop = Nop
implicitConvertsion _ (TPGenericWrap _ TPVoid) Nil = None TPNil
implicitConvertsion _ TPVoid expression = expression
implicitConvertsion env destinationType expression = if isInstanceOfCheck env (exprDataType theResult) destinationType then theResult 
		else ExpLError ("Could not convert " ++ show (exprDataType theResult) ++ " to " ++ show destinationType) theResult
	where
		theResult = implicitConvertsion' destinationType expression
		implicitConvertsion' (TPMap _ _) (Arr []) = Map []
		implicitConvertsion' (TPMap k v) (Arr exps) = Map $ map tup exps
			where
				tup (Tuple [ke, ve]) = (implicitConvertsion env k ke, implicitConvertsion env v ve)
				tup (Cast _ t) = tup t
				tup e = (ExpLError "Not tuple in map" e, ExpLError "Not tuple in map" e)
		implicitConvertsion' _ Nop = Nop
		implicitConvertsion' dtp ex = let stp = exprDataType ex
			in 
				case ex of
					Braces _ -> maybeAddReturn env dtp ex
					If cond l r -> If cond (implicitConvertsion env dtp l) (implicitConvertsion env dtp r)
					_ -> if stp == dtp then mdCheckGens stp dtp else conv stp dtp
			where
				conv (TPGenericWrap _ s) d = conv s d
				conv s (TPGenericWrap _ d) = conv s d
				conv sc@(TPFun _ (TPGenericWrap _ _)) dc@(TPFun _ (TPGenericWrap _ _)) = mdCheckGens sc dc
				conv sc@(TPFun _ _) dc@(TPFun _ fdtp@(TPGenericWrap _ _)) = case ex of
					Lambda lambdaPars le _ -> Lambda lambdaPars  (maybeAddReturn env fdtp le) fdtp
					_ -> mdCheckGens sc dc
				conv sc@(TPFun _ stp) dc@(TPFun _ TPVoid) = if stp == TPVoid then ex else  case ex of
					Lambda lambdaPars le _ -> Lambda lambdaPars  (maybeAddReturn env TPVoid le) TPVoid
					_ -> mdCheckGens sc dc
				conv sc@TPFun{} dc@TPFun{} = mdCheckGens sc dc
				conv _ f@(TPFun _ fdtp) = Lambda (lambdaImplicitParameters f) (maybeAddReturn env fdtp ex) fdtp
				{-conv TPFun{} _ = LambdaCall ex-}
				conv (TPOption True a) bb@(TPOption False b) = Cast bb $ conv a b
				conv (TPOption _ a) (TPOption _ b) = conv a b
				conv TPNil (TPOption _ tp) = None (wrapGeneric tp)
				conv a (TPOption ch b) 
					| a == b = Some ch ex
					| otherwise = Some ch $ conv a b
				conv _ (TPClass TPMGeneric _ _) = ex
				conv (TPNumber _ _) TPString = Cast TPString ex
				conv (TPFloatNumber _ ) TPString = Cast TPString ex
				conv (TPNumber s1 l1) d@(TPNumber s2 l2) = if s1 /= s2 || l1 /= l2 then Cast d ex else ex
				conv (TPFloatNumber l1) d@(TPFloatNumber l2) = if l1 /= l2 then Cast d ex else ex
				conv TPFloatNumber{} d@TPNumber{} = Cast d ex
				conv TPNumber{} d@TPFloatNumber{} = Cast d ex
				conv (TPArr _ _) d@(TPEArr _ _) = Cast d ex
				conv sc@(TPTuple _) dc@(TPTuple dtps) = case ex of
					Tuple exps -> Tuple $ zipWith (implicitConvertsion env) dtps exps
					(Cast tp (Tuple exps)) -> maybeCast tp $ Tuple $ zipWith (implicitConvertsion env) dtps exps
					_ -> mdCheckGens sc dc
				conv sc@(TPArr _ _) dc@(TPArr _ adtp) = case ex of
					Arr [] -> Cast dc ex
					Arr exps -> Arr $ map (implicitConvertsion env adtp) exps
					_ -> mdCheckGens sc dc
				conv (TPArr _ _) (TPClass _ [d] Class{className = "PArray"}) = Cast (TPEArr 0 (unwrapGeneric d)) ex
				conv sc dc@TPClass{} = if isInstanceOfTp env sc dc then mdCheckGens sc dc else classConversion dc sc
				conv _ _ = ex

				classConversion c (TPGenericWrap _ g) = classConversion c g
				classConversion dc@(TPClass _ gens cls) sc =
					maybe (mdCheckGens sc dc) wrapWithApply $
						find(checkApplyPars . defPars ).
						map (replaceGenericsInDef gens') .
						filter(\d -> 
							(defName d == "apply") 
							&& (DefModStatic `elem` defMods d)) $ (classDefs cls)
					where
						gens' = buildGenerics cls gens
						checkApplyPars [Def{defType = tp}] = isInstanceOfTp env sc tp
						checkApplyPars _ = False
						od = objectDef cls
						wrapWithApply apply@Def{defPars = [par]} = Dot (Call od (defType od) [] []) (Call apply dtp [(par, ex)] [])
				classConversion t sc = ExpLError (show t ++ " from " ++ show sc) ex


				mdCheckGens sc dc = if envLang env == Java then checkGens sc dc else ex
				
				--checkGens sc dc | trace ("checkGens: " ++ show ex ++ ": " ++ show sc ++ " <<>>" ++ show dc)  False = undefined
				checkGens sc dc 
					| isValidDataType dc = if eqTps (dataTypeGenerics sc) (dataTypeGenerics dc) then ex else Cast dc ex
					| otherwise = ex

				eqTps [] [] = True
				eqTps (s:ss) (d:ds) = eqTp s d && eqTps ss ds
				eqTps _ _ = False

				eqTp s (TPGenericWrap _ d) = eqTp s d
				eqTp s (TPOption True d) = eqTp s d
				eqTp (TPGenericWrap _ s) d = eqTp s d
				eqTp (TPOption True s) d = eqTp s d
				eqTp (TPClass _ sgs s) (TPClass _ dgs d) = s == d && eqTps sgs dgs
				eqTp TPAnyGeneric _ = False
				eqTp _ TPAnyGeneric = False
				eqTp s d = s == d

lambdaImplicitParameters :: DataType -> [(String, DataType)]
lambdaImplicitParameters (TPFun [] _) = []
lambdaImplicitParameters (TPFun [fstp] _) = [("_", fstp)]
lambdaImplicitParameters (TPFun stps _) = (map(\(tp, i) -> ('_' : show i, tp)) . zipWithIndex) stps


