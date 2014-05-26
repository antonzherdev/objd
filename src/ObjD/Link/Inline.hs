module ObjD.Link.Inline (
	maybeInlineCall, inlineCall 
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import qualified Data.Map            as M
import 			 Control.Arrow
import           Data.Maybe

maybeInlineCall :: Env -> Exp -> Exp
maybeInlineCall env e = let
	callExpOpt = case e of
		Dot _ c@Call{} -> Just c
		Call{} -> Just e
		_ -> Nothing
	defOpt = case callExpOpt of 
		Just (Call dd _ _ _) -> Just dd
		Nothing -> Nothing
	in if isJust defOpt && DefModInline `elem` defMods (fromJust defOpt) then inlineCall env{envVarSuffix = "__il_" ++ envVarSuffix env} e else e

inlineCall :: Env -> Exp -> Exp
inlineCall env e = let
	callExpOpt = case e of
		Dot _ c@Call{} -> Just c
		Call{} -> Just e
		_ -> Nothing
	(Call def _ pars callGens) = fromJust callExpOpt 
	pars' = if DefModStatic `notElem` defMods def then selfPar : pars else pars
	selfExp = case e of Dot l _ -> l
	selfTp = exprDataType selfExp
	selfClass = dataTypeClass env selfTp
	selfPar = (localVal "self" selfTp, selfExp)
	declaredVals :: [Def]
	declaredVals = forExp findVal $ defBody def
		where 
			findVal (Val _ d) = [d]
			findVal _ = []

	defClass :: Class
	defClass = fromJust $ defClassRec selfClass
		where defClassRec cl
			| def `elem` classDefs cl = Just cl
			| otherwise = listToMaybe $ mapMaybe defClassRec $ map fst $ extendsRefs $ classExtends cl

	gens :: Generics
	gens = M.union classGensMap defGensMap
		where
			classGensMap = buildGenerics defClass $ fromJust $ upGenericsToClass defClass ((dataTypeClass env selfTp), (dataTypeGenerics selfTp))
			defGensMap = M.fromList $ zip (maybe [] (map className . defGenericsClasses) $ defGenerics def) callGens
		
	mapDeclaredValsGenerics = map repGens declaredVals
		where repGens d = (d, d{defType = repgens $ defType d, 
			defName = envVarSuffix env ++ defName d})
	
	repgens = replaceGenerics False gens . unblockGenerics

	replacedExp = mapExp rep $ defBody def
	rep :: Exp -> Maybe Exp
	rep (Dot (Call d tp [] _) (Call Def{defMods = mbLamdaMods} _ lambdaCallPars _)) 
		| DefModApplyLambda `elem` mbLamdaMods = fmap (unwrapLambda (map (mapExp rep . snd) lambdaCallPars)) $ findRef d tp
	rep (LambdaCall (Call d tp [] _)) = fmap (unwrapLambda []) $ findRef d tp
	rep (Call d tp [] _) 
		| DefModField `elem` defMods d || DefModLocal `elem` defMods d = findRef d tp
	rep (Call d tp cpars cgens) =
		Just $ Call d 
			(repgens tp) 
			(map (second $ mapExp rep) cpars)
			(map repgens cgens)
	rep (Val b dd) = 
		fmap (\d -> Val b $ d{defBody = mapExp rep (defBody d)} ) $ lookup dd mapDeclaredValsGenerics
	rep (Self _) = lookup (fst selfPar) refs
	rep (Is tp) = Just $ Is $ repgens tp
	rep (As tp) = Just $ As $ repgens tp
	rep (CastDot tp) = Just $ CastDot $ repgens tp
	rep (Cast tp ee) = Just $ Cast (repgens tp) (mapExp rep ee)
	rep _ = Nothing

	findRef :: Def -> DataType -> Maybe Exp
	findRef d tp = fmap checkOpt $ lookup d refs
		where
			checkOpt ee = case tp of 
				TPOption True _ -> case ee of
					Call d' (TPOption False otp) [] [] -> Call d' (TPOption True otp) [] []
					_ -> ee
				_ -> ee

	refs :: [(Def, Exp)]
	refs = (map (second callRef) vals) ++ pars' ++ map (second callRef) mapDeclaredValsGenerics
	vals = (map dec parsForDeclareVars)
	dec (d, pe) = (d, tmpVal env (defName d) (repgens $ defType d) pe)
	unwrapLambda :: [Exp] -> Exp -> Exp
	unwrapLambda [] (Lambda _ ee _) = ee
	unwrapLambda parExps (Lambda lpars ee _) = let
		nonelemPars = (map (\((nm, tp), lpe) -> Val False $ localValE nm tp lpe) $ filter (not . isElementaryExpression . snd ) (zip lpars parExps))
		elemPars = map (first fst) $ filter (isElementaryExpression . snd ) (zip lpars parExps)
		ee' = case elemPars of
			[] -> ee
			_ -> mapExp replaceOnEP ee
		replaceOnEP (Call d _ [] _) = lookup (defName d) elemPars
		replaceOnEP _ = Nothing
		in case nonelemPars of
			[] -> ee'
			_ -> Braces $ nonelemPars ++ [ee']
	unwrapLambda [] ee = LambdaCall ee
	unwrapLambda p ee = case unwrapGeneric $ exprDataType ee of
		tp@TPFun{} -> Dot ee $ call (applyLambdaDef tp) p
		_ -> ee
		
	parsForDeclareVars = filter (checkCountOfUsing . fst) unelementaryPars
		where 
			unelementaryPars = filter (not . isElementaryExpression . snd) pars'
			checkCountOfUsing d = (length $ filter (d ==) usingDefs) > 1
			usingDefs = forExp findUsage (defBody def)
			findUsage (Call d _ [] _) = [d]
			findUsage (Self _) = [fst selfPar]
			findUsage _ = []
	in if null parsForDeclareVars then replacedExp
		else Braces $ (map (Val False . snd) vals) ++ [replacedExp]
