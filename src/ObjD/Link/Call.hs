module ObjD.Link.Call (
	dotCall, exprCall, insertWeak
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Conversion
import 			 ObjD.Link.Extends
import           Ex.String
import           Data.Maybe
import           Data.List
import           Control.Monad.State
import 			 Control.Arrow
import qualified ObjD.Struct         as D
import qualified Data.Map            as M
-- import Debug.Trace


detailedReferenceError :: Bool
detailedReferenceError = False

dotCall :: Env -> Exp -> String -> [DataType] -> [Exp] -> Exp
dotCall env l nm gens pars = let 
		tp = exprDataType l 
		cl = dataTypeClass env tp
	in  
		fromMaybe (ExpError $ "Could not find \"" ++ nm ++ "\" in " ++ show tp ++ " for " ++ show l)
		$ fmap (\d -> Dot l $ Call d (defType d) (zip (defPars d) (pars)) gens) 
		$ find (\d -> defName d == nm && length (defPars d) == length pars) 
		$ allDefsInClass (cl, M.empty)


classTypeClass :: DataType -> DataType
classTypeClass (TPClass TPMClass [g] Class{className = "ClassType"}) = g
classTypeClass (TPClass TPMClass [g] Class{className = "PType"}) = g
classTypeClass _ = TPUnknown "Not Type"

exprCall :: Env-> Maybe DataType -> D.Exp -> Exp
exprCall _ (Just (TPUnknown t)) e = ExpDError t e
exprCall env (Just _) (D.Call "as" Nothing [tp]) = As $ dataType env tp
exprCall env (Just _) (D.Call "is" Nothing [tp]) = Is $ dataType env tp
exprCall env (Just _) (D.Call "is" (Just [(_, e)]) [tp]) = IsTp (dataType env tp) (envExprCompile env e)
exprCall env (Just _) (D.Call "is" (Just [(_, e)]) _) = let e' = envExprCompile env e in IsTp (classTypeClass $ exprDataType e') e' 
exprCall env (Just _) (D.Call "as" (Just [(_, e)]) [tp]) = AsTp (dataType env tp) (envExprCompile env e)
exprCall env (Just _) (D.Call "as" (Just [(_, e)]) _) = let e' = envExprCompile env e in AsTp (classTypeClass $ exprDataType e') e' 

exprCall env (Just _) (D.Call "cast" Nothing [tp]) = CastDot $ dataType env tp
exprCall env strictClass cll@(D.Call name pars gens) = 
	case tryExprCall env strictClass cll of
		err@ExpDError{} -> if isNothing pars then err else
			case tryExprCall env strictClass (D.Call name Nothing gens) of
				ExpDError es _ -> ExpLError es err
				e -> case tryExprCall env (Just $ unoptionIfChecked $ exprDataType e) (D.Call "apply" pars gens) of
					ExpDError es _ -> ExpLError es err
					ee -> Dot e ee
		e -> e
		
exprCall _ _ err = ExpDError "It is not call" err

tryExprCall :: Env-> Maybe DataType -> D.Exp -> Exp
tryExprCall _ (Just (TPUnknown t)) e = ExpDError t e
tryExprCall env strictClass cll@(D.Call name pars gens) = maybeLambdaCall
	where
		pars' = mapPars 0 (fromMaybe [] pars)
			where
				mapPars :: Int -> [D.CallPar] -> [(Maybe String, Exp)]
				mapPars _ [] = []
				mapPars i ((n, e):xs) = (n, FirstTry e (envExprCompileToSome (envAddSuffix env $ 'p' : show i) e)) : mapPars (i + 1) xs
		selfTp = fromMaybe (envSelf env) strictClass
		call' :: (Maybe Class, Exp)
		call' = fromMaybe (Nothing, ExpDError errorString cll) $ listToMaybe $ mplus (findCall True) (findCall False)
		call'' :: Exp
		call'' = case call' of
			(cl, cc@Call{}) -> (resolveDef strictClass cl . correctCall) cc
			_ -> snd call'
			where
				resolveDef Nothing cl c@(Call d _ _ _)
					| DefModConstructor `elem` defMods d = c
					| DefModObject `elem` defMods d = c
					| DefModLocal `elem` defMods d = c
					| isJust cl = Dot (callRef $ objectDef $ fromJust cl) c
					| otherwise = Dot (self env) c
				resolveDef _ _ c = c

		{-tr r | trace ("tr: " ++ maybe "_" (className . dataTypeClass env) strictClass ++ "." ++ show cll ++ " = " ++ show r) False = undefined
		tr r = r-}
		{-checkedCall = case call'' of
			(Call d tp cpars) ->
				Call d tp (map (checkPar d) cpars)
			_ -> call''
			where 
				checkPar _ par@(Def{defType = dtp, defName = dname}, e) = 
					let tp = (exprDataType e)
					in if isInstanceOfCheck env tp dtp then par else (fst par, 
						ExpLError (show tp ++ " is not instance of " ++ show dtp ++ " in parameter \"" ++ dname ++ "\"") call'')
-}
		maybeLambdaCall = case pars of
			Just [] -> case unwrapGeneric $ exprDataType call'' of
				(TPFun [] _) -> LambdaCall call''
				TPOption True (TPGenericWrap _ (TPFun [] _))-> LambdaCall call''
				TPOption True (TPFun [] _) -> LambdaCall call''
				_ -> call''
			_ -> call''

		

		pars'' :: [(Def, Exp)]
		pars'' = case snd call' of
			Call _ _ r _ -> r
		pars''' :: [(Def, Exp)]
		pars''' = (map (correctCallPar . md ) pars'')
			where 
				md (d, e) = (unwrapGeneric $ unoption $ defType d, d, e)
		
		gens' :: Generics
		gens' = resolveGenerics pars''

		gens'' :: Generics
		gens'' = resolveGenerics pars'''

		

		resolveGenerics :: [(Def, Exp)] -> Generics
		resolveGenerics rpars = let
			extendList :: Int -> [a] -> [Maybe a]
			extendList l a
				| length a == l = map Just a
				| length a > l = (map Just . take l) a
				| otherwise = map Just a ++ replicate (l - length a) Nothing 
			ddefGenerics :: DefGenerics -> [(String, DataType)]
			ddefGenerics DefGenerics{defGenericsClasses = defGens, defGenericsSelfType = selfType} = 
				(zipWith (determineGenericType selfType) defGens . extendList (length defGens)) gens
			srcClassGenerics = classGenerics $ dataTypeClass env selfTp
			dclassGenerics :: [(String, DataType)]
			dclassGenerics = (zipWith extractGen srcClassGenerics . extendList (length srcClassGenerics)) (dataTypeGenerics selfTp) 
				where 
					extractGen :: Class -> Maybe DataType -> (String, DataType)
					extractGen g (Just t) = (className g, t)
					extractGen g Nothing = (className g, TPUnknown $
						"Could not find generic type for " ++ show g ++ " in self " ++ show selfTp ++ " for call " ++ show call')
				
			determineGenericType :: DataType -> Class -> Maybe D.DataType -> (String, DataType)
			determineGenericType _ g (Just tp) = (className g, (wrapGeneric . dataType env) tp)
			determineGenericType selfType g  _ = (className g, 
					fromMaybe (TPUnknown errorText) $ 
					mplus determineBySelfType determineByPars
				)
				where 
					determineByPars :: Maybe DataType
					determineByPars = (listToMaybe . mapMaybe ( (defType *** (exprDataType >>> unwrapGeneric))>>> tryDetermine g) ) rpars
					determineBySelfType :: Maybe DataType
					determineBySelfType = tryDetermine g (selfType, selfTp)
					errorText = "Could not determine generic type for " ++ show g ++ " in " ++ show cll 
					tryDetermine :: Class -> (DataType, DataType) -> Maybe DataType
					tryDetermine c (TPGenericWrap _  a, TPGenericWrap _ b) = tryDetermine c (a, b)
					tryDetermine c (TPGenericWrap _ a, b) = tryDetermine c (a, b)
					tryDetermine c (a, TPGenericWrap _ b) = tryDetermine c (a, b)
					tryDetermine c (TPClass TPMGeneric _ gg, tp) = if c == gg then Just (wrapGeneric tp) else Nothing
					tryDetermine c (TPArr _ a, TPArr _ a') = tryDetermine c (a, a')
					tryDetermine c (TPOption _ a, TPOption _ a') = tryDetermine c (a, a')
					tryDetermine c (TPOption _ a, a') = tryDetermine c (a, a')
					tryDetermine c (TPMap a b, TPMap a' b') = mplus (tryDetermine c (a, a')) (tryDetermine c (b, b'))
					tryDetermine c (TPTuple a, TPTuple a') = listToMaybe $ mapMaybe (tryDetermine c) (zip a a')
					tryDetermine c (TPClass _ gg cl, TPClass _ gg' cl') = 
						listToMaybe $ mapMaybe (tryDetermine c) (zip gg gg'')
						where
							gg'' = fromMaybe [] $ upGenericsToClass cl (cl', gg')
					tryDetermine c (cl@(TPClass _ _ _), TPObject m cl') = 
						tryDetermine c (cl, TPClass TPMClass [TPClass m [] cl'] (classFind (envIndex env) "Class"))
					tryDetermine c (TPFun a b, TPFun a' b') = listToMaybe $ catMaybes ( map (tryDetermine c)  (zip a a') ++ [tryDetermine c (b, b')])
					tryDetermine c (a, b@TPArr{}) = tryDetermine c (a, dtpw b)
					tryDetermine c (a, b@TPMap{}) = tryDetermine c (a, dtpw b)
					tryDetermine _ _ = Nothing
					dtpw tp = TPClass TPMGeneric (dataTypeGenerics tp) (dataTypeClass env tp) 
			in case snd call' of
				(Call Def{defGenerics = Just defGens} _ _ _) -> 
					M.fromList $ ddefGenerics defGens ++ dclassGenerics
				_ -> M.fromList dclassGenerics 
								
		errorString :: String
		errorString = "Could find reference for call " ++ callStr ++ "\n" ++
			maybe "" (\cl -> "strict in class " ++ show cl ++ "\n") strictClass ++
			(if detailedReferenceError then "in defs:\n" ++
			(strs "\n" . map (ind . showDef False. snd)) (allDefs)  else "")
			where callStr = name ++ maybe "" (\ps -> "(" ++ strs ", " (map ((++ ":") . fromMaybe "" . fst) ps) ++ ")" ) pars

		correctCall :: Exp -> Exp
		correctCall (Call d tp _ _) = 
			Call d (replaceGenerics True gens'' tp) (map doImplicitConversation pars''') 
				(fromMaybe [] $ fmap cordg $ defGenerics d)
			where
				cordg = map (replaceGenerics True gens'' . TPClass TPMGeneric []) . defGenericsClasses
				doImplicitConversation (dd, e) = (dd, implicitConvertsion env (replaceGenerics True gens'' $ defType dd) e)

		allDefs :: [(Maybe Class, Def)]
		allDefs = maybe allDefsInEnv allDefsInStrictClass strictClass
			where 
			non f = map (\d -> (Nothing, d)) f
			clRef = dataTypeClassRef env (fromJust strictClass)
			allDefsInStrictClass _ = 
				non (allDefsInClass clRef)
				++ non (allDefsInObject clRef)
			allDefsInEnv = non(envVals env)
				++ non (allDefsInClass (dataTypeClassRef env $ envSelf env) )
				++ non (allDefsInObject (dataTypeClassRef env $ envSelf env) )
				++ concatMap (\cl -> map (\d -> (Just cl, d)) $ classStaticDefs cl) (envObjectIndex env)
				++ non objects 
			objects = if isNothing pars then (map (objectDef . snd) . M.toList) (envIndex env) else []
		
		findCall :: Bool -> [(Maybe Class, Exp)]
		findCall hard = (mapMaybe fit . filter ((== name) . defName . snd)) allDefs
			where
				fit :: (Maybe Class, Def) -> Maybe (Maybe Class, Exp)
				fit (cl, d)
					| length pars' == length (defPars d) = 
							if checkParameters pars' (defPars d) then Just $ (cl, def' d) else Nothing
					| otherwise = Nothing

				def' d = Call d (resolveTp d) (zipWith (\dp (_, e) -> (dp, e) ) (defPars' d) pars') []
				resolveTp d = case defType d of
					TPSelf _ -> selfTp
					TPOption True tp@(TPFun _ dtp)-> if length pars' == length (defPars d) then tp else dtp
					tp@(TPFun _ dtp)-> if length pars' == length (defPars d) then tp else dtp
					tp -> tp
				defPars' :: Def -> [Def]
				defPars' Def{defType = t, defPars = []} = dataTypePars t
				defPars' Def{defPars = r} = r
				checkParameters :: [(Maybe String, Exp)] -> [Def] -> Bool
				checkParameters cp dp = all (checkParameter) $ zip cp dp
				checkParameter :: ((Maybe String, Exp), Def) -> Bool
				checkParameter ((Just cn, e), Def{defName = dn, defType = tp}) = cn == dn && checkDataType tp (exprDataType e)
				checkParameter ((_, e), Def{defType = tp}) = checkDataType tp (exprDataType e)
				checkDataType dtp tp = if hard then isInstanceOfTp env tp dtp else True

		correctCallPar :: (DataType, Def, Exp) -> (Def, Exp)
		correctCallPar (tp@(TPFun _ _), d, FirstTry _ e'@Lambda{}) = correctCallPar (tp, d, e')
		correctCallPar (tp@(TPFun _ _), d, FirstTry D.Lambda{} e') = correctCallPar (tp, d, e')
		correctCallPar (tp@(TPFun _ _), d, FirstTry e e')
			| isTpFun (exprDataType e') || (isTpOption (defType d) && isTpFun (unoption $ exprDataType e')) = checkCallParOnWeak (d, e')
			| otherwise = correctCallPar (tp, d, ExpDError "" $ 
				D.Lambda (map (\(n, _) -> (n, Nothing)) $ lambdaImplicitParameters tp) e)
		correctCallPar (tp, d, FirstTry _ e) = correctCallPar (tp, d, e)
		correctCallPar (TPFun _ TPVoid, d, Lambda lpars e dtp) = (d, Lambda lpars (mapExp removeReturn e) dtp)
			where
				removeReturn ee@(Return _ Nil) = Just ee
				removeReturn (Return _ ee) = Just ee
				removeReturn l@Lambda{} = Just l
				removeReturn _ = Nothing
		correctCallPar (TPFun _ (TPClass TPMGeneric _ _), d, Lambda lpars e dtp) = checkCallParOnWeak (d, Lambda lpars e dtp)
		correctCallPar (TPFun stp dtp, d, ExpDError _ (D.Lambda lambdaPars lambdaExpr)) = checkCallParOnWeak (d, Lambda lpars' expr' tp')
			where
				lpars' :: [(String, DataType)]
				lpars' = map (second (replaceGenerics True gens' )) $ zip (map fst lambdaPars) stp
				env' = envAddVals (map (uncurry localVal) lpars') $ env{envTp = dtp'}
				dtp' = replaceGenerics True gens' dtp
				expr' = if dtp' == TPVoid then envExprCompile env' lambdaExpr else maybeAddReturn env dtp' $ envExprCompile env' lambdaExpr
				tp' = if containsGeneric dtp then wrapGeneric (exprDataType expr') else dtp
				containsGeneric = fromMaybe False . forDataType (\t -> case t of
					TPClass TPMGeneric _ _ -> Just True
					_ -> Nothing)
		correctCallPar (_, d, e) = checkCallParOnWeak (d, e)

checkCallParOnWeak :: (Def, Exp) -> (Def, Exp)
checkCallParOnWeak (d@Def{defMods = mods}, e) = 
	if DefModWeak `elem` mods then (d, insertWeak e)
	else (d, e)

insertWeak :: Exp -> Exp
insertWeak e = mapExp f e
	where
		f (Lambda p1 ee p2) = Just $ Lambda p1 (Weak ee) p2
		f _ = Nothing

