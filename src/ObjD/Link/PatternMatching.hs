module ObjD.Link.PatternMatching (
	linkCase
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Extends
import 			 ObjD.Link.Conversion
import 			 ObjD.Link.Call
import           Control.Monad.State
import           Data.List
import qualified ObjD.Struct         as D

data CaseEnv = CaseEvn{caseEnvEnv :: Env, caseEnvCurrentVal :: Def, caseEnvValNum :: Int, caseEnvDefs :: [Def]}
 
caseEnvIncVal :: CaseEnv -> CaseEnv
caseEnvIncVal env = env {caseEnvValNum = caseEnvValNum env + 1}
caseEnvAddDef :: Def -> CaseEnv -> CaseEnv
caseEnvAddDef d env = env {caseEnvDefs = caseEnvDefs env ++ [d]}

linkCase :: Env -> D.Exp -> Exp
linkCase env (D.Case mainExpr items) = 
	let 
		mainExpr' = envExprCompile env mainExpr
		_case = (localVal "__case__" (exprDataType mainExpr')) {defBody = mainExpr'}
	 	_incomplete = (localVal "__incomplete__" TPBool) {defBody = BoolConst True}
	 	_ok = (localVal "__ok__" TPBool) {defBody = BoolConst True}
	 	notOk = Set Nothing (callRef _ok) (BoolConst False)
	 	isOk = callRef _ok
		_result = (localVal "__result__" TPVoid)
		caseEnvVal caseEnv = "__case" ++ show (caseEnvValNum caseEnv) ++ "__"
		linkCaseItem :: D.CaseItem -> (Exp, DataType)
		linkCaseItem (cond, e) = 
			let 
				(ex, caseEnv) = runState (linkCaseCond cond) $ CaseEvn env _case 1 []
				caseDefs = caseEnvDefs caseEnv
				vars = map (declareVal env) caseDefs
				env' = envAddVals caseDefs env
				itemExpr = envExprCompile env' e
				setResultTo to = Set Nothing (callRef _result) to
				setResult = case itemExpr of
					Braces exprs -> init exprs ++ [setResultTo $ last exprs]
					ee -> [setResultTo ee]

			in  (If (callRef _incomplete) (Braces $ 
				declareVal env _ok : vars ++ [
					ex, 
					If isOk (Braces $ setResult ++ [Set Nothing (callRef _incomplete) (BoolConst False)])
						Nop]) Nop, exprDataType itemExpr)
		linkCaseCond :: D.CaseCondition -> State CaseEnv Exp
		linkCaseCond (D.CaseUnapply _ "" pars) = do
			caseEnv <- get
			let
				val = caseEnvCurrentVal caseEnv
				valTp = defType val
				env' = caseEnvEnv caseEnv
				valCl = dataTypeClass env' valTp
				constr = classConstructor valCl

				linkPar :: (Def, D.CaseCondition) -> State CaseEnv [Exp]
				linkPar (_, D.CaseAny) = return []
				linkPar (d@Def{defType = newTp}, D.CaseVal valName)= do
					let newVal = localVal valName newTp
					modify $ caseEnvAddDef newVal
					return [Set Nothing (callRef newVal) $Dot (callRef val) (callRef d)]
				linkPar (d@Def{defType = newTp}, cond)= do
					let newVal = localVal (caseEnvVal caseEnv) newTp
					modify caseEnvIncVal
					e <- get
					put e{caseEnvCurrentVal = newVal}
					cond' <- linkCaseCond cond
					e' <- get
					put e'{caseEnvCurrentVal = val}
					return $ (declareVal env $ newVal{defBody = Dot (callRef val) (callRef d)}) : [cond']
			pars' <- mapM linkPar $ zip (maybe [] defPars constr) pars
			return $ Braces $ join pars'

		linkCaseCond (D.CaseUnapply _ ref pars) = do
			caseEnv <- get
			let 
				val = caseEnvCurrentVal caseEnv
				env' = caseEnvEnv caseEnv
				tp = dataType env' $ D.DataType ref []
				cl = dataTypeClass env' tp
				newValOpt = localVal  ("__caseOpt" ++ show (caseEnvValNum caseEnv) ++ "__") newTpOpt
				newTpOpt = maybe (TPUnknown "Not found unapply") defType unapply
				newVal = localVal  (caseEnvVal caseEnv) newTp
				newTp = case newTpOpt of
					TPOption _ t -> t
					TPUnknown _ -> tp
					t -> t

				allUnappies = filter ( ("unapply" == ). defName) $ filter ( (DefModStatic `elem`) .defMods) $ classDefs cl
				unapplyCall :: [Exp] -> Exp
				unapplyCall next = maybe (buildIf next) (buildCall next) unapply
				buildCall next f@Def{defType = ftp, defPars = [fpar]} = Braces $
					[declareVal env $ newValOpt{defBody = Dot (callRef (objectDef cl)) (Call f ftp [(fpar, maybeCast tp $ callRef val)] [])},
					If (callFromValOpt "isDefined") 
						(Braces $ (declareVal env $ newVal{defBody = callFromValOpt "get"}): next)
						notOk 
						]
				buildIf next = If (Dot (callRef val) (Is tp)) (Braces $
					(declareVal env $  newVal{defBody = Dot (callRef val) (CastDot tp)}) : next)
					notOk
				unapply :: Maybe Def
				unapply = find (parsTypeIs (defType val)) allUnappies
				parsTypeIs dtp def = case defPars def of
					[x] -> defType x == dtp
					_ -> False
				callFromValOpt fname = Dot (callRef newValOpt) (exprCall env' (Just newTpOpt) (D.Call fname Nothing []))
				
				caseEnv' = caseEnv {caseEnvCurrentVal = newVal, caseEnvValNum = caseEnvValNum caseEnv + 1}
			put caseEnv'
			tupleCond <- linkCaseCond (D.CaseUnapply Nothing "" pars)
			modify (\e -> e{caseEnvCurrentVal = val})
			return $ unapplyCall [tupleCond]

		linkCaseCond D.CaseAny = return Nop
		linkCaseCond  (D.CaseVal name) = do
			caseEnv <- get
			let 
				val = caseEnvCurrentVal caseEnv
				ret = localVal name $ defType val
				caseEnv' = caseEnvAddDef ret caseEnv
			put caseEnv'
			return $ Set Nothing (callRef ret) (callRef val)
		linkCaseCond  (D.CaseType cond tp) = do
			caseEnv <- get
			let 
				oldVal = caseEnvCurrentVal caseEnv
				env' = caseEnvEnv caseEnv
				tp' = dataType env' tp 
				val = case cond of
					D.CaseVal name -> localVal name tp'
					_ -> localVal (caseEnvVal caseEnv) tp'
				caseEnv' = caseEnv {caseEnvCurrentVal = val, caseEnvValNum = caseEnvValNum caseEnv + 1}
			put caseEnv'
			cond' <- linkCaseCond cond
			modify (\e -> e{caseEnvCurrentVal = oldVal})
			let ok = case cond of
					D.CaseVal _ -> Set Nothing (callRef val) $ Dot (callRef oldVal) (CastDot tp')
					_ -> cond'
			return $ If (Dot (callRef oldVal) (Is tp')) ok notOk
		items' = map linkCaseItem items
		_result' = _result {defType = reduceDataTypes env $ map snd items'}
	in Braces $ [
		declareVal env _case, declareVal env _incomplete, declareVal env _result'] 
		++ map fst items' 
		++ [If (callRef _incomplete) (Throw $ StringConst "Case incomplete") Nop, 
			callRef _result']

