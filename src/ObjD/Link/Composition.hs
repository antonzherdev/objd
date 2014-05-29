module ObjD.Link.Composition (
	linkFuncOp
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Conversion
import           Data.List
import qualified ObjD.Struct         as D

{------------------------------------------------------------------------------------------------------------------------------ 
 - Functional Compositions >> *|* **
 ------------------------------------------------------------------------------------------------------------------------------}
linkFuncOp :: Env -> D.Exp -> Exp
linkFuncOp env ex@(D.FuncOp tp l r)  = 
	let 
		l' = envExprCompile env l
		r' = envExprCompile env r
		ltp = exprDataType l'
		lInputType = case ltp of 
			TPFun ret _ -> Right $ head ret
			_ -> Left $ "Left is not function but " ++ show ltp ++ " in " ++ show l'
		lOutputType = case ltp of 
			TPFun _ ret -> Right ret
			_ -> Left $ "Left is not function but " ++ show ltp ++ " in " ++ show l'
		rInputTypeShouldBe = case tp of
			D.FuncOpBind -> lOutputType >>= \t -> case t of
				TPOption _ o -> return $ unwrapGeneric o
				_ -> return $ t
			D.FuncOpClone -> lInputType
		r'' = case exprDataType r' of
			TPFun{} -> r'
			_ -> case rInputTypeShouldBe of
				Left _ -> r'
				Right ritp -> 
					let 
						rr = envExprCompile (envAddVals [localVal "_" ritp] env) r
						etp = exprDataType rr
					in Lambda [("_", ritp)] (maybeAddReturn env etp rr) etp 
		ldef = localVal "__l" (exprDataType l')
		rdef = localVal "__r" (exprDataType r'')
		rtp = exprDataType r''
		rInputType = case rtp of 
			TPFun ret _ -> Right $ head ret
			_ -> Left $ "Right is not function but " ++ show rtp ++ " in " ++ show r''
		rOutputType = case rtp of 
			TPFun _ ret -> Right ret
			_ -> Left $ "Right is not function but " ++ show rtp ++ " in " ++ show r''
		f p = do
			lInputType
			return $ Dot (callRef ldef) $ call (applyLambdaDef ltp) [p]
		g p = do 
			rInputType
			return $ Dot (callRef rdef) $ call (applyLambdaDef rtp) [p]
		compile :: Either String Exp
		compile = do
			li <- lInputType 
			lo <- lOutputType 
			ri <- rInputType
			ro <- rOutputType
			let
				lambda o c = Lambda [("_", li)] (maybeAddReturn env o c) o
				bind :: Either String Exp
				bind = do
					ff <- f $ callRef $ localVal "_" li
					let 		
						dotCl = do
							c <- g ff
							return $ lambda ro c
						optClass = dataTypeClass env lo 
						mapDef = maybe (Left "map in option didn't find") Right $ find ( (== "map") . defName) $ classDefs optClass 
						forDef = maybe (Left "for in option didn't find") Right $ find ( (== "for") . defName) $ classDefs optClass 
						optCall = do
							m <- if ro == TPVoid then forDef else mapDef
							gg <- g $ callRef $ localVal "_" $ wrapGeneric ri
							let c = Dot ff $ call m [Lambda 
								[("_", wrapGeneric ri)] 
								(maybeAddReturn env (wrapGeneric ro) gg)
								(wrapGeneric ro)]
							return $ lambda (if ro == TPVoid then TPVoid else TPOption False $ wrapGeneric ro) c
					case (lo, ri) of
						(TPOption _ _, TPOption _ _) -> dotCl
						(TPOption _ _, _) -> optCall
						_ -> dotCl
				clone :: Either String Exp
				clone = do
					ff <- f $ callRef $ localVal "_" li
					gg <- g $ callRef $ localVal "_" li
					case (lo, ro) of
						(TPVoid, TPVoid) -> return $ lambda TPVoid $ Braces [ff, gg]
						(TPVoid, _) -> return $ lambda ro $ Braces [ff, maybeAddReturn env ro gg]
						(_, TPVoid) -> return $ lambda lo $ Braces [gg, maybeAddReturn env lo ff]
						_ -> return $ lambda (TPTuple [lo, ro]) $ Tuple [ff, gg]
					
			case tp of
				D.FuncOpBind -> bind
				D.FuncOpClone -> clone
	in case compile of
		Left err -> ExpDError err ex
		Right e -> 
			Braces [
				declareVal env ldef{defBody = implicitConvertsion env ltp l'},
				declareVal env rdef{defBody = implicitConvertsion env rtp r''},
				e
			]


