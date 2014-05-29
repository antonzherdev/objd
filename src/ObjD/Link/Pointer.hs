module ObjD.Link.Pointer (
	linkPointerCall, linkPointerStatic
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Call
import           Ex.String
import qualified ObjD.Struct         as D


linkPointerCall :: Env -> (D.Exp, Exp) -> D.Exp -> Maybe Exp
linkPointerCall env (_, leftExp) (D.Call "cast" Nothing [tp]) = Just $ Cast (case dataType env tp of
	p@TPPointer{} -> p
	p -> TPUnknown $ "Cast Pointer to non-pointer: " ++ show p) leftExp
linkPointerCall _ (_, leftExp) (D.Call "get" Nothing _) = Just $ Deferencing leftExp
linkPointerCall env (_, leftExp) (D.Call "get" (Just [(_, rel)]) _) = Just $ Deferencing $ MathOp Plus leftExp (envExprCompile env rel)
linkPointerCall env (_, leftExp) (D.Call "set" (Just [(_, value)]) _) = Just $ Set Nothing (Deferencing $ leftExp) (envExprCompile env value)
linkPointerCall env (_, leftExp) (D.Call "set" (Just [(_, rel), (_, value)]) _) = 
	Just $ Set Nothing (Deferencing $ MathOp Plus leftExp (envExprCompile env rel)) (envExprCompile env value)
linkPointerCall env (_, leftExp) c@(D.Call "free" Nothing _) = Just $ Dot leftExp $ exprCall env (Just $ exprDataType leftExp) c
linkPointerCall env (_, leftExp) c@(D.Call "copy" (Just [_]) _) = Just $ Dot leftExp $ exprCall env (Just $ exprDataType leftExp) c
linkPointerCall _ _ _ = Nothing

linkPointerStatic :: Env -> D.Exp -> Maybe Exp
linkPointerStatic env (D.Call "null" Nothing [tp]) = Just $ Null $ dataType env tp
linkPointerStatic _ _ = Nothing
