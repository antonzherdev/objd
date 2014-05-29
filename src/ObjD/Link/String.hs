module ObjD.Link.String (
	linkStringBuild
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import           Data.List
import qualified ObjD.Struct         as D

linkStringBuild :: Env -> D.Exp -> Exp
linkStringBuild _ (D.StringBuild [] lastString) =  StringConst lastString
linkStringBuild env (D.StringBuild pars lastString) = 
	let 
		processPart :: String -> (String, D.Exp) -> (String, (Exp, String))
		processPart next (prev, e) = (modPrev e prev, (compile prev e next, modNext e next))
		modPrev :: D.Exp -> String -> String
		modPrev (D.Call "when" (Just [_]) _) "" = ""
		modPrev (D.Call "when" (Just [_]) _) s = (edrp . dropWhile ( /= '\n') . reverse) s
			where
				edrp "" = ""
				edrp ssss = reverse $ tail ssss
		modPrev _ s = s
		modNext :: D.Exp -> String -> String
		modNext _ s = s
		compile :: String -> D.Exp -> String -> Exp
		compile prev (D.Call "when" (Just [(_, e)]) _) _ = 
			If (envExprCompile env e) 
				(StringConst $ '\n':(reverse . takeWhile ( /= '\n') . reverse) prev) 
				(StringConst "")
		compile _ e _ = envExprCompile env e
		accumr :: (String, [(Exp, String)])
		accumr = mapAccumR processPart lastString pars
		accuml :: (String, [(String, Exp)])
		accuml = mapAccumL (\prev (e, next) -> (next, (prev, e)) ) (fst accumr) (snd accumr)
	in StringBuild (snd accuml) (fst accuml)
