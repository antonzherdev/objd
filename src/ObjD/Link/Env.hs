module ObjD.Link.Env (
	Env(..), ClassIndex, ObjectIndex,
	envChangeDefTp, envAddVals, envAddClasses, idxFind, classFind, envAddSuffix, tmpVal, baseClassExtends, envExprCompile,
	self
	) where

import 			 ObjD.Link.Struct
import qualified Data.Map            as M
import qualified ObjD.Struct         as D
import           Data.Maybe
--import Debug.Trace
{------------------------------------------------------------------------------------------------------------------------------ 
 - Env 
 ------------------------------------------------------------------------------------------------------------------------------}

type ClassIndex = M.Map String Class
type ObjectIndex = [Class]
data Env = Env{envExprCompiler :: Env -> D.Exp -> Exp, envLang :: Lang, envSelf :: DataType, envSelfCast :: Bool, envIndex :: ClassIndex
	, envObjectIndex :: ObjectIndex, envVals :: [Def], envInit :: Bool, envTp :: DataType, envVarSuffix :: String}

self :: Env -> Exp
self env = (if envSelfCast env then Cast (envSelf env) else id) (Self $ envSelf env)

envChangeDefTp :: Env -> Def -> DataType -> Env
envChangeDefTp env@Env{envVals = vals} d tp = env{envVals = d{defType = tp} : filter (/= d) vals}
envAddVals :: [Def] -> Env -> Env 
envAddVals newVals env@Env{envVals = vals} = env{envVals = vals ++ newVals}
envAddClasses :: [Class] -> Env -> Env 
envAddClasses newClasses env@Env{envIndex = cidx} = env{
	envIndex = cidx `M.union` M.fromList (map (\cc -> (className cc, cc)) newClasses)}
idxFind :: M.Map String a -> String -> Maybe a
idxFind idxx k = M.lookup k idxx
classFind :: ClassIndex -> String -> Class
--classFind _ name | trace ("classFind " ++ name) False = undefined
classFind cidx name = fromMaybe (ClassError name name ("Class " ++ name ++ " not found") ) $ idxFind cidx name
envAddSuffix :: Env -> String -> Env
envAddSuffix env s = env{envVarSuffix = envVarSuffix env ++ s}

tmpVal :: Env -> String -> DataType -> Exp -> Def
tmpVal env sf tp e = Def ("__tmp" ++ envVarSuffix env ++ sf) [] tp e [DefModLocal] Nothing []

baseClassExtends :: Bool -> ClassIndex -> ExtendsClass
--baseClassExtends _ _ | trace "baseClassExtends" False = undefined
baseClassExtends p cidx = ExtendsClass (classFind cidx (if p then "PObject" else "Object"), []) []

envExprCompile :: Env -> D.Exp -> Exp
envExprCompile env e = (envExprCompiler env) env e


