module ObjD.Link.Env (
	Env(..), ClassIndex, ObjectIndex,
	envChangeDefTp, envAddVals, envAddClasses, idxFind, classFind, envAddSuffix, tmpVal, baseClassExtends
	) where

import 			 ObjD.Link.Struct
import qualified Data.Map            as M
import           Data.Maybe
{------------------------------------------------------------------------------------------------------------------------------ 
 - Env 
 ------------------------------------------------------------------------------------------------------------------------------}

type ClassIndex = M.Map String Class
type ObjectIndex = [Class]
data Env = Env{envSelf :: DataType, envSelfCast :: Bool, envIndex :: ClassIndex, envObjectIndex :: ObjectIndex
	, envVals :: [Def], envInit :: Bool, envTp :: DataType, envVarSuffix :: String}

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
classFind cidx name = fromMaybe (ClassError name ("Class " ++ name ++ " not found") ) $ idxFind cidx name
envAddSuffix :: Env -> String -> Env
envAddSuffix env s = env{envVarSuffix = envVarSuffix env ++ s}

tmpVal :: Env -> String -> DataType -> Exp -> Def
tmpVal env sf tp e = Def ("__tmp" ++ envVarSuffix env ++ sf) [] tp e [DefModLocal] Nothing []

baseClassExtends :: ClassIndex -> ExtendsClass
baseClassExtends cidx = ExtendsClass (classFind cidx "Object", []) []
