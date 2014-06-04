module ObjD.Link.InlineClass (
	inlineClassAdditionalDefs
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Inline
import 			 ObjD.Link.Env
import           Data.List
import 			 Control.Arrow
--import Debug.Trace


inlineClassAdditionalDefs :: Env -> Extends -> [Def]
-- correctInlineDefs cl | trace ("correctInlineDefs " ++ className cl) False = undefined
inlineClassAdditionalDefs env exts 
	| isExtendsInlineClass exts = case exts of
		Extends Nothing traits -> concatMap (inlineChildDefs env []) traits
		Extends (Just (ExtendsClass cl pars)) traits -> inlineChildDefs env pars cl ++ concatMap (inlineChildDefs env []) traits
	| otherwise = []


inlineChildDefs :: Env -> [CallPar] -> ExtendsRef -> [Def]
inlineChildDefs env constrPars er@(cl, _) = map (inlineChildDef env constrPars er) $ filter (isDefChild) $ classDefsWithTraits cl

isExtendsInlineClass :: Extends -> Bool
isExtendsInlineClass = any isInlineRef . extendsRefs
		where
			isInlineRef (r, _) = 
				let m = classMods r
				in ClassModInline `elem` m || ClassModExtendsInline `elem` m 

inlineChildDef :: Env -> [CallPar] -> ExtendsRef -> Def -> Def
inlineChildDef env constrPars (cl, gens) d = let
	g = buildGenerics cl gens
	repgen = unwrapGeneric . replaceGenerics False g . unblockGenerics
	pars = defPars d
	pars' = map (\p -> p{defType = repgen (defType p)}) pars
	tp' = repgen (defType d)
	mods = defMods d
	constrPar = find ((== d) . fst) constrPars
	repConstrPars (Call dd _ [] []) = fmap snd $ find ((== dd) . fst) constrPars
	repConstrPars _ = Nothing

	in d{defMods = DefModConstructorField `delete` (DefModChild `delete` mods), defType = tp', defPars = pars',
			defBody = 
				if DefModField `elem` mods then
					case constrPar of
						Just p -> snd p 
						Nothing -> mapExp repConstrPars $ defBody d
				else inlineCall env $ Dot (Super $ TPClass TPMClass gens cl) $ Call d tp' (map (second callRef) $ zip pars pars') []
		}