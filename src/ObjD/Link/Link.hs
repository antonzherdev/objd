module ObjD.Link.Link (
	link
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Extends
import 			 ObjD.Link.Conversion
import 			 ObjD.Link.Inline
import 			 ObjD.Link.Call
import 			 ObjD.Link.Expr
import 			 Control.Arrow
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import           Data.List
import           Ex.String
import qualified ObjD.Struct         as D
--import Debug.Trace

idx :: (a -> k) -> a -> (k, a)
idx f a = (f a, a)

{-----------------------------------------------------------------------------------------------------------------------------------------
 - Link 
 -----------------------------------------------------------------------------------------------------------------------------------------}

link :: Lang -> D.Sources -> Sources
link lang src = Sources files includes
	where
		allFiles = files ++ includes
		files = map (linkFile lang allFiles) (D.sourcesFiles src)
		includes = map (linkFile lang allFiles) (D.sourcesIncludes src)

linkFile :: Lang -> [File] -> D.File -> File
linkFile lang files (D.File name package stms) = fl
	where
		fl :: File
		fl = File {fileName = name, fileImports = thisFileImports,
			fileClasses = classes, filePackage = package'}
		stms' = filter (not . containsOtherLangAnnotation) stms
		containsOtherLangAnnotation stm = any isOtherLangAnnotation $ D.stmAnnotations stm
		otherLangs = ["ObjC" | lang /= ObjC] ++ ["Java" | lang /= Java]
		isOtherLangAnnotation (D.Annotation nm [] []) = nm `elem` otherLangs
		isOtherLangAnnotation _ = False

		classes = (concatMap linkCl . filter isCls) stms'
		linkCl cl = linkClass (lang, cidx cl, glidx cl, fl, package', clImports cl) cl
		isCls s = D.isClass s || D.isStub s || D.isEnum s || D.isType s
		cidx cl =  M.fromList . map (idx className) $ classes ++ importClasses cl ++ (concatMap fileClasses $ kernelFiles ++ packageFiles)
		glidx cl = importObjectDefs cl
		package' = case package of
			[] -> error $ "Empty package for file " ++ name
			_ -> Package package packObj packPref

		packPref :: String
		packPref = fromMaybe "" $ do 
			o <- packObj
			p <- findValWithName "prefix" o
			(extractStringConst . defBody) p
		packObj :: Maybe Class
		packObj = find (\cl -> ClassModPackageObject `elem` classMods cl) 
			. concatMap fileClasses 
			. filter ((== package) . packageName . filePackage) $ files

		importClasses cl = mapMaybe impcl (imports cl)
			where
				impcl (ImportClass c)= Just c
				impcl _ = Nothing
		importObjectDefs cl = mapMaybe impcl (imports cl)
			where
				impcl (ImportObjectDefs c)= Just c
				impcl _ = Nothing
		imports :: D.FileStm -> [Import]
		imports cl = nub (clImports cl ++ thisFileImports ++ packObjImports)

		packObjImports = maybe [] (\o -> ImportObjectDefs o : classImports o) packObj

		thisFileImports :: [Import]
		thisFileImports = concatMap processImport . filter D.isImport $ stms'
			where
				processImport (D.Import imp _) = linkImport allFiles imp

		packageFiles = filter (\f -> f /= fl && package == (packageName . filePackage) f) files
		
		allFiles = filter (/= fl) $ files
		
		kernelFiles :: [File]
		kernelFiles = filter ((\n -> n == ["objd", "lang"] || n == ["objd", "collection"]) . packageName . filePackage ) files 
		
		clImports :: D.FileStm -> [Import]
		clImports cl = concatMap (\(D.ClassImport inn) -> linkImport files inn) . filter D.isClassImport $ classBody cl
			where
				classBody D.Class{} = D.classBody cl
				classBody D.Enum{} = D.classBody cl
				classBody _ = []
	

linkImport :: [File] -> [String] -> [Import]
linkImport files name
	| last name == "_" = let s = init name
		in (map ImportClass . filter (startsWith s . classPackageName)) allClasses 
				++ (map ImportObjectDefs . packObject) s
	| otherwise = map ImportClass $ classesWithName name
	where
		allClasses = concatMap fileClasses files
		packObject imp = filter (\c -> classFullName c == imp) allClasses
		classesWithName imp = filter (\c -> className c == last imp && classPackageName c == init imp) allClasses

linkClass :: (Lang, ClassIndex, ObjectIndex, File, Package, [Import]) -> D.FileStm -> [Class]
-- linkClass (_, _, _, _, _) D.Class{D.className = cls} | trace ("Class " ++ cls) False = undefined
linkClass (lang, ocidx, glidx, file, package, clImports) cl = if isSeltTrait && not isSeltStub then [cl', traitImplClass env cl'] else [cl']
	where
		cidx = ocidx `M.union` M.fromList (map (\g -> (className g, g)) generics)
		env = Env expr lang selfType False cidx glidx [] False TPVoid ""
		staticEnv = Env expr lang (TPObject (refDataTypeMod cl') cl') False ocidx glidx [] False TPVoid ""
		isObject = case cl of
			D.Class{} -> D.ClassModObject `elem` D.classMods cl
			_ -> False
		_isEnum = case cl of
			D.Enum{} -> True
			_ -> False
		clsName = case cl of
			D.Class{} -> if D.ClassModPackageObject `elem` D.classMods cl then "PackageObject" ++ (cap $ last $ packageName package) else D.className cl
			_ -> D.className cl
		clsGenName = fromMaybe clsName $ genName annotations
		cl' = case cl of
			D.Class{} -> Class {
				_classFile = file,
				_classPackage = package,
				_classMods =  nub $ concatMap clsMod (D.classMods cl), 
				className = clsName, 
				genClassName = clsGenName,
				_classExtends = if D.className cl == "Object" || D.className cl == "PObject" then extendsNothing 
					else fromMaybe (Extends (Just $ baseClassExtends selfIsStruct cidx) []) extends, 
				_classDefs = 
					if isObject then fields ++ defs ++ [typeField] 
					else fields ++ defs ++ constr constrPars ++ [typeField] ++ [description | needDescription]  ++ [equal | needEquals] ++ [hash | needHash]
				{-++ [unapply | D.ClassModTrait `notElem` D.classMods cl && not hasUnapply]-}, 
				_classGenerics = generics,
				_classImports = clImports,
				classAnnotations = annotations,
				classDefsWithTraits = defsWithTraits env cl'
			}
			D.Enum{} -> Class {
				_classFile = file,
				_classPackage = package,
				_classMods = [ClassModEnum], 
				className = D.className cl, 
				genClassName = clsGenName,
				_classExtends = Extends (Just $ ExtendsClass 
					(classFind cidx "Enum", [TPClass TPMEnum [] cl'])  
					[(enumOrdinal, callLocalVal "ordinal" uint), (enumName, callLocalVal "name" TPString)]) [], 
				_classDefs =  enumConstr ++
					snd (mapAccumL enumItem 0 (D.enumItems cl)) ++ fields ++ defs ++ [Def{
					defName = "values", defType = TPArr 0 (TPClass TPMEnum [] cl'), defBody = Nop,
					defMods = [DefModStatic, DefModSpecial], defPars = [], defGenerics = Nothing, defAnnotations = []}],
				_classGenerics = generics,
				_classImports = clImports,
				classAnnotations = annotations,
				classDefsWithTraits = defsWithTraits env cl'
			}
			D.Type{} -> Class {
				_classFile = file,
				_classPackage = package,
				_classMods = [ClassModType, ClassModStub], 
				className = D.className cl, 
				genClassName = clsGenName,
				_classExtends = Extends (Just $ ExtendsClass (linkExtendsRef env (D.typeDef cl)) []) [], 
				_classDefs = [constructorForType], 
				_classGenerics = generics,
				_classImports = [],
				classAnnotations = annotations,
				classDefsWithTraits = defsWithTraits env cl'
			}
		isSeltTrait = case cl of 
			D.Class{} -> D.ClassModTrait `elem` D.classMods cl
			_ -> False
		isSeltStub = case cl of 
			D.Class{} -> D.ClassModStub `elem` D.classMods cl
			D.Type{} -> True
			_ -> False
		annotations = map (linkAnnotation env) $ D.stmAnnotations cl
		enumOrdinal = Def "ordinal" [] uint Nop [] Nothing []
		enumName = Def "name" [] TPString Nop [] Nothing []
		enumAdditionalDefs = [(enumOrdinal, Nothing), (enumName, Nothing)]
		selfType = refDataType cl' (map (TPClass TPMGeneric []) generics)
		clsMod D.ClassModStruct = [ClassModStruct]
		clsMod D.ClassModStub = [ClassModStub]
		clsMod D.ClassModTrait =  [ClassModTrait]
		clsMod D.ClassModObject = [ClassModObject]
		clsMod D.ClassModAbstract = [ClassModAbstract]
		clsMod D.ClassModFinal = [ClassModFinal]
		clsMod D.ClassModPackageObject = [ClassModPackageObject]
		clsMod D.ClassModCase = [ClassModFinal, ClassModCase]
		-- clsMod _ = []
		extends = fmap (linkExtends env (isSeltTrait, selfIsStruct) (map fst constrPars)) (D.classExtends cl) 
		selfIsStruct = case cl of
			D.Class{} -> D.ClassModStruct `elem` D.classMods cl
			_ -> False
		isStaticDecl d = isObject || D.isStatic d
		additionalMods = [DefModStruct | selfIsStruct] ++ [DefModStatic | isObject] 
			++ [DefModStub | D.ClassModStub `elem` D.classMods cl] ++ [DefModEnum | _isEnum || D.className cl == "Enum"]
		fields :: [Def]
		fields =  concatMap (linkField staticEnv additionalMods) (filter (isStaticDecl) decls)  ++
			concatMap (linkField (envAddVals (map fst constrPars) env) additionalMods) (filter (not . isStaticDecl) decls)
		decls = (map (\d -> d{D.defBody = D.Nop}) . filter (\f -> D.isDecl f)) (D.classFields cl) 
			++ filter D.isDecl (D.classBody cl)
		defs = concatMap (\ def -> 
			linkDef (envForDef def) additionalMods def) 
			. filter D.isDef $ D.classBody cl
		envForDef def = if isStaticDecl def then staticEnv else env
		enumConstr = constr (enumAdditionalDefs ++ constrPars)
		constr :: [(Def, Maybe Exp)] -> [Def]
		constr pars = let
			mainDef = Def{defName = "apply", defMods = [DefModStatic, DefModConstructor, DefModPublic] ++ [DefModStruct | selfIsStruct], defBody = Nop,
				defPars = map fst pars, defType = selfType, defGenerics = Just $ DefGenerics generics selfType, defAnnotations = []}
			in resolveDefPar env mainDef pars 
		constrPars :: [(Def, Maybe Exp)]
		constrPars = map constrPar (D.classFields cl)
		constrPar :: D.ClassStm -> (Def, Maybe Exp)
		constrPar D.Def{D.defName = name, D.defRetType = Just tp, D.defBody = b} = let
			tp' = dataType env tp
			env' = envAddVals (map fst constrPars) env
			in (Def name [] (dataType env tp) Nop [DefModLocal, DefModWeak] Nothing [],
				case b of
					D.Nop -> Nothing
					_ -> Just $ exprTo env' tp' b)
		generics = linkGenerics env (D.classGenerics cl) 
		
		
		enumItem :: Int -> D.EnumItem -> (Int, Def)
		enumItem ordinal (D.EnumItem name pars) = (ordinal + 1, Def{defName = name, defMods = [DefModStatic, DefModEnumItem, DefModField], 
				defType = selfType, defGenerics = Nothing, defPars = [], 
				defBody = enumConstrCall, defAnnotations = []})
			where
				enumConstrCall = exprCall env Nothing enumConstrDCall
				enumConstrDCall = D.Call 
					(D.className cl) 
					(Just $ [ (Nothing,D.IntConst ordinal), (Nothing, D.StringConst name)] ++  pars)
					[]
		constructorForType = parConstructor' {defType = selfType}
			where
				gens = buildGenericsForSelf cl'
				parGenerics = superGenerics gens (extendsClassRef parClassExtends)
				parClassExtends = fromJust $ extendsClass $ classExtends $ cl'
				parConstructor = fromJust $ classConstructor $ extendsClassClass $ parClassExtends
				parConstructor' = replaceGenericsInDef parGenerics parConstructor
		typeField :: Def 
		typeField = Def{defMods = [DefModField, DefModStatic, DefModSpecial] ++ [DefModStruct | selfIsStruct], defName = "type", 
			defType = TPClass TPMClass [mapDataTypeGenerics (map (\_ -> TPAnyGeneric)) selfType] (classFind cidx typeName), 
			defBody = Nop, 
			defGenerics = Nothing, defPars = [], defAnnotations = []}
			where 
				typeName = if selfIsStruct then "PType" else "ClassType"

		needDescription = case cl of
			D.Class{} -> (not $ any ( ("description" == ). defName) defs)
		needHash = case cl of
			D.Class{} -> (D.ClassModCase `elem` D.classMods cl || D.ClassModStruct `elem` D.classMods cl)
				 && (not $ any ( ("hash" == ). defName) defs)
		needEquals = case cl of
			D.Class{} -> 
				D.ClassModCase `elem` D.classMods cl 
				|| (D.ClassModStruct `elem` D.classMods cl && not (any ( ("isEqual" == ). defName) defs))
				|| (D.ClassModStruct `notElem` D.classMods cl && any ( ("isEqual" == ). defName) defs)
			_ -> False
		reloadedEquals = filter ( ("isEqual" == ). defName) defs
		equalFields = 
			let dds = map (defName . fst) constrPars
			in filter (\d -> DefModField `elem` defMods d && defName d `elem` dds) fields
		equal :: Def
		equal = let 
			p = localVal "to" (if selfIsStruct then selfType else baseDataType env)
			a = Self selfType
			b = callRef p
			o = localValE "o" selfType (Cast selfType b)
			equalPrelude = If (BoolOp ExactEq a b) (Return True $ BoolConst True) Nop
			defEqual = 
				[equalPrelude,
				If (BoolOp Or (BoolOp ExactEq b (None (baseDataType env) )) (Not $ Dot b $ Is selfType)) (Return True $ BoolConst False) Nop]
				++ if null equalFields then [Return True $ BoolConst True] else [Val False o, equalsFun (callRef o)]
			
			equalsFun ref = Return True $ foldl foldEq Nop equalFields
				where
					foldEq Nop d = eqd d
					foldEq pp d = BoolOp And pp (eqd d)
					eqd d = BoolOp Eq (Dot a $ callRef d) (Dot ref $ callRef d)
		
			reloadedEqualCall d@Def{defPars = [pp@Def{defType = tp}]} = 
					If (Dot b $ Is tp) 
						(Return True $ Dot a (Call d TPBool [(pp, Cast tp b)] []))
					 	Nop
			reloadedEqualCall d = ExpError $ "Incorrect equal def " ++ show d
			body  
				| selfIsStruct = [equalsFun $ callRef p]
				| null reloadedEquals = defEqual
				| otherwise =
					[equalPrelude,
					If (BoolOp ExactEq b (None (baseDataType env) )) (Return True $ BoolConst False) Nop]
					++ map reloadedEqualCall reloadedEquals
					++ [Return True $ BoolConst False]
			in
				Def{defName = "isEqual", defMods = [DefModDef, DefModPublic] ++ [DefModStruct | selfIsStruct], defBody = Braces body,
					defPars = [p], defType = TPBool, defGenerics = Nothing, defAnnotations = []}
				
		
		hash :: Def
 		hash = let
 			r = localVarE "hash" uint (IntConst 0)
			hashFun [] = Return True $ IntConst 0
			hashFun fs = Braces (
				Val False r :
				map hashSet fs ++
				[Return True $ callRef r])
			hashSet d@Def{defType = tp} = Set Nothing (callRef r) $
				MathOp Plus (MathOp Mul (callRef r) (IntConst 31)) $ hashCall tp (Dot (Self selfType) (callRef d))
			hashCall :: DataType -> Exp -> Exp
			hashCall tp ref = 
				case tp of
					TPNumber{} -> ref
					TPBool -> ref
					TPChar -> ref
					TPEArr n atp -> arrHash atp n 0 Nop
					_ -> dotCall env ref "hash" [] []
				where	
					arrElemHash atp i = hashCall atp $ Index ref (IntConst i)
					arrHash _ 0 _ _ = IntConst 0
					arrHash atp n i op 
						| i >= n = op
						| i == 0 = arrHash atp n 1 (arrElemHash atp i)
						| otherwise = arrHash atp n (i + 1) $ MathOp Plus (MathOp Mul (IntConst 13) op) (arrElemHash atp i)

 			in Def{defName = "hash", defMods = [DefModDef, DefModPublic] ++ [DefModStruct | selfIsStruct], defBody = hashFun equalFields,
					defPars = [], defType = uint, defGenerics = Nothing, defAnnotations = []}

 		description = let
		 	descriptionFun = case equalFields of
		 		[] -> Return True $ StringConst $ D.className cl
		 		_ -> Return True $ foldl append (StringBuild [] ")") $ filter pos equalFields
			pos Def{defType = TPFun{}} = False
			pos _ = True
			append :: Exp -> Def -> Exp
			append (StringBuild [] e) d = StringBuild [(D.className cl ++ "(", Dot (Self selfType) (callRef d))] e
			append (StringBuild xs e) d = StringBuild (xs ++ [(", ", Dot (Self selfType) (callRef d))]) e
		
 			in Def{defName = "description", defMods = [DefModDef, DefModPublic] ++ [DefModStruct | selfIsStruct], defBody = descriptionFun,
					defPars = [], defType = TPString, defGenerics = Nothing, defAnnotations = []}
		



traitImplClass :: Env -> Class -> Class
traitImplClass env cl = let 
	con = Def{defName = "apply", defMods = [DefModStatic, DefModConstructor, DefModPublic], defBody = Nop,
				defPars = [], defType = envSelf env, defGenerics = Just $ DefGenerics (classGenerics cl) (envSelf env), defAnnotations = []}
	base = baseClassExtends False $ envIndex env 
	ext = Extends {
		extendsClass = Just $ case extendsRefs $ classExtends cl of
			[] -> base
			(xcl, xgens):_ -> maybe base (\c -> ExtendsClass (c, xgens) []) $ M.lookup (className xcl ++ "_impl") (envIndex env),
		extendsTraits = [(cl, map (TPClass TPMGeneric []) $ classGenerics cl)]
	}
	cl' = cl {
		className = className cl ++ "_impl",
		genClassName = genClassName cl ++ "_impl",
		_classExtends = ext, 
		_classMods = ClassModTraitImpl : ClassModAbstract : delete ClassModTrait (classMods cl), 
		_classDefs = con : (filter( (DefModOverride `elem`) .defMods) $ classDefs cl),
		_classImports = [],
		classDefsWithTraits = defsWithTraits env cl'
	}
	in cl'

getTraitImplClass :: Env -> Class -> Maybe Class
getTraitImplClass env cl = M.lookup (className cl ++ "_impl") (envIndex env)


defsWithTraits :: Env -> Class -> [Def]
defsWithTraits env cl = classDefs cl ++ notOverloadedTraitDefs
	where	
		
		notOverloadedTraitDefs = map updef $ filter (\(def, _) -> not $ any ((== def) . fst) notAbstractClassDefs) traitDefs
		--notOverloadedTraitDefs = notAbstractTraitDefs
		traitDefs = defs True
		notAbstractClassDefs = filter ((DefModAbstract `notElem`) . defMods .  fst) $ defs False
		defs :: Bool ->[(Def, Class)]
		defs trait = ( map (\(a, _, c) -> (a, c)) . filter (\(_, t, _) -> trait == t)) allDefsWithLine
		allDefsWithLine :: [(Def, Bool, Class)]
		allDefsWithLine = allDefsWithLine' False True cl
		allDefsWithLine' :: Bool -> Bool -> Class -> [(Def, Bool, Class)] -- (Def, traitLine - True/classLine - False)
		allDefsWithLine' currentLine traitLine cll = 
			map (\def -> (def, currentLine, cll)) (classDefs cll) 
			++ concatMap nextRec ((extendsRefs . classExtends) cll)
			where
				nextRec :: ExtendsRef -> [(Def, Bool, Class)]
				nextRec (nextClass, _) = 
					let line = traitLine && isTrait nextClass
					in allDefsWithLine' line line nextClass
		stp = TPClass TPMClass clGens cl
		clGens = map (TPClass TPMGeneric []) $ classGenerics cl
		updef :: (Def, Class) -> Def
		updef (d, defClass) = let
			gens = buildGenerics defClass $ fromJust $ upGenericsToClass defClass (cl, clGens)
			rg = replaceGenerics False gens
			tp' = rg $ defType d
			pars' :: [(Def, Def)]
			pars' = map (\p -> (p, p{defType = rg $ defType p})) (defPars d)
			body' :: Exp
			body' = inlineCall env{envSelf = stp} $ Dot (Self stp) $ Call d tp' (map (second callRef) pars') []
			in case classGenerics cl of
				[] -> d
				_ -> d{defType = tp', defPars = map snd pars', defBody = body'}

linkExtends :: Env -> (Bool, Bool) -> [Def] -> D.Extends -> Extends
linkExtends env (isSeltTrait, isSelfStruct) constrPars (D.Extends (D.ExtendsClass eref@(_, gens) pars) withs) = 
	let 
		env' = env {envVals = constrPars, envSelf = objectType $ envSelf env}
		superCall = D.Dot D.Super $ D.Call "apply" (Just $ pars) gens
		superCall' = expr env' superCall
		superCallPars = case superCall' of
			Dot _ (Call _ _ pars' _) -> pars'
			err -> [(unknownDef, err)]
		mainExt = linkExtendsRef env eref
		withs' = map (linkExtendsRef env) withs
		isMainTrait = isTrait (fst mainExt)
	in 
		if isSeltTrait then Extends Nothing (mainExt:withs')
		else 
			if isMainTrait then 
				case getTraitImplClass env (fst mainExt) of
					Just me -> Extends (Just $ ExtendsClass (me, snd mainExt) []) withs'
					Nothing -> Extends (Just $ baseClassExtends isSelfStruct $ envIndex env) (mainExt:withs')
			else Extends (Just $ ExtendsClass mainExt superCallPars) withs'

linkAnnotation :: Env -> D.Annotation -> Annotation
linkAnnotation env (D.Annotation nm pars tps) = case expr env (D.Call nm (Just pars) tps) of
	Call d _ pars' _ -> Annotation d pars'
	Dot _ (Call d _ pars' _ ) -> Annotation d pars'

linkExtendsRef :: Env -> D.ExtendsRef -> ExtendsRef
linkExtendsRef env (ecls, gens) = (classFind (envIndex env) ecls, map (wrapGeneric . dataType env) gens) 

linkGenerics :: Env -> [D.Generic] -> [Class]
linkGenerics env gens = cls
	where 
		linkGeneric (D.Generic name ext) = let
			genExtendsRefs = case ext of
				Nothing -> []
				Just (D.Extends (D.ExtendsClass firstExtends []) nextExtends) -> firstExtends : nextExtends
			in Generic{
				className = name, genClassName = name,
				_classExtendsRef = (classFind (envIndex env) "Object", []) : map (linkExtendsRef env') genExtendsRefs}
		cls = map linkGeneric gens
		env' = envAddClasses cls env
		

linkField :: Env -> [DefMod] -> D.ClassStm -> [Def]
--linkField _ _ D.Def{D.defName = nm} | trace ("Field " ++ nm) False = undefined
linkField env additionalMods dd@D.Def{D.defMods = mods, D.defName = name, D.defRetType = tp, D.defBody = e} = 
	let 
		i = exprToSome env e
		i' = implicitConvertsion env tp'' i
		tp' = unwrapGeneric $ getDataType env tp i
		tp'' = if DefModStruct `elem` additionalMods then case tp' of
				TPArr n atp -> TPEArr n $ unwrapGeneric atp 
				_ -> tp' 
			else tp'
		ans = map (linkAnnotation env) (D.defAnnotations dd)
		gtp = wrapGeneric tp''
		def = Def{defMods = 
			DefModField : translateMods mods ++ additionalMods ++ checkOverrideMods mods (findOverridenDef env dd), 
			defName = name, defType = tp'', 
			defBody = i', defGenerics = Nothing, defPars = [], defAnnotations = ans}
		isLazy = D.DefModLazy `elem` mods
		lazyClass = classFind (envIndex env) "Lazy"
		lazyGet = fromJust $ findDefWithName "get" lazyClass
		lazyConstr = fromJust $ classConstructor lazyClass
		lazyTp = TPClass TPMClass [gtp] lazyClass
		defLazy = Def{defMods = [DefModField, DefModPrivate] ++ [DefModStatic | D.DefModStatic `elem` mods ] ++ additionalMods, defName = "_lazy_" ++ name, 
			defType = lazyTp, 
			defBody = Dot (callRef (objectDef lazyClass)) $ Call lazyConstr lazyTp [(head $ defPars lazyConstr, Lambda [] (Return True (Weak i')) gtp)] [], 
			defGenerics = Nothing, defPars = [], defAnnotations = []}
		defLazyGet = Def{defMods = DefModDef : translateMods mods ++ additionalMods, defName = name, 
			defType = tp'', 
			defBody = Return True $ Dot (callRef defLazy) (Call lazyGet gtp [] []), defGenerics = Nothing, defPars = [], defAnnotations = ans}
		in if isLazy then [defLazyGet, defLazy] else [def]

findOverridenDef :: Env -> D.ClassStm -> Maybe Def
findOverridenDef env D.Def{D.defName = name, D.defPars = opars} = find eqDef $ allDefsInParentClass (buildGenerics cl $ dataTypeGenerics $ envSelf env) cl
	where
		cl = envSelfClass env
		eqDef d = defName d == name && length (defPars d) == length opars' && all eqParameterNames (zip (defPars d) opars')
		eqParameterNames (Def{defName = l}, D.Par{D.parName = r}) = l == r
		filterSelfPar (D.Par{D.parName = "self"}:xs) = xs
		filterSelfPar xs = xs
		opars' = filterSelfPar opars

checkOverrideMods :: [D.DefMod] -> Maybe Def -> [DefMod]
checkOverrideMods mods (Just o) =
	[DefModError "No override modifier" |  D.DefModOverride `notElem` mods] ++
	[DefModError "Override final def" | DefModFinal `elem` defMods o] 
checkOverrideMods mods Nothing = [DefModError "Override nothing" |  D.DefModOverride `elem` mods]		

translateMods :: [D.DefMod] -> [DefMod]
translateMods = fx . mapMaybe m
	where 
		fx ms = if DefModPrivate `notElem` ms && DefModProtected `notElem` ms then DefModPublic : ms else ms
		m D.DefModStatic = Just DefModStatic
		m D.DefModMutable = Just DefModMutable
		m D.DefModPrivate = Just DefModPrivate
		m D.DefModProtected = Just DefModProtected
		m D.DefModWeak = Just DefModWeak
		m D.DefModPure = Just DefModPure
		m D.DefModFinal = Just DefModFinal
		m D.DefModOverride = Just DefModOverride
		m D.DefModConstructorField = Just DefModConstructorField
		m D.DefModInline = Just DefModInline
		m D.DefModVolatile = Just DefModVolatile
		m _ = Nothing
		
linkDef :: Env -> [DefMod] -> D.ClassStm  -> [Def]
-- linkDef env D.Def{D.defName = name} _ | trace ("Def " ++ show (envSelf env) ++ "." ++ name) False = undefined
linkDef env additionalMods dd@D.Def{D.defMods = mods, D.defName = name, D.defPars = opars, D.defRetType = tp, D.defBody = body, D.defGenerics = generics} = 
	resolveDefPar env' mainDef pars''
	where 
		env' = addEnvInit $ envAddClasses generics' env
		generics' = linkGenerics env generics
		isInit = name == "init" && null opars
		addEnvInit e = if isInit then e {envInit = True} else e
		
		pars :: [(Def, Maybe Exp)]
		pars = linkDefPars env'' opars
		
		overrideDef = findOverridenDef env dd	
		pars'' :: [(Def, Maybe Exp)]
		pars'' = case overrideDef of
			Nothing -> filterSelfPar pars
			Just od -> map checkParTypeAndWrapIfNeeded $ zip (filterSelfPar pars) (defPars od)
			where
				filterSelfPar ((Def{defName = "self"}, _):xs) = xs
				filterSelfPar d = d

		ans = map (linkAnnotation env) (D.defAnnotations dd)
		checkParTypeAndWrapIfNeeded :: ((Def, Maybe Exp), Def) -> (Def, Maybe Exp)
		checkParTypeAndWrapIfNeeded (p@(thisPar@Def{defType = thisTp}, de), Def{defType = superTp})
			| isInstanceOfTp env thisTp superTp = case (thisTp, superTp) of
				(TPGenericWrap{}, TPGenericWrap{}) -> p
				(_, TPGenericWrap{}) -> (thisPar{defType = wrapGeneric thisTp}, de)
				_ -> p
			| otherwise = (thisPar{defType = TPUnknown $ "Incorrect override: " ++ show thisTp ++ " is not instance of " ++ show superTp }, de)
		defGenerics' = Just $ DefGenerics generics' $ envSelf env''' 
		mods' = translateMods mods ++ if isInit then [] else checkOverrideMods mods overrideDef 
		

		overrideTp = fmap defType overrideDef
		needWrapRetType = maybe False isTpG overrideTp
		isTpG (TPClass TPMGeneric _ _) = True
		isTpG (TPGenericWrap _ _) = True
		isTpG _ = False

		mapOverrideType rtp = 
			let rtp' = if needWrapRetType then wrapGeneric rtp else rtp
		 	in case overrideTp of
		 		Just otp -> 
		 			if isInstanceOfTp env' rtp' otp then (if isJust tp || (null opars && name == "apply") then rtp' else otp) else
		 				TPUnknown $ "Could not choose correct datatype for override " ++ show rtp' ++ " is not instance of " ++ show otp
		 		_ -> rtp'
		parDefs = map fst pars''
		env'' = envAddVals parDefs env'
		env''' = case pars of
		 	[] -> env''
		 	(Def{defName = dn, defType = dtp}, _) : _ -> if dn == "self" then env''{envSelf = dtp, envSelfCast = True} else env''

		isSelfStub = case envSelf env of
			TPClass _ _ cl -> isStub cl
			TPObject _ cl -> isStub cl
		mainDef = (case body of
			D.Nop -> Def {defMods = DefModDef : mods' ++ [DefModAbstract | not isSelfStub] ++ additionalMods, 
					defName = name, defGenerics = defGenerics',
					defPars = parDefs,
					defType = dataType env' (fromMaybe (D.DataType "void" []) tp), defBody = Nop, defAnnotations = ans} 
			_   -> 
				let 
					b = case tp of
						Just (D.DataType "void" []) -> expr env''' body
						_ -> exprToSome env''' body
					tp' = unblockGenerics $ getDataType env' tp b
					tp'' = mapOverrideType tp'
					superInitDef :: Maybe Def
					superInitDef = join $ fmap classInitDef $ superClass (envSelfClass env)
					callSuperInit :: Exp
					callSuperInit = Dot (Super $ fromJust $ superType $ envSelf env) $ call (fromJust superInitDef) []
					addSuperInit e
						| isInit && isJust superInitDef = case e of
							Braces es -> Braces $ callSuperInit : es
							_ -> Braces [callSuperInit, e]
						| otherwise = e
				in Def {defMods = DefModDef : mods' ++ additionalMods, defName = name, defGenerics = defGenerics',
					defPars = parDefs,
					defType = tp'', defBody = addSuperInit $ maybeAddReturn env tp'' b, defAnnotations = ans})

resolveDefPar :: Env -> Def -> [(Def, Maybe Exp)] -> [Def]
--resolveDefPar _ Def{defName = dn} _ | trace ("resolveDefPar: " ++ dn) False = undefined
resolveDefPar env mainDef parameters = parRec True parameters []
	where
		parRec :: Bool -> [(Def, Maybe Exp)] -> [(Def, Maybe Exp)]  -> [Def]
		--parRec _ ((Def{defName = name}):_) _ | trace ("parRec: " ++ name) False = undefined
		--parRec _ [] _ | trace ("parRec: []") False = undefined
		parRec True [] _ = [mainDef]
		parRec False [] recPars = [makeDef $ reverse recPars]
		parRec isMainDef ((par, defaultExp):xs) recPars = 
			parRec isMainDef xs ((par, Nothing):recPars) 
			++ case defaultExp of 
				Nothing -> []
				_ -> parRec False xs ((par, defaultExp):recPars)
		makeDef :: [(Def, Maybe Exp)] -> Def
		makeDef pars =  
			let 
				callGens = maybe [] (map (TPClass TPMGeneric []). defGenericsClasses) $ defGenerics mainDef
				callMainDef = Call mainDef (defType mainDef) (map expCallPar pars) callGens
				callMainDef' = maybeAddReturn env (defType mainDef) $ 
					if DefModConstructor `elem` defMods mainDef then callMainDef 
					else Dot (self env) $ callMainDef
				in mainDef {defMods = (map (\m -> if m == DefModConstructor then DefModDef else m) . filter (\m -> m /= DefModAbstract) ) (defMods mainDef),
					defPars = (map fst . filter ( isNothing . snd)) pars,
					defBody = callMainDef'}  
		expCallPar :: (Def, Maybe Exp) -> (Def, Exp)
		expCallPar (d, Nothing) = (d, callRef d)
		expCallPar (d, Just b) = (d, b)

linkDefPars :: Env -> [D.Par] -> [(Def, Maybe Exp)]
--linkDefPars _ _ | trace ("linkDefPars: ") False = undefined
linkDefPars env pars = let 
	pars' = map linkPar pars 
	--env' = envAddVals (map fst pars') env
	parMod D.ParModWeak = DefModWeak
	linkPar D.Par {D.parMods = mods, D.parName = pnm, D.parType  = ttt, D.parDefault = pd} = let 
		tp = fmap (dataType env) ttt
		tp' = fromMaybe (exprDataType defaultExp) tp
		defaultExp = if isJust tp then exprTo env (fromJust tp) pd else exprToSome env pd
		in (Def {
			defName = pnm, defPars = [], 
			defType = tp', 
			defBody = Nop, 
			defMods = DefModLocal : map parMod mods, defGenerics = Nothing, defAnnotations = []}, 
			case pd of
				D.Nop -> Nothing
				_ -> Just $ defaultExp)
	in pars'


