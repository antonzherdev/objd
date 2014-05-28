module ObjD.Link.Link (
	link
)where

import 			 ObjD.Link.Struct
import 			 ObjD.Link.Env
import 			 ObjD.Link.DataType
import 			 ObjD.Link.Extends
import 			 ObjD.Link.Conversion
import 			 ObjD.Link.Inline
import 			 ObjD.Link.Option
import 			 ObjD.Link.Call
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
link lang src = files
	where
		files = map (linkFile lang files) src

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
			++ [DefModStub | D.ClassModStub `elem` D.classMods cl] ++ [DefModEnum | _isEnum]
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
			D.Class{} -> D.ClassModCase `elem` D.classMods cl && (not $ any ( ("hash" == ). defName) defs)
		needEquals = case cl of
			D.Class{} -> D.ClassModCase `elem` D.classMods cl || any ( ("isEqual" == ). defName) defs
			_ -> False
		reloadedEquals = filter ( ("isEqual" == ). defName) defs
		equalFields = 
			let dds = map (defName . fst) constrPars
			in filter (\d -> DefModField `elem` defMods d && defName d `elem` dds) fields
		equal :: Def
		equal = let 
			p = localVal "to" (baseDataType env)
			a = Self selfType
			b = callRef p
			o = localValE "o" selfType (Cast selfType b)
			equalPrelude = If (BoolOp ExactEq a b) (Return True $ BoolConst True) Nop
			defEqual = 
				[equalPrelude,
				If (BoolOp Or (BoolOp ExactEq b (None (baseDataType env) )) (Not $ Dot b $ Is selfType)) (Return True $ BoolConst False) Nop]
				++ if null equalFields then [Return True $ BoolConst True] else equalsFun
			
			equalsFun = [Val False o, Return True $ foldl foldEq Nop equalFields]
				where
					foldEq Nop d = eqd d
					foldEq pp d = BoolOp And pp (eqd d)
					eqd d = BoolOp Eq (Dot a $ callRef d) (Dot (callRef o) $ callRef d)
		
			reloadedEqualCall d@Def{defPars = [pp@Def{defType = tp}]} = 
					If (Dot b $ Is tp) 
						(Return True $ Dot a (Call d TPBool [(pp, Cast tp b)] []))
					 	Nop
			reloadedEqualCall d = ExpError $ "Incorrect equal def " ++ show d
			body = if null reloadedEquals then defEqual
				else 
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
					tp' = unblockGenerics $ unwrapGeneric $ getDataType env' tp b
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



{------------------------------------------------------------------------------------------------------------------------------ 
 - Expression 
 ------------------------------------------------------------------------------------------------------------------------------}

exprTo :: Env -> DataType -> D.Exp -> Exp
exprTo env tp e = implicitConvertsion env tp $ expr env{envTp = tp} e

exprToSome :: Env ->D.Exp -> Exp
exprToSome env e =  expr env{envTp = baseDataType env} e

expr :: Env -> D.Exp -> Exp
expr env (D.If cond t D.Nop) = let
	cond' = exprTo (envAddSuffix env "c") TPBool cond 
	env' = optChecking (envAddSuffix env "t") cond'
	in If cond' (expr (fst env') t) Nop
expr env (D.If cond t f)
	| envTp env == TPVoid = let
		cond' = exprTo (envAddSuffix env "c") TPBool cond 
		env' = optChecking env cond'
		in If cond' (expr (envAddSuffix (fst env') "t") t) (expr (envAddSuffix (snd env') "f") f)
	| otherwise = let 
		cond' = exprTo env TPBool cond 
		env' = optChecking env cond'
		t' = expr (envAddSuffix (fst env') "t") t
		f' = expr (envAddSuffix (snd env') "f") f
		dt = exprDataType t'
		df = exprDataType f'
		retTp = firstCommonSuperDataType env dt df
	in If cond' (maybeCast retTp t') (maybeCast retTp f')
expr env (D.While cond t) = let
	cond' = exprTo env TPBool cond 
	env' = fst $ optChecking env{envTp = TPVoid} cond'
	in While cond' (expr env' t)
expr env (D.Synchronized cond t) = Synchronized (exprToSome env cond) (expr env t)
expr env (D.Try e f) = Try (expr env e) (expr env f)
expr env (D.Do cond t) = Do (exprTo env TPBool cond) (expr env{envTp = TPVoid} t)
expr env (D.Weak e) = insertWeak (expr env e)
expr _ (D.Braces []) = Nop
expr env (D.Braces es) = Braces $ bracesRec env 0 es
	where
		bracesRec :: Env -> Int -> [D.Exp] -> [Exp]
		bracesRec _  _ [] = []
		bracesRec env' n [x] = [expr env'{envVarSuffix = envVarSuffix env ++ ('_' : show n)} x]
		bracesRec env' n (x@D.Val{}:xs) = let
			x'@(Val m d) = expr env'{envVarSuffix = envVarSuffix env ++ ('_' : show n), envTp = TPVoid} x 
			env'' = (envAddVals [d] env')
			xs' = (bracesRec env'' (n + 1) xs)
			in 
				if DefModMutable `elem` defMods d then 
					let 
						existsSetInLambda = any (isJust . setsInLambda) parLambdas
						parLambdas = forExp isLambda (Braces xs')
						isLambda ee@Lambda{} = [ee]
						isLambda _ = []
						setsInLambda (Lambda _ lambdaExpr _) = forExp isSet lambdaExpr
						isSet ee@(Set _ (Call dd _ _ _) _) = if d == dd then Just ee else Nothing
						isSet ee@(PlusPlus (Call dd _ _ _)) = if d == dd then Just ee else Nothing
						isSet ee@(MinusMinus (Call dd _ _ _)) = if d == dd then Just ee else Nothing
						isSet _ = Nothing
						d' = d{defMods = DefModChangedInLambda :defMods d}
						ch (Call dd t [] [])
							| dd == d = Just $ Call d' t [] []
						ch _ = Nothing
					in if existsSetInLambda then (Val m d'):map (mapExp ch) xs' else x':xs'
				else x':xs'
		bracesRec env' n (x:xs) = let 
				x' = expr env'{envVarSuffix = envVarSuffix env ++ ('_' : show n), envTp = TPVoid} x
				env'' = case x' of
					(Set _ (Call d _ [] _) r) -> case unwrapGeneric (defType d) of
						TPOption cl tpl -> case unwrapGeneric (exprDataType r) of
							TPOption cr _ -> if cr == cl then env' else envChangeDefTp env' d (TPOption cr tpl)
							_ -> env'
						_ -> env'
					_ -> env'
				xs' = (bracesRec env'' (n + 1) xs)
			in x':xs'
expr _ D.Nop = Nop
expr _ (D.IntConst i) = IntConst i
expr _ (D.StringConst i) = StringConst i
expr _ D.Nil = Nil
expr _ (D.BoolConst i) = BoolConst i
expr _ (D.FloatConst s) = FloatConst s
expr env (D.BoolOp tp a b)
	| tp == Or || tp == And = let
		a' = exprTo (envAddSuffix env "a") TPBool a
		env' = optChecking env a'
		env'' = envAddSuffix (if tp == Or then snd env' else fst env') "b"
	in BoolOp tp a' (exprTo env'' TPBool b)
expr env (D.BoolOp tp a b) 
	| tp == Eq || tp == NotEq = let
		a' = exprToSome (envAddSuffix env "a") a
		b' = exprToSome (envAddSuffix env "b") b
		atp = unwrapGeneric $ exprDataType a'
		btp = unwrapGeneric $ exprDataType b'
	in case (a', b') of
		(Nil, _) -> compareWithNil env tp (b', btp)
		(_, Nil) -> compareWithNil env tp (a', atp)
		_ -> case (atp, btp) of
			(TPOption False _, TPOption False _) -> compareOptions env tp (a', atp) (b', btp)
			(TPOption False _, _) -> compareOptionWithNonOption env tp (a', atp) (b', btp)
			(_, TPOption False _) -> compareOptionWithNonOption env tp (b', btp) (a', atp)
			_ -> BoolOp tp  a' b'
expr env (D.BoolOp tp a b) = BoolOp tp (exprToSome (envAddSuffix env "a") a) (exprToSome (envAddSuffix env "b") b)
expr env (D.MathOp tp a b) = 
	let 
		aa = exprToSome (envAddSuffix env "a") a
		ltp = exprDataType aa
		math = MathOp tp aa (exprToSome (envAddSuffix env "b") b)
		cll = D.Call (literalDefName $ show tp) (Just [(Nothing, b)]) []
		callOp =  Dot aa $ exprCall (envAddSuffix env "b") (Just ltp) $ cll
	in case unwrapGeneric ltp of
		TPNumber{} -> math
		TPFloatNumber{} -> math
		TPString{} -> math
		TPPointer{} -> math
		_ -> callOp 
expr env d@(D.Dot a b) = let
	aa = case a of
		D.Call {} -> exprCall (envAddSuffix env "l") Nothing a
		_ -> exprToSome (envAddSuffix env "l") a
	aTp' = case exprDataType aa of
		TPOption True t -> t
		TPPointer t -> t
		TPGenericWrap _ (TPOption True t) -> t
		t -> t
	aTp = case exprDataType aa of
		TPOption True t -> t
		TPGenericWrap _ (TPOption True t) -> t
		t -> t
	bb = exprCall (envAddSuffix env "r") (Just aTp')  b
	def = case aa of
			ExpDError s _ -> ExpDError s d
			_ -> case bb of
				Dot l r -> maybeInlineCall env $ Dot (Dot aa l) r
				_ -> maybeInlineCall env $ Dot aa bb
	in case unwrapGeneric aTp of
		TPOption{} -> linkOptionCall env (a, aa) b
		TPPointer{} -> fromMaybe (Arrow aa bb) $ linkPointerCall env (a, aa) b
		TPObject _ Class{className = "Pointer"} -> fromMaybe def $ linkPointerStatic env b
		_ -> def
expr env d@(D.NullDot _ _) = linkNullDot env d
expr env (D.Set tp a b) = 
	let 
		aa = exprToSome env a
		ltp = case aa of
			NullDot _ r _ -> exprDataType r
			_ -> exprDataType aa
		simpleSet = if isNothing tp then set Nothing aa math
			else case unwrapGeneric ltp of
				TPNumber{} -> set tp aa math
				TPFloatNumber{} -> set tp aa math
				TPString{} -> set tp aa math
				TPPointer{} -> set tp aa math
				_ -> set Nothing aa callOp

		set tp' (NullDot dl dr _) r = 
			let 
				dltp = exprDataType dl
				tmp = tmpVal env "" dltp dl
				ref = if isElementaryExpression dl then dl else callRef tmp
				f = If (BoolOp NotEq ref (None dltp) ) (Set tp' (Dot (nonOpt env False ref) dr) r) Nop
			in  if isElementaryExpression dl then f else Braces [declareVal env tmp, f]
		set tp' l r = if isSimpleExpression r then Set tp' l r else multilineSet env tp' l r

		rtp = case unwrapGeneric ltp of
			 t@TPPointer{} -> if isJust tp then int else t
			 t -> t
		math = exprTo env rtp b 
		callOp = Dot aa $ exprCall env (Just ltp) $ D.Call (literalDefName $ show $ fromJust tp) (Just [(Nothing, b)]) []
		lcall = case aa of
				Dot _ c@(Call {}) -> Just c
				Arrow _ c@(Call {}) -> Just c
				NullDot _ c@(Call {}) _ -> Just c
				c@Call {} -> Just c
				_ -> Nothing
		isSelfSet = case aa of 
			Dot Self{} _ -> True
			_ -> False
		bToProcCall = if isJust tp then D.MathOp (fromJust tp) a b else b
		callSet ldef ref = exprCall env (Just $ unoptionIfChecked $ exprDataType ref) $ D.Call "set" (Just [(Just $ defName ldef, bToProcCall)]) []
	in if isNothing lcall then Set tp (ExpLError "Left is not def" aa) $ expr env b
		else case fromJust lcall of
			Call ldef _ [] _ -> 
				if DefModMutable `elem` defMods ldef then simpleSet 
				else if envInit env && isSelfSet then simpleSet
				else case aa of
					Dot ref _ ->  Dot ref $ callSet ldef ref
					NullDot ref _ _ ->  nullDot env ref $ callSet ldef ref
					_ -> exprCall env Nothing $ D.Call "set" (Just [(Just $ defName ldef, bToProcCall)]) []
			_ -> Set tp (ExpLError "Unassinable left" aa) (expr env b)
expr env (D.PlusPlus e) = PlusPlus (exprToSome env e)
expr env (D.MinusMinus e) = MinusMinus (exprToSome env e)
expr env D.Self = self env
expr env D.Super = Super $ fromMaybe (error $ "No super data type for " ++ show (envSelf env)) $ superType $ envSelf env
expr env r@D.Call{} = maybeInlineCall env $ exprCall env Nothing r
expr env (D.Index e i) = let
	e' = exprToSome env e
	obf = expr env $ D.Dot e (D.Call "apply" (Just [(Nothing, i)]) [])
	in case exprDataType e' of
		TPClass{} -> obf
		(TPGenericWrap _ TPClass{}) -> obf
		_ -> Index e' $ expr env i 
expr env l@(D.Lambda pars e) = 
	if all (isJust.snd) pars then 
		let 
			pars' = map (second (dataType env . fromJust)) pars
			env' =  envAddVals (map (uncurry localVal) pars') env
			mapEnvTp = case envTp env' of
				TPFun _ d -> d
				d -> d
			e' = expr env'{envTp = mapEnvTp} e
			tp = exprDataType e'
		in Lambda pars' (maybeAddReturn env tp e') tp
	else ExpDError "Not all types are defined in lambda" l

expr env (D.Val name tp body mods) = let
	tp' = fmap (dataType env) tp
	body' = expr env{envTp = fromMaybe (baseDataType env) tp'} body
	tp'' = unwrapGeneric $ fromMaybe (exprDataType body') tp'
	body'' = if isJust tp then implicitConvertsion env tp'' body' else body'
	mods' = DefModLocal : [DefModMutable | D.DefModMutable `elem` mods] ++ [DefModWeak | D.DefModWeak `elem` mods] ++ [DefModVolatile | D.DefModVolatile `elem` mods] 
	def' = Def{defName = name, defType = tp'', defMods = mods', defPars = [], 
		defBody = if isTpOption tp'' then body'' else case body'' of
			Nop -> ExpError $ name ++ ": no initialiazation value for non-option"
			_ -> body'', 
		defGenerics = Nothing, defAnnotations = []}
	in declareVal env def'
expr _ (D.Arr []) = Arr []
expr env (D.Arr [e]) = Arr [expr env e]
expr env (D.Arr exprs) = 
	let 
		exprs' = map (expr env) exprs
		tps = map exprDataType exprs'
		commonTp = reduceDataTypes env tps 
	in Arr $ map (\(e, tp) -> if tp == commonTp then e else maybeCast commonTp e) (zip exprs' tps)
expr env (D.Tuple items) = Tuple $ map (expr env) items
expr env (D.Throw e) = Throw (expr env e)
expr env (D.Return e) = Return  True (expr env e)
expr env (D.Not e) = Not (expr env e)
expr env (D.Negative e) = Negative (expr env e)
expr _ D.Break = Break
expr env c@D.Case{} = linkCase env c
expr env s@D.StringBuild {} = linkStringBuild env s
expr env ex@D.FuncOp{} = linkFuncOp env ex
-- expr x = error $ "No expr for " ++ show x

{------------------------------------------------------------------------------------------------------------------------------ 
 - Pointer
 ------------------------------------------------------------------------------------------------------------------------------}

linkPointerCall :: Env -> (D.Exp, Exp) -> D.Exp -> Maybe Exp
linkPointerCall env (_, leftExp) (D.Call "cast" Nothing [tp]) = Just $ Cast (case dataType env tp of
	p@TPPointer{} -> p
	p -> TPUnknown $ "Cast Pointer to non-pointer: " ++ show p) leftExp
linkPointerCall _ (_, leftExp) (D.Call "get" Nothing _) = Just $ Deferencing leftExp
linkPointerCall env (_, leftExp) (D.Call "get" (Just [(_, rel)]) _) = Just $ Deferencing $ MathOp Plus leftExp (expr env rel)
linkPointerCall env (_, leftExp) (D.Call "set" (Just [(_, value)]) _) = Just $ Set Nothing (Deferencing $ leftExp) (expr env value)
linkPointerCall env (_, leftExp) (D.Call "set" (Just [(_, rel), (_, value)]) _) = 
	Just $ Set Nothing (Deferencing $ MathOp Plus leftExp (expr env rel)) (expr env value)
linkPointerCall env (_, leftExp) c@(D.Call "free" Nothing _) = Just $ Dot leftExp $ exprCall env (Just $ exprDataType leftExp) c
linkPointerCall env (_, leftExp) c@(D.Call "copy" (Just [_]) _) = Just $ Dot leftExp $ exprCall env (Just $ exprDataType leftExp) c
linkPointerCall _ _ _ = Nothing

linkPointerStatic :: Env -> D.Exp -> Maybe Exp
linkPointerStatic env (D.Call "null" Nothing [tp]) = Just $ Null $ dataType env tp
linkPointerStatic _ _ = Nothing

{------------------------------------------------------------------------------------------------------------------------------ 
 - Functional Compositions >> *|* **
 ------------------------------------------------------------------------------------------------------------------------------}
linkFuncOp :: Env -> D.Exp -> Exp
linkFuncOp env ex@(D.FuncOp tp l r)  = 
	let 
		l' = expr env l
		r' = expr env r
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
						rr = expr (envAddVals [localVal "_" ritp] env) r
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
{------------------------------------------------------------------------------------------------------------------------------ 
 - String build
 ------------------------------------------------------------------------------------------------------------------------------}
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
			If (expr env e) 
				(StringConst $ '\n':(reverse . takeWhile ( /= '\n') . reverse) prev) 
				(StringConst "")
		compile _ e _ = expr env e
		accumr :: (String, [(Exp, String)])
		accumr = mapAccumR processPart lastString pars
		accuml :: (String, [(String, Exp)])
		accuml = mapAccumL (\prev (e, next) -> (next, (prev, e)) ) (fst accumr) (snd accumr)
	in StringBuild (snd accuml) (fst accuml)



{------------------------------------------------------------------------------------------------------------------------------ 
 - Pattern matching
 ------------------------------------------------------------------------------------------------------------------------------}

data CaseEnv = CaseEvn{caseEnvEnv :: Env, caseEnvCurrentVal :: Def, caseEnvValNum :: Int, caseEnvDefs :: [Def]}
 
caseEnvIncVal :: CaseEnv -> CaseEnv
caseEnvIncVal env = env {caseEnvValNum = caseEnvValNum env + 1}
caseEnvAddDef :: Def -> CaseEnv -> CaseEnv
caseEnvAddDef d env = env {caseEnvDefs = caseEnvDefs env ++ [d]}

linkCase :: Env -> D.Exp -> Exp
linkCase env (D.Case mainExpr items) = 
	let 
		mainExpr' = expr env mainExpr
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
				itemExpr = expr env' e
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

