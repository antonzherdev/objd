module ObjD.Parser(
	parseFile, parseStatement, removeComments
) where

import 			 Ex.String
import           Control.Monad
import           Data.Maybe
import           ObjD.Struct
import           Data.Decimal
import 			 Control.Arrow
import           Text.ParserCombinators.Parsec


parseFile :: String -> String -> Either ParseError File
parseFile name text  = fmap (\(pack, stms) -> File name pack stms) (parse pFile "ObjD" $ removeComments $ text)



removeComments :: String -> String
removeComments [] = []
removeComments ('/' : '/' : xs) = waitLine xs
	where
		waitLine [] = []
		waitLine ('\n' : xxs) = '\n' : removeComments xxs
		waitLine (_  : xxs) = waitLine xxs
removeComments ('/' : '*' : xs) = waitMultiLine xs
	where
		waitMultiLine [] = []
		waitMultiLine ('*' : '/' : xxs) = removeComments xxs
		waitMultiLine (_ : xxs) = waitMultiLine xxs
removeComments (x : xs) = x : removeComments xs

pFile :: Parser ([String], [FileStm])
pFile = do
	sps
	package <- option [] $ do
		string "package"
		sps1
		ident `sepBy` char '.'
	sps
	res <- many pStatement
	eof
	return (package, res)

parseStatement :: String -> Either ParseError FileStm
parseStatement = parse pStatement "ObjD"

pStatement :: Parser FileStm
pStatement =  (pAnnotations >>= \a -> pImport a <|> pTypeStm a <|>  pClass a <|> pEnum a)

pAnnotations :: Parser [Annotation]
pAnnotations = many $ do
	char '@'
	(Call name pars tps) <- pCall
	sps
	return $ Annotation name (fromMaybe [] pars) tps

wsps :: Parser String
wsps = many (char ' ' <|> char '\t') <?> ""
sps :: Parser String
sps = many (char ' ' <|> char '\t'  <|> char '\n') <?> ""
sps1 :: Parser String
sps1 = many1 (char ' ' <|> char '\t'  <|> char '\n') <?> ""
ident :: Parser String
ident = many1 (letter <|> digit <|> oneOf "_")
spsChar :: Char -> Parser Char
spsChar c = sps >> char c
charSps :: Char -> Parser String
charSps c = char c >> sps
pDataType ::Bool -> Parser DataType
pDataType lambda = charSps ':' >> pType lambda
pType :: Bool -> Parser  DataType
pType = tp
	where
		tp lm = do 
			t <- arrr <|> tuple <|> simple
			al <- optionMaybe $ do
				charSps '['
				d <- option "0" $ many digit
				sps
				charSps ']'
				return d
			sps
			opt <- option False $ charSps '?' >> return True
			sps
			r <- if lm then return Nothing else optionMaybe $ do
				string "->"
				sps
				tp True
			sps
			let t' = maybe t (\s -> DataTypeArr (read s) t) al
			let t'' = if opt then DataTypeOption t' else t'
			return $ maybe t'' (\rr -> DataTypeFun t'' rr) r
		arrr = do
			charSps '['
			k <- tp False
			sps
			v <- optionMaybe $ charSps ':' >> tp False
			char ']'
			sps
			return $ maybe (DataTypeArr 0 k) (DataTypeMap k) v
		tuple = do
			charSps '('
			t <- tp False `sepBy` charSps ','
			sps
			char ')'
			sps
			return $ makeTuple t
		makeTuple :: [DataType] -> DataType
		makeTuple [] = DataType "void" []
		makeTuple t = DataTypeTuple t

		simple = do
			v <- ident
			sps
			gens <- generics
			return $ DataType v gens
		generics = option [] $ between (charSps '<') (charSps '>') $ (tp False) `sepBy` charSps ','
		
braces :: Parser a -> Parser a
braces p = do 
	r <- between (charSps '{') (spsChar '}') p 
	sps
	return r
brackets :: Parser a -> Parser a
brackets = between (charSps '(') (spsChar ')')

stringSps :: String -> Parser String
stringSps s = string s >> sps

pTypeStm :: [Annotation] -> Parser FileStm
pTypeStm a = do
	try(string "typedef")
	sps1
	name <- ident
	sps
	generics <- pGenerics
	sps
	charSps '='
	tp <- ident
	sps
	gens <- pGensRef
	sps
	return $ Type name generics (tp, gens) a


pClass :: [Annotation] -> Parser FileStm
pClass a = do
	mods <- many $ (try (stringSps "stub") >> return ClassModStub) <|> (try (stringSps "abstract") >> return ClassModAbstract) <|> (try (stringSps "final") >> return ClassModFinal) <|> (try (stringSps "case") >> return ClassModCase) <|> (try (stringSps "package") >> return ClassModPackageObject)
	struct <- (string "class" >> return []) <|> (string "struct" >> return [ClassModStruct]) <|> (string "trait" >> return [ClassModTrait]) <|> (string "object" >> return [ClassModObject])
	sps
	name <- option "" ident
	sps
	generics <- pGenerics
	sps
	fields <- pClassFields
	sps
	extends <- pExtends
	sps
	body <- pClassBody
	sps
	return Class {_classMods = mods ++ struct, className = name, classFields = fields, classExtends = extends, classBody = body, classGenerics = generics, stmAnnotations = a}

pEnum :: [Annotation] -> Parser FileStm
pEnum a = do
	string "enum"
	sps 
	name <- ident
	sps
	generics <- pGenerics
	sps
	fields <- pClassFields
	sps
	extends <- pExtends
	sps
	braces $ let
			enumItem = try $ do
				itemName <- ident
				sps
				pars <- brackets $ pCallPar `sepBy` charSps ','
				sps 
				return $ EnumItem itemName pars
		in do
			items <- many enumItem
			body <- many pStm
			return Enum { className = name, classFields = fields, classExtends = extends, enumItems = items, classBody = body, classGenerics = generics, stmAnnotations = a}


pGenerics :: Parser [Generic]
pGenerics = option [] $ between (charSps '<') (charSps '>') (pClassGeneric `sepBy` charSps ',')
	where
		pClassGeneric = do
			name <- ident
			sps
			extends <- pExtends
			return $ Generic name extends

pExtends :: Parser (Maybe Extends)
pExtends = optionMaybe (do
		try $ string "extends" >> sps1
		cls <- pExtendsCls
		withs <- many $ string "with" >> sps1 >> pExtendsRef
		return $ Extends cls withs
	)
	where 
		pExtendsCls = do
			ref <- pExtendsRef
			p <- option [] $ do
				charSps '('
				pars <- pCallPar `sepBy` charSps ','
				charSps ')'
				return pars
			return $ ExtendsClass ref  p
		pExtendsRef = do
			cls <- ident
			sps
			gens <- pGensRef
			sps
			return (cls, gens)

pClassBody :: Parser [ClassStm]
pClassBody = option [] $ braces $ many pStm

pClassFields :: Parser [ClassStm]
pClassFields = option [] $ brackets $ pConstrField `sepBy` charSps ','


pConstrField :: Parser ClassStm
pConstrField = pDecl' [] (\p -> (optionMaybe p >>= (\mut -> return $ DefModConstructorField : [DefModField | isJust mut] ++ [DefModMutable| isJust mut && fromJust mut] ) ))
pDecl :: [Annotation] -> Parser ClassStm
pDecl ans = pDecl' ans (\ p -> p >>= (\mut -> return $ DefModField : [DefModMutable| mut]))

pDecl' :: [Annotation] -> (Parser Bool -> Parser [DefMod]) -> Parser ClassStm
pDecl' ans mtf = do
		mods <- many pMod
		tpmd <- mtf mutableType
		name <- ident
		sps
		dataType <- optionMaybe $ pDataType False
		sps
		def <- oExp
		sps
		return Def{defName = name, defRetType = dataType, defMods = tpmd ++ mods, defBody = def, defGenerics = [], defPars = [], defAnnotations = ans}
	where
		mutableType = var <|> val
		val = try(string "val" >> sps1)  >> return False
		var = try(string "var" >> sps1) >> return True
		oExp = option Nop (do
			char '='
			sps
			pExp)
	
pMod :: Parser DefMod	
pMod = do
	v <- (try(string "private") >> return DefModPrivate) <|> (try(string "protected") >> return DefModProtected)
		<|> (try(string "final") >> return DefModFinal) <|>  (try(string "override") >> return DefModOverride)
		<|> (try(string "static") >> return DefModStatic) 
		<|> (try(string "weak") >> return DefModWeak) <|> (try(string "delegate") >> return DefModDelegate)
		<|> (try(string "lazy") >> return DefModLazy) <|> (try(string "pure") >> return DefModPure)
		<|> (try(string "inline") >> return DefModInline) <|> (try(string "volatile") >> return DefModVolatile)
	sps1
	return v


pImport :: [Annotation] -> Parser FileStm
pImport a = do
	string "import"
	sps
	ret <- ident `sepBy1` char '.'
	sps
	return $ Import ret a
	

pStm :: Parser ClassStm
pStm = do
	ans <- pAnnotations
	pDef ans  <|> pDecl ans <|> (pImport [] >>= \(Import name _) -> return $ ClassImport name) <?> "Class statement"
	
pDef :: [Annotation] -> Parser ClassStm
pDef ans = do
	mods <- try $ do 
		mds <- many pMod 
		sps
		string "def"
		sps1
		return $ DefModDef : mds
	name <- ident
	sps
	gens <- pGenerics
	sps
	pars <- pDefPars
	sps
	ret <- optionMaybe $ pDataType False
	sps
	(do 
			char '='
			sps
			body <- option Nop pExp 
			sps
			return $ Def mods name gens pars ret body ans
		) <|> (do
			body <- optionMaybe pBraces
			sps
			return $ Def mods name gens pars 
				(Just $ calcTp ret body)
				(fromMaybe Nop body) ans
		)
		where
			calcTp :: Maybe DataType -> Maybe Exp -> DataType
			calcTp Nothing Nothing = DataType "void" []
			calcTp (Just tp) _ = tp
			calcTp Nothing _ = DataType "void" []


pDefPars :: Parser [Par]
pDefPars = option [] (brackets (option [] (pDefPar `sepBy` charSps ',')))

pDefPar :: Parser Par
pDefPar = (do
	mods <- many $ try (string "weak" >> sps1) >> return ParModWeak
	name <- ident
	sps
	tp <- optionMaybe $ pDataType False
	dp <- option Nop (do
		sps
		charSps '='
		pExp)
	return $ Par mods name tp dp) <|> (do
		mods <- many $ try (string "weak" >> sps1) >> return ParModWeak
		tp <- pDataType False
		dp <- option Nop (do
			sps
			charSps '='
			pExp)
		return $ Par mods "" (Just tp) dp)

pExp :: Parser Exp
pExp = do
	o <- pOp0
	sps
	return o
	where
		pOp0 = pOp1 `chainl1` (funcOp "*|*" FuncOpClone <|> funcOp "**" FuncOpClue)
		pOp1 = pOp10 `chainl1` (funcOp ">>" FuncOpBind)
		pOp10 = pOp11 `chainl1` (pBoolOp "||" Or)
		pOp11 = pOp12 `chainl1` (pBoolOp "&&" And)
		pOp12 = pOp13 `chainl1` pCompareOp
		pOp13 = pOp14 `chainl1` pSetOp
		pOp14 = pOp15 `chainl1` (mathOp '+' Plus <|> mathOp '-' Minus)
		pOp15 = pOp16 `chainl1` (mathOp '*' Mul <|> mathOp '/' Div)
		pOp16 = 
			(do
				charSps '!'	
				e <- pTerm
				pf <- postFix e
				return $ Not $ pf)
			<|> (do
				e <- pTerm 
				sps
				postFix e)
		pSetOp = try(do
			char '='
			notFollowedBy $ char '='
			sps
			return $ Set Nothing) <|> pSetOpMath '+' Plus <|> pSetOpMath '-' Minus  <|> pSetOpMath '/' Div  <|> pSetOpMath '*' Mul
		pSetOpMath s t = try(do
			char s
			char '='
			sps
			return $ Set (Just t))
		mathOp s t = try(do 
			char s
			notFollowedBy $ oneOf $ s : "=>*|"
			sps
			return $ MathOp t)
		funcOp s t = try(do 
			string s
			sps
			return $ FuncOp t)
		postFix o = 
			try(do
				string "++"
				sps
				return $ PlusPlus o) <|>
			try(do
				string "--"
				sps
				return $ MinusMinus o) <|>
			try(do 
				charSps '['
				e <- pExp
				sps
				charSps ']' <?> "Array index close bracket"
				postFix $ Index o e) <|>
			try(do 
				charSps '.'
				e <- pTerm
				sps
				postFix $ Dot o e) <|>
			try(do 
				stringSps "?."
				e <- pTerm
				sps
				postFix $ NullDot o e) <|>
			(return o)

pDot :: Parser Exp
pDot = (do
	e <- pTerm 
	sps
	postFix e)
	where postFix o =
		try(do 
			charSps '.'
			e <- pTerm
			sps
			postFix $ Dot o e) <|>
		try(do 
			stringSps "?."
			e <- pTerm
			sps
			postFix $ NullDot o e) <|>
		(return o)

pTerm :: Parser Exp
pTerm = do
		e <- pTerm'
		sps
		return e
	where
		pTerm' = pCase <|> pThrow <|> pLambda <|> pTuple <|> pString <|> pArr <|> pVal <|> pWeak <|> try(pNumConst) <|> pBreak <|> pReturn <|>
			pMinus <|> pBoolConst <|> pBraces <|> pIf <|> pWhile <|> pTry <|> pSync <|> pDo <|> pSelf <|> pSuper <|> pNil <|> pCall  <?> "Expression"

		pMinus = do
			charSps '-'
			e <- pDot
			return $ Negative e
		pBreak = try $ do 
			string "break"
			sps1
			return Break
		pThrow = do
			try $ do
				string "throw"
				sps1
			e <- pExp
			return $ Throw e
		pReturn = do
			try $ do
				string "return"
				sps1
			e <- pExp
			return $ Return e
		pArr = do
			charSps '['
			exps <- pExp `sepBy` charSps ','
			sps
			charSps ']'
			return $ Arr exps
		pWeak = do 
			try $ string "weak" >> sps1
			e <- pExp
			return $ Weak e
		pVal = do
			mods <- try $ do
				mm <- many ((try (string "weak") >> sps1 >> return DefModWeak) 
					<|> (try (string "volatile") >> sps1 >> return DefModVolatile))
				m <- (try (string "val") >> return [] ) <|> (string "var" >> return [DefModMutable]) 
				sps1
				return $ m ++ mm
			name <- ident
			sps
			tp <- optionMaybe (pDataType False)
			sps
			e <- option Nop (charSps '=' >> pExp)
			sps
			return $ Val name tp e mods

		pNumConst = do
			sign <- option 1 (charSps '-' >> return (-1))
			i <- liftM read (many1 digit)
			d <- optionMaybe $ try $ do
				char '.'
				many1 digit
			return $ maybe (IntConst $ sign*i) (FloatConst . (toFloat sign i) ) d
			where
				toFloat :: Int -> Int -> String -> Decimal
				toFloat s a b = read $ (if s == 1 then "" else "-") ++ show a ++ "." ++ b
		pBoolConst = (try(string "true") >> return (BoolConst True)) <|> (try(string "false") >> return (BoolConst False))
		pIf = do
			try $ do 
				string "if"
				sps
				char '('
			sps
			cond <- pExp
			sps
			char ')' <?> "Closing if bracket"
			sps
			i <- pExp
			e <- option Nop (try(string "else") >> sps >> pExp)
			sps
			return $ If cond i e
		pSync = do
			try $ do 
				string "synchronized"
				sps
				charSps '('
			cond <- pExp
			sps
			charSps ')' <?> "Closing while bracket"
			e <- pExp
			sps
			return $ Synchronized cond e	
		pWhile = do
			try $ do 
				string "while"
				sps
				charSps '('
			cond <- pExp
			sps
			charSps ')' <?> "Closing while bracket"
			e <- pExp
			sps
			return $ While cond e
		pTry = do
			e <- try $ do 
				string "try"
				pBraces <|> (sps1 >> pExp)
			sps
			stringSps "finally"
			f <- pExp
			return $ Try e f
		pDo = do
			try $ do 
				string "do"
				sps
				e <- pExp
				sps
				string "while"
				sps
				charSps '('
				cond <- pExp
				sps
				charSps ')'
				return $ Do cond e
		pSelf = try(string "self" >> notFollowedBy ident) >> return Self
		pSuper = try(string "super" >> notFollowedBy ident) >> return Super
		pNil = try(string "nil" >> notFollowedBy ident) >> return Nil

pTuple :: Parser Exp
pTuple = do
	charSps '('
	exps <- pExp `sepBy1` charSps ','
	sps
	charSps ')'
	return $ case exps of
		[e] -> e
		_ -> Tuple exps

pCall :: Parser Exp
pCall = do
	name <- ident
	gens <- pGensRef
	pars <- optionMaybe (do
		try $ wsps >> charSps '('
		ps <- pCallPar `sepBy` charSps ','
		char ')' <?> "Function call close bracket"
		return ps) 
	postLambda <- liftM (fmap (\l -> (Nothing, l))) $ optionMaybe $ try (wsps >> pPostLambda)
	let parsList = fromMaybe [] pars ++ maybeToList postLambda
	return $ Call name (if null parsList && isNothing pars then Nothing else Just parsList) gens

createBraces :: [Exp] -> Exp
createBraces [e] = e
createBraces exps = Braces exps

pString :: Parser Exp
pString = do
	char '"'
	pos <- getPosition
	let 
		startPos = sourceColumn pos
		pStringParts :: Parser Exp
		pStringParts = do 
			parts <- many pPart
			return $ case parts of
				[] -> StringConst ""
				[(part, Nothing)] -> StringConst $ cutString part
				_ -> case last parts of
					(lastPart, Nothing) -> StringBuild (map extract $ init parts) (cutString lastPart)
					_ -> StringBuild (map extract parts) ""
		extract :: ((Int, String), Maybe Exp) -> (String, Exp)
		extract = cutString *** fromMaybe (error "Error in string parsing")
		cutString :: (Int, String) -> String
		cutString (_, "") = ""
		cutString (partStart, str) = let
			stringLines = lines2 str
			maybeDrop n s
				| n <= 0 = s
				| otherwise = dropWhile (== ' ') (take n s) ++ drop n s
			in case stringLines of
				[] -> ""
				_ -> strs "\n" $
					 maybeDrop (startPos - partStart - 1) (head stringLines) 
					 : map (maybeDrop (startPos - 1)) (tail stringLines)
		pStringPart :: Parser String
		pStringPart = many1 (pEscape <|> noneOf "\"$")
		pPart :: Parser ((Int, String), Maybe Exp)
		pPart = do
			p <- getPosition
			((do 
				e <- pStringExp
				return ((sourceColumn p, ""), Just e)
				) <|> (do
				str <- pStringPart 
				e <- optionMaybe pStringExp
				return ((sourceColumn p, str), e)
				))
		pStringExp :: Parser Exp
		pStringExp = try (char '$' >> notFollowedBy (try(string "else") <|> try(string "endif")) ) >> (pIfString <|> pCall <|> brackets pExp)
		pIfString :: Parser Exp
		pIfString = do
			try$ string "if" >> sps >> charSps '('
			cond <- pExp
			char ')'
			t <- pStringParts
			f <- pElseString <|> pEndIfString
			return $ If cond (trimN t) (trimN f)
		pElseString :: Parser Exp 
		pElseString = do
			try $ string "$else"
			pIfString <|> do 
				b <- pStringParts
				pEndIfString
				return b
		trimN :: Exp -> Exp
		trimN (StringConst s) = StringConst (sTrimN s)
		trimN (StringBuild [] s) = StringBuild [] (sTrimN s)
		trimN (StringBuild ((x, e):es) s) = StringBuild ((sStartTrimN x, e) : es) (sTrimN s)
		sStartTrimN = dropWhile (== '\n')
		sEndTrimN = reverse . dropWhile (== '\n') . reverse
		sTrimN = sStartTrimN . sEndTrimN
		pEndIfString :: Parser Exp
		pEndIfString = try (string "$endif") >> return (StringConst "")
		pEscape :: Parser Char
		pEscape = char '\\' >> ((char 'n' >> return '\n') <|> (char 't' >> return '\t') <|> anyChar )
	ret <- pStringParts
	char '"'
	sps
	return ret
		

pGensRef :: Parser [DataType]
pGensRef = option [] $ try $ do
	wsps
	charSps '<'
	r <- pType False `sepBy` charSps ','
	char '>'
	return r


pCase :: Parser Exp
pCase = do
	try $ string "case" >> sps >> charSps '('
	e <- pExp
	sps
	charSps ')'
	br <- optionMaybe $ charSps '{'
	items <- many pCaseItem
	sps
	when(isJust br) $ charSps '}' >> return ()
	return $ Case e items

pCaseItem :: Parser CaseItem
pCaseItem = do
	cond <- try $ do 
		c <- pCaseCondition
		sps
		string "->"
		sps
		return c
	e <- pExp
	sps
	return (cond, e)
pCaseCondition :: Parser CaseCondition
pCaseCondition = unapplyItem <|> typeItem
	where
		unapplyItem = do
			name <- optionMaybe $ try $ do
				n <- ident
				sps
				charSps '@'
				return n
			ref <- try $ do
				n <- option "" ident
				sps
				charSps '('
				return n
			conds <- pCaseCondition `sepBy` charSps ','
			sps
			charSps ')'
			return  $ CaseUnapply name ref conds
		typeItem = do
			cond <- anyItem <|> valItem
			sps
			tp <- optionMaybe $ pDataType True
			return $ maybe cond (\t -> CaseType cond t) tp
		anyItem = try $ do
			char '_'
			sps1
			return CaseAny
		valItem = do
			name <- ident
			sps
			return $ CaseVal name
			
pPostLambda :: Parser Exp
pPostLambda = (do 
		charSps '{'
		pars <- option [] $ try (do		
				r <- pLambdaPar `sepBy` charSps ','
				sps
				string "->"
				sps
				return r)
		exps <- many pExp
		sps
		charSps '}'
		return $ Lambda pars (createBraces exps))

pLambda :: Parser Exp 
pLambda =  (do
	pars <- try $ do 
		r <- lambdaPars
		string "->"
		sps
		return r
	e <- pExp
	return $ Lambda pars e)
	<|> (do 
		pars <- try (do
				charSps '{'
				r <- pLambdaPar `sepBy` charSps ','
				sps
				string "->"
				return r)
		sps
		exps <- many pExp
		sps
		charSps '}'
		return $ Lambda pars (createBraces exps))

	where
		lambdaPars = (do 
			charSps '('
			r <- pLambdaPar `sepBy` charSps ','
			sps
			charSps ')'
			return r) 
			<|> (do
				r <- pLambdaPar
				sps
				return [r])

pLambdaPar :: Parser (String, Maybe DataType)
pLambdaPar = do
	name <- ident
	sps
	tp <- optionMaybe $ pDataType True
	return (name, tp)

pBraces :: Parser Exp
pBraces = liftM Braces $ braces (many (do
			e <- pExp
			sps
			return e))

pCallPar :: Parser CallPar
pCallPar = do
	name <- optionMaybe $ try $ do 
		r <- ident
		sps
		char '='
		notFollowedBy $ char '='
		sps
		return r
	e <- pExp
	sps
	return (name, e)

pCompareOp :: Parser (Exp -> Exp -> Exp)
pCompareOp = try (pBoolOp "===" ExactEq) <|> try (pBoolOp "!==" ExactNotEq) <|> pBoolOp "==" Eq <|> pBoolOp "!=" NotEq <|> pBoolOp ">=" MoreEq <|> pBoolOp "<=" LessEq <|> pBoolOp "<" Less <|> (try $ do
	r <- pBoolOp ">" More  
	notFollowedBy $ char '>'
	return r)

pBoolOp :: String -> BoolTp -> Parser (Exp -> Exp -> Exp)
pBoolOp s t = do 
	try(stringSps s)
	return $ BoolOp t


