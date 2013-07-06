module ObjD.Parser(
	parseFile, parseStatement, removeComments
) where

import           Control.Monad
import           Data.Maybe
import           ObjD.Struct
import           Data.Decimal
import           Text.ParserCombinators.Parsec


parseFile :: String -> Either ParseError [FileStm]
parseFile = parse pFile "ObjD" . removeComments


removeComments :: String -> String
removeComments [] = []
removeComments ('/' : '/' : xs) = waitLine xs
	where
		waitLine [] = []
		waitLine ('\n' : xxs) = removeComments xxs
		waitLine (_  : xxs) = waitLine xxs
removeComments ('/' : '*' : xs) = waitMultiLine xs
	where
		waitMultiLine [] = []
		waitMultiLine ('*' : '/' : xxs) = removeComments xxs
		waitMultiLine (_ : xxs) = waitMultiLine xxs
removeComments (x : xs) = x : removeComments xs

pFile :: Parser [FileStm]
pFile = do
	sps
	res <- many pStatement
	eof
	return res

parseStatement :: String -> Either ParseError FileStm
parseStatement = parse pStatement "ObjD"

pStatement :: Parser FileStm
pStatement = pImport <|> pStub <|> pClass <|> pEnum

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
			t <- arr <|> tuple <|> simple
			sps
			r <- if lm then return Nothing else optionMaybe $ do
				string "->"
				sps
				tp True
			sps
			opt <- option False $ charSps '?' >> return True
			let rtp = maybe t (\rr -> DataTypeFun t rr) r
			return $ if opt then DataTypeOption rtp else rtp
		arr = do
			charSps '['
			k <- tp False
			sps
			v <- optionMaybe $ charSps ':' >> tp False
			char ']'
			sps
			return $ maybe (DataTypeArr k) (DataTypeMap k) v
		tuple = do
			charSps '('
			t <- tp False `sepBy` charSps ','
			sps
			char ')'
			sps
			return $ makeTuple t
		makeTuple :: [DataType] -> DataType
		makeTuple [] = DataType "void" []
		makeTuple [v] = v
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

pStub :: Parser FileStm
pStub = do
	mods <- try (do
		string "stub"
		sps
		(string "def" >> return []) <|> (string "val" >> return [StubDefModVal])
		)
	sps
	name <- ident
	sps
	pars <- pDefPars
	sps
	ret <- option (DataType "void" []) (pDataType False)
	sps
	return StubDef {stubDefName = name, stubDefPars = pars, stubDefRetType = ret, stubDefMods = mods}


pClass :: Parser FileStm
pClass = do
	stub <- option [] $ (try (string "stub")) >> return [ClassModStub]
	sps
	struct <- (string "class" >> return []) <|> (string "struct" >> return [ClassModStruct]) <|> (string "trait" >> return [ClassModTrait])
	sps
	name <- ident
	sps
	generics <- pGenerics
	sps
	fields <- pClassFields
	sps
	extends <- pExtends
	sps
	body <- pClassBody
	sps
	return Class {classMods = stub ++ struct, className = name, classFields = fields, classExtends = extends, classBody = body, classGenerics = generics}

pEnum :: Parser FileStm
pEnum = do
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
			return Enum { className = name, classFields = fields, classExtends = extends, enumItems = items, classBody = body, classGenerics = generics}


pGenerics :: Parser [Generic]
pGenerics = option [] $ between (charSps '<') (charSps '>') (pClassGeneric `sepBy` charSps ',')
	where
		pClassGeneric = do
			name <- ident
			sps
			return $ Generic name

pExtends :: Parser (Maybe Extends)
pExtends = optionMaybe (do
		try $ string "extends" >> sps1
		cls <- ident
		sps
		gens <- pGensRef
		sps
		return $ Extends cls gens
	)

pClassBody :: Parser [ClassStm]
pClassBody = option [] $ braces $ many pStm

pClassFields :: Parser [ClassStm]
pClassFields = option [] $ brackets $ pDeclPar `sepBy` charSps ','


pDeclPar :: Parser ClassStm
pDeclPar = pDecl' (option False)
pDecl :: Parser ClassStm
pDecl = pDecl' id

pDecl' :: (Parser Bool->Parser Bool) -> Parser ClassStm
pDecl' mtf = do
		mods <- many pMod
		mut <- mtf mutableType
		name <- ident
		sps
		dataType <- optionMaybe $ pDataType False
		sps
		def <- oExp
		sps
		accs <- option [] (do
			char '|'
			sps
			many acc)
		return Decl{defName = name, defRetType = dataType, defMods = [DefModMutable| mut] ++ mods, defBody = def, declAccs = accs}
	where
		mutableType = do
			v <- try var <|> val
			sps1
			return v
		val = try(string "val") >> return False
		var = try(string "var") >> return True
		oExp = option Nop (do
			char '='
			sps
			pExp)
		acc = try(acct "get" DeclAccRead) <|> try(acct "set" DeclAccWrite)
		acct rvn mdf = do
			m <- many accMod
			sps
			string rvn
			sps
			e <- oExp 
			sps
			return $ mdf m e
		accMod = try (string "private") >> return DeclAccModPrivate
	
pMod :: Parser DefMod	
pMod = do
	v <- (try(string "private") >> return DefModPrivate) <|> (try(string "static") >> return DefModStatic) 
		<|> (try(string "weak") >> return DefModWeak) <|> (try(string "delegate") >> return DefModDelegate)
	sps1
	return v

pImport :: Parser FileStm
pImport = do
	string "import"
	sps
	ret <- pImportLib <|> pImportUser <|> pImportD
	sps
	return ret
	where
	pImportLib = do
		char '<'
		lib <- impIdent
		char '>'
		return $ Import lib ImportTypeCLib
	pImportUser = do
		char '"'
		lib <- impIdent
		char '"'
		return $ Import lib ImportTypeCUser
	pImportD = impIdent >>= \lib -> return $ Import lib ImportTypeD
	impIdent = many1 (letter <|> digit <|> oneOf "_/.")

pStm :: Parser ClassStm
pStm = pDef <|> pDecl <?> "Class statement"
	
pDef :: Parser ClassStm
pDef = do
	mods <- try $ do 
		mds <- many pMod 
		sps
		string "def"
		sps1
		return mds
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
			return $ Def mods name gens pars ret body
		) <|> (do
			body <- optionMaybe pBraces
			sps
			return $ Def mods name gens pars 
				(Just $ calcTp ret body)
				(fromMaybe Nop body)
		)
		where
			calcTp :: Maybe DataType -> Maybe Exp -> DataType
			calcTp Nothing Nothing = DataType "void" []
			calcTp (Just tp) Nothing = tp
			calcTp Nothing _ = DataType "void" []


pDefPars :: Parser [Par]
pDefPars = option [] (brackets (option [] (pDefPar `sepBy` charSps ',')))

pDefPar :: Parser Par
pDefPar = do
	name <- option "" ident
	sps
	tp <- pDataType False
	return $ Par name tp

pExp :: Parser Exp
pExp = do
	o <- pOp0
	sps
	postFix o
	where
		postFix o = try(do
			string "++"
			sps
			return $ PlusPlus o) <|>
			try(do
			string "--"
			sps
			return $ MinusMinus o) <|> (return o)
		pOp0 = pOp1 `chainl1` pAndOrOp
		pOp1 = pOp2 `chainl1` pCompareOp
		pOp2 = pOp3 `chainl1` pSetOp
		pOp3 = pOp4 `chainl1` (mathOp '+' Plus <|> mathOp '-' Minus)
		pOp4 = pOp5 `chainl1` (mathOp '*' Mul <|> mathOp '/' Div)
		pOp5 = pTerm `chainl1` pDot
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
			notFollowedBy $ (char '=' <|> char s <|> char '>')
			sps
			return $ MathOp t)

pTerm :: Parser Exp
pTerm = do
		e <- pTerm'
		sps
		return e
	where
		pTerm' = pThrow <|> pLambda <|> pTuple <|> pString <|> pArr <|> pVal <|> pNumConst <|> pBoolConst <|> pBraces <|> pIf <|> pSelf <|> pNil <|> pCall  <?> "Expression"
		pThrow = do
			try $ do
				string "throw"
				sps1
			e <- pExp
			return $ Throw e
		pArr = do
			charSps '['
			exps <- pExp `sepBy` charSps ','
			sps
			charSps ']'
			return $ Arr exps
		pTuple = do
			charSps '('
			exps <- pExp `sepBy1` charSps ','
			sps
			charSps ')'
			return $ case exps of
				[e] -> e
				_ -> Tuple exps
		pString = do
			char '"'
			s <- manyTill anyChar (char '"')
			return $ StringConst s
		pVal = do
			mods <- try $ do
				m <- (try (string "val") >> return [] ) <|> (string "var" >> return [DefModMutable])
				sps1
				return m
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
			d <- optionMaybe $ do
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
			e <- option Nop (string "else" >> sps >> pExp)
			sps
			return $ If cond i e
		pSelf = try(string "self") >> return Self
		pNil = try(string "nil") >> return Nil
		pCall = do
			name <- ident
			sps
			gens <- pGensRef
			sps
			(do
				charSps '('
				pars <- pCallPar `sepBy` charSps ','
				charSps ')' <?> "Function call close bracket"
				return $ Call name pars gens) <|> (do
				charSps '['
				e <- pExp
				sps
				charSps ']' <?> "Array index close bracket"
				return $ Index (Call name [] gens) e) <|> return (Call name [] gens)

pGensRef :: Parser [DataType]
pGensRef = option [] $ try $ do
	charSps '<'
	r <- pType False `sepBy` charSps ','
	charSps '>'
	return r

pLambda :: Parser Exp 
pLambda =  do
	pars <- try $ do 
		optionMaybe $ charSps '('
		r <- lambdaPars `sepBy` charSps ','
		sps
		optionMaybe $ charSps ')'
		string "->"
		sps
		return r
	e <- pExp
	return $ Lambda pars e
	where
		lambdaPars = do
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
		sps
		return r
	e <- pExp
	sps
	return (name, e)

pDot :: Parser (Exp -> Exp -> Exp)
pDot = stringSps "." >> return Dot

pCompareOp :: Parser (Exp -> Exp -> Exp)
pCompareOp =  pBoolOp "==" Eq <|> pBoolOp "!=" NotEq <|> pBoolOp ">=" MoreEq <|> pBoolOp ">" More  <|> pBoolOp "<=" LessEq <|> pBoolOp "<" Less

pAndOrOp :: Parser (Exp -> Exp -> Exp)
pAndOrOp =  pBoolOp "&&" And <|> pBoolOp "||" Or

pBoolOp :: String -> BoolTp -> Parser (Exp -> Exp -> Exp)
pBoolOp s t = do 
	try(stringSps s)
	return $ BoolOp t


