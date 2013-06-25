module ObjD.Parser(
	parseFile, parseStatement
) where

import           Control.Monad
import           ObjD.Struct
import           Text.ParserCombinators.Parsec


parseFile :: String -> Either ParseError [FileStm]
parseFile = parse pFile "ObjD"

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
sps1 = many (char ' ' <|> char '\t'  <|> char '\n') <?> ""
ident :: Parser String
ident = many1 (letter <|> digit <|> oneOf "_")
spsChar :: Char -> Parser Char
spsChar c = sps >> char c
charSps :: Char -> Parser String
charSps c = char c >> sps
pDataType :: Parser DataType
pDataType = do
	charSps ':'
	t <- tp
	sps
	return t
	where 
		tp = arr <|> simple
		arr = do
			charSps '['
			t <- tp
			sps
			char ']'
			return $ DataTypeArr t
		simple = ident >>= \v -> return $ DataType v

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
	try (do
		string "stub"
		sps
		string "def"
		sps)
	name <- ident
	sps
	pars <- pDefPars
	sps
	ret <- option (DataType "void") pDataType
	sps
	return StubDef {stubDefName = name, stubDefPars = pars, stubDefRetType = ret}


pClass :: Parser FileStm
pClass = do
	stub <- option [] $ (try (string "stub")) >> return [ClassModStub]
	sps
	struct <- (string "class" >> return []) <|> (string "struct" >> return [ClassModStruct]) <|> (string "trait" >> return [ClassModTrait])
	sps
	name <- ident
	sps
	fields <- pClassFields
	sps
	extends <- pExtends
	sps
	body <- pClassBody
	sps
	return Class {classMods = stub ++ struct, className = name, classFields = fields, classExtends = extends, classBody = body}

pEnum :: Parser FileStm
pEnum = do
	string "enum"
	sps 
	name <- ident
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
			return Enum { className = name, classFields = fields, classExtends = extends, enumItems = items, classBody = body}


pExtends :: Parser Extends
pExtends = optionMaybe (do
		try $ string "extends"
		sps
		ident
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
		dataType <- optionMaybe pDataType
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
		val = string "val" >> return False
		var = string "var" >> return True
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
	v <- try(string "private") >> (return DefModPrivate)
	sps
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
pStm = pDecl <|> pDef <?> "Class statement"
	
pDef :: Parser ClassStm
pDef = do
	mods <- many pMod 
	sps
	string "def"
	sps1
	static <- option [] $ do 
		try (string "static")
		sps1
		return [DefModStatic]
	name <- ident
	sps
	pars <- pDefPars
	sps
	ret <- optionMaybe pDataType
	sps
	(do 
			char '='
			sps
			body <- option Nop pExp 
			sps
			return $ Def (mods ++ static) name pars ret body
		) <|> (do
			body <- option Nop pBraces
			sps
			return $ Def (mods ++ static) name pars (Just $ DataType "void") body
		)

pDefPars :: Parser [Par]
pDefPars = option [] (brackets (option [] (pDefPar `sepBy` charSps ',')))

pDefPar :: Parser Par
pDefPar = do
	name <- ident
	sps
	tp <- pDataType
	return $ Par name tp

pExp :: Parser Exp
pExp = do
	o <- pOp1
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
		pOp1 = pOp2 `chainl1` pBoolOp
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
			notFollowedBy $ (char '=' <|> char s)
			sps
			return $ MathOp t)

pTerm :: Parser Exp
pTerm = do
		e <- pTerm'
		sps
		return e
	where
		pTerm' = pNumConst <|> pBoolConst <|> pBraces <|> pIf <|> pSelf <|> pNil <|> pCall  <?> "Expression"
		pNumConst = do
			i <- liftM read (many1 digit)
			d <- optionMaybe $ do
				char '.'
				liftM read (many1 digit)
			return $ maybe (IntConst i) (FloatConst i) d
		pBoolConst = (try(string "true") >> return (BoolConst True)) <|> (try(string "false") >> return (BoolConst False))
		pIf = do
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
			(do
				charSps '('
				pars <- pCallPar `sepBy` charSps ','
				charSps ')' <?> "Function call close bracket"
				return $ Call name pars) <|> return (Ref name)

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

pBoolOp :: Parser (Exp -> Exp -> Exp)
pBoolOp =  op "==" Eq <|> op "!=" NotEq <|> op ">=" MoreEq <|> op ">" More  <|> op "<=" LessEq <|> op "<" Less
	where
		op s t = do 
			try(stringSps s)
			return $ BoolOp t


