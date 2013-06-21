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
	try (string "stub")
	sps
	pStubDef <|> pStubClass
	where
		pStubClass = do
			struct <- option False (string "struct" >> return True)
			sps
			name <- ident
			sps
			fields <- pClassFields >>= \fs -> return $ map DeclStm fs
			sps
			extends <- pExtends
			sps
			body <- pClassBody
			sps
			return Stub {isStruct = struct, className = name, classExtends = extends, classBody = fields ++ body}
		pStubDef = do
			string "def"
			sps
			name <- ident
			sps
			pars <- pDefPars
			sps
			ret <- option (DataType "void") pDataType
			sps
			return StubDef {stubDefName = name, stubDefPars = pars, stubDefRetType = ret}


pClass :: Parser FileStm
pClass = do
	struct <- (string "class" >> return False) <|> (string "struct" >> return True)
	sps
	name <- ident
	sps
	fields <- pClassFields
	sps
	extends <- pExtends
	sps
	body <- pClassBody
	sps
	return Class {isStruct = struct, className = name, classFields = fields, classExtends = extends, classBody = body}

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

pClassFields :: Parser [Decl]
pClassFields = option [] $ brackets $ pDeclPar `sepBy` charSps ','


pDeclPar :: Parser Decl
pDeclPar = pDecl' (option False)
pDecl :: Parser Decl
pDecl = pDecl' id

pDecl' :: (Parser Bool->Parser Bool) -> Parser Decl
pDecl' mtf = do
		mut <- mtf mutableType
		name <- ident
		sps
		dataType <- optionMaybe pDataType
		sps
		def <- option Nop (do
			char '='
			sps
			pExp)
		sps
		return Decl{declName = name, declDataType = dataType, isDeclMutable = mut, declDef = def}
	where
		mutableType = do
			v <- try var <|> val
			sps1
			return v
		val = string "val" >> return False
		var = string "var" >> return True

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
pStm = do
	stm <- (pDeclStm <|> pDef <?> "Class statement")
	sps
	return stm
	where
	pDeclStm = do
		decl <- pDecl
		return $ DeclStm decl

pDef :: Parser ClassStm
pDef = do
	string "def"
	sps1
	static <- option False $ do 
		try (string "static")
		sps1
		return True
	name <- ident
	sps
	pars <- pDefPars
	sps
	ret <- optionMaybe pDataType
	sps
	( (do 
		char '='
		sps
		body <- option Nop pExp 
		return $ Def static name pars ret body) <|> (do
		body <- option Nop pBraces
		return $ Def static name pars ret body
		))

pDefPars :: Parser [Par]
pDefPars = option [] (brackets (option [] (pDefPar `sepBy` charSps ',')))

pDefPar :: Parser Par
pDefPar = do
	name <- ident
	sps
	tp <- pDataType
	return $ Par name tp

pExp :: Parser Exp
pExp = pOp1
	where
		pOp1 = pOp2 `chainl1` pEqOp
		pOp2 = pOp3 `chainl1` pSetOp
		pOp3 = pTerm `chainl1` pDot
		pSetOp = try(do
			char '='
			notFollowedBy $ char '='
			sps
			return Set)

pTerm :: Parser Exp
pTerm = do
		e <- pTerm'
		sps
		return e
	where
		pTerm' = pNumConst <|> pBraces <|> pIf <|> pSelf <|> pCall  <?> "Expression"
		pNumConst = do
			i <- liftM (read) (many1 digit)
			d <- optionMaybe $ do
				char '.'
				liftM (read) (many1 digit)
			return $ maybe (IntConst i) (FloatConst i) d
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

pEqOp :: Parser (Exp -> Exp -> Exp)
pEqOp = (stringSps "!=" >> return NotEq) <|> (stringSps "==" >> return Eq)


