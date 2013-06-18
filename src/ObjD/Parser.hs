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
	res <- many pStatement
	eof
	return res

parseStatement :: String -> Either ParseError FileStm
parseStatement = parse pStatement "ObjD"

pStatement :: Parser FileStm
pStatement = pImport <|> pClass <|> pStub

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
	name <- ident
	sps
	r <- many $ charSps '*'
	return $ foldl (\rr _ -> DataTypeRef rr) (DataType name) r
braces :: Parser a -> Parser a
braces = between (charSps '{') (spsChar '}')
brackets :: Parser a -> Parser a
brackets = between (charSps '(') (spsChar ')')

stringSps :: String -> Parser String
stringSps s = string s >> sps

pStub :: Parser FileStm
pStub = do
	string "stub"
	sps
	name <- ident
	sps
	extends <- pExtends
	sps
	body <- pClassBody
	sps
	return Stub {className = name, classExtends = extends, classBody = body}

pClass :: Parser FileStm
pClass = do
	string "class"
	sps
	name <- ident
	sps
	fields <- pClassFields
	sps
	extends <- pExtends
	sps
	body <- pClassBody
	sps
	return Class {className = name, classFields = fields, classExtends = extends, classBody = body}

pExtends :: Parser Extends
pExtends = optionMaybe (do
		string "extends"
		sps
		ident
	)

pClassBody :: Parser [ClassStm]
pClassBody = option [] $ braces $ many (do
	stm <- pStm
	sps
	return stm)

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
	string "#import"
	sps
	pImportLib <|> pImportUser
	where
	pImportLib = do
		char '<'
		lib <- ident
		char '>'
		return $ Import lib
	pImportUser = do
		char '"'
		lib <- ident
		char '"'
		return $ Import lib

pStm :: Parser ClassStm
pStm = pDeclStm <|> pDef <?> "Class statement"
	where
	pDeclStm = do
		decl <- pDecl
		return $ DeclStm decl

pDef :: Parser ClassStm
pDef = do
	string "def"
	sps1
	name <- ident
	sps
	pars <- option [] (brackets (option [] (pDefPar `sepBy` charSps ',')))
	sps
	ret <- optionMaybe pDataType
	sps
	option ' ' (char '=')
	sps
	body <- option Nop pExp
	return $ Def name pars ret body

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
		pTerm' = pIntConst <|> pBraces <|> pIf <|> pSelf <|> pCall  <?> "Expression"
		pIntConst = liftM (IntConst . read) (many1 digit)
		pBraces = liftM Braces $ braces (many (do
			e <- pExp
			sps
			return e))
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

		pCallPar = do
			name <- ident
			sps
			char '='
			sps
			e <- pExp
			sps
			return (name, e)

pDot :: Parser (Exp -> Exp -> Exp)
pDot = stringSps "." >> return Dot

pEqOp :: Parser (Exp -> Exp -> Exp)
pEqOp = (stringSps "!=" >> return NotEq) <|> (stringSps "==" >> return Eq)


