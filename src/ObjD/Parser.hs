module ObjD.Parser where

import qualified ObjC.Struct as C
import Data.Char
import ObjD.Struct 
import Text.ParserCombinators.Parsec


parseFile input = parse pFile "ObjD" input

pFile = many pStatement 

parseStatement input = parse pStatement "ObjD" input

pStatement :: Parser Statement
pStatement = pImport <|> pClass 

sps :: Parser String
sps = many (char ' ' <|> char '\t'  <|> char '\n') <?> ""
sps1 :: Parser String
sps1 = many (char ' ' <|> char '\t'  <|> char '\n') <?> ""
ident :: Parser String
ident = many1 (letter <|> digit <|> oneOf "_") 
spsChar c = sps >> char c
charSps :: Char -> Parser String
charSps c = char c >> sps
pDataType :: Parser DataType
pDataType = do
	name <- ident
	sps
	r <- many $ charSps '*'
	return $ foldl (\r _ -> DataTypeRef r) (DataType name) r
braces = between (charSps '{') (spsChar '}') 
brackets = between (charSps '(') (spsChar ')') 

stringSps s = string s >> sps

pClass :: Parser Statement
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
	return $ Class {className = name, classFields = fields, extends = extends, classBody = body}

pExtends = option (ExtendsNone) (do
		string "extends"
		sps
		cls <- ident
		return $ Extends cls
	)

pClassBody :: Parser [Stm]
pClassBody = option ([]) $ braces $ many (do
	stm <- pStm 
	sps
	return stm)

pClassFields :: Parser [Decl]
pClassFields = option [] $ brackets $ pDeclPar `sepBy` (charSps ',')


pDecl :: Parser Decl
pDeclPar = pDecl' $ option Val (do
	v <- (try(var) <|> val)
	sps1
	return v
	)
pDecl = pDecl' $ (do
	v <- (try(var) <|> val)
	sps1
	return v
	)

pDecl' mtf = do
	mut <- mtf
	name <- ident
	sps
	char ':'
	sps
	dataType <- pDataType
	sps
	def <- option Nop (do 
		char '=' 
		sps 
		e <- pExp
		return e)
	sps
	return $ Decl{declName = name, declDataType = dataType, declMutableType = mut, declDef = def}

val = string "val" >> return Val
var = string "var" >> return Var

pImport = do 
	string "#import"
	sps
	(pImportLib <|> pImportUser)

pImportLib = do 
	char '<'
	lib <- ident
	char '>'
	return $ CStatement $ C.ImportLib lib

pImportUser = do 
	char '"'
	lib <- ident
	char '"'
	return $ CStatement $ C.Import lib

pStm :: Parser Stm
pStm = pDeclStm <|> pDef <?> "Class statement"

pDeclStm = do 
	decl <- pDecl 
	return $ DeclStm decl

pDef :: Parser Stm
pDef = do
	string "def"
	sps1
	name <- ident
	sps
	pars <- option [] (brackets (option [] (pDefPar `sepBy` (charSps ','))))
	sps
	ret <- option (DataType "void") (do
		char ':'
		sps
		tp <- pDataType
		return tp)
	sps
	option ' ' (char '=')
	sps
	body <- pExp
	return $ Def name pars ret body

pDefPar :: Parser Par
pDefPar = do
	name <- ident
	sps
	char ':'
	sps
	tp <- ident
	return $ Par name tp

pExp :: Parser Exp
pExp = pOp1
pOp1 = pOp2 `chainl1` pEqOp
pOp2 = pOp3 `chainl1` pSetOp 
pOp3 = pTerm `chainl1` pDot
pTerm :: Parser Exp
pTerm = do
	e <- pTerm'
	sps
	return e
pTerm' = pIntConst <|> pBraces <|> pIf <|> pSelf <|> pCall  <?> "Expression"

pIntConst = (many1 digit) >>= return . IntConst . read
pBraces = braces (many (do 
	e <- pExp
	sps
	return e)) >>= return . Braces
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

pDot :: Parser (Exp -> Exp -> Exp)
pDot = stringSps "." >> return Dot

pEqOp :: Parser (Exp -> Exp -> Exp)
pEqOp = ((stringSps "!=" >> return NotEq) <|> (stringSps "==" >> return Eq))

pSetOp = try(do
	char '=' 
	notFollowedBy $ char '='
	sps
	return Set)

pCall = do
	name <- ident
	sps 
	(do 
		charSps '(' 
		pars <- (pCallPar `sepBy` (charSps ','))
		charSps ')' <?> "Function call close bracket"
		return $ Call name pars) <|> (return $ Ref name Nothing)

pCallPar = do
	name <- ident
	sps
	char '='
	sps
	e <- pExp
	sps
	return (name, e)
