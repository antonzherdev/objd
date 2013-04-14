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

stringSpaces :: Parser String
stringSpaces = many (char ' ' <|> char '\t'  <|> char '\n')
stringSpaces1 :: Parser String
stringSpaces1 = many (char ' ' <|> char '\t'  <|> char '\n')
ident :: Parser String
ident = many1 (letter <|> digit <|> oneOf "_") 
spaceChar :: Char -> Parser String
spaceChar c = char c >> stringSpaces
pDataType :: Parser String
pDataType = do
	name <- ident
	stringSpaces
	s <- many $ char '*'
	return $ name ++ s
pat = between (spaceChar '{') (char '}') 

pClass :: Parser Statement
pClass = do
	string "class" 
	stringSpaces
	name <- ident
	stringSpaces
	fields <- pClassFields
	stringSpaces
	extends <- pExtends
	stringSpaces
	body <- pClassBody
	return $ Class {className = name, classFields = fields, extends = extends, classBody = body}

pExtends = option (ExtendsNone) (do
		string "extends"
		stringSpaces
		cls <- ident
		return $ Extends cls
	)

pClassBody :: Parser [Stm]
pClassBody = option ([]) $ pat $ many pStm

pClassFields :: Parser [Decl]
pClassFields = option [] $ between (spaceChar '(') (char ')') $ pDecl `sepBy` (spaceChar ',')


pDecl :: Parser Decl
pDecl = do
	mut <- mutableType
	name <- ident
	stringSpaces
	char ':'
	stringSpaces
	dataType <- pDataType
	stringSpaces
	def <- option Nop (do 
		char '=' 
		stringSpaces 
		e <- pExp
		return e)
	stringSpaces
	return $ Decl{declName = name, declDataType = dataType, declMutableType = mut, declDef = def}

mutableType = option Val (do
	v <- (var <|> val)
	stringSpaces1
	return v
	)
val = string "val" >> return Val
var = string "var" >> return Var

pImport = do 
	string "#import"
	stringSpaces
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
pStm = pDeclStm

pDeclStm = do 
	decl <- pDecl 
	return $ DeclStm decl

pExp :: Parser Exp
pExp = pIntConst

pIntConst = (many1 digit) >>= return . IntConst . read
