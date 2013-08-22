module ObjD.Parser(
	parseFile, parseStatement, removeComments
) where

import           Control.Monad
import           Data.Maybe
import           ObjD.Struct
import           Data.Decimal
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
pStatement = pTypeStm <|> pImport <|> pStub <|> pClass <|> pEnum

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
		arr = do
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
	def <- try (do
		string "stub"
		sps
		pDef <|> pDecl
		)
	sps
	return $ StubDef def

pTypeStm :: Parser FileStm
pTypeStm = do
	try(string "type")
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
	return $ Type name generics (tp, gens)


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
		return Def{defName = name, defRetType = dataType, defMods = DefModVal : [DefModMutable| mut] ++ mods, defBody = def, defGenerics = [], defPars = []}
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
	impIdent = many1 (letter <|> digit <|> oneOf "_/.+")

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
			calcTp (Just tp) _ = tp
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
	return o
	where
		
		pOp0 = pOp1 `chainl1` (pBoolOp "||" Or)
		pOp1 = pOp2 `chainl1` (pBoolOp "&&" And)
		pOp2 = pOp3 `chainl1` pCompareOp
		pOp3 = pOp4 `chainl1` pSetOp
		pOp4 = pOp5 `chainl1` (mathOp '+' Plus <|> mathOp '-' Minus)
		pOp5 = pOp6 `chainl1` (mathOp '*' Mul <|> mathOp '/' Div)
		pOp6 = 
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
			notFollowedBy $ (char '=' <|> char s <|> char '>')
			sps
			return $ MathOp t)
		postFix o = try(do
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
			(return o)

pTerm :: Parser Exp
pTerm = do
		e <- pTerm'
		sps
		return e
	where
		pTerm' = pThrow <|> pLambda <|> pTuple <|> pString <|> pArr <|> pVal <|> try(pNumConst) <|> pBreak <|> pReturn <|>
			pMinus <|> pBoolConst <|> pBraces <|> pIf <|> pWhile <|> pDo <|> pSelf <|> pSuper <|> pNil <|> pCall  <?> "Expression"
		pMinus = do
			charSps '-'
			e <- pExp
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
		pSelf = try(string "self") >> return Self
		pSuper = try(string "super") >> return Super
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
				return $ Call name (Just pars) gens) <|> return (Call name Nothing gens)

pGensRef :: Parser [DataType]
pGensRef = option [] $ try $ do
	charSps '<'
	r <- pType False `sepBy` charSps ','
	charSps '>'
	return r

pLambda :: Parser Exp 
pLambda =  do
	pars <- try $ do 
		r <- (do 
			charSps '('
			r <- lambdaPars `sepBy` charSps ','
			sps
			charSps ')'
			return r) <|> (do
				r <- lambdaPars
				sps
				return [r])
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

pCompareOp :: Parser (Exp -> Exp -> Exp)
pCompareOp =  pBoolOp "==" Eq <|> pBoolOp "!=" NotEq <|> pBoolOp ">=" MoreEq <|> pBoolOp ">" More  <|> pBoolOp "<=" LessEq <|> pBoolOp "<" Less

pBoolOp :: String -> BoolTp -> Parser (Exp -> Exp -> Exp)
pBoolOp s t = do 
	try(stringSps s)
	return $ BoolOp t


