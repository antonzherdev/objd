module ObjD.Struct (
	Decl(..),
	Statement(..),
	MutableType(..),
	Extends(..),
	Stm(..),
	Exp(..),
	Par(..),
	DataType(..),
	isClass,
	stmName,
	isDef
) where
import           Ex.String
import qualified ObjC.Struct as C


data Decl = Decl {declMutableType :: MutableType, declName :: String, declDataType :: Maybe DataType, declDef :: Exp}

data MutableType = Var | Val

data Statement =
	CStatement C.Statement
	| Class { className :: String, classFields :: [Decl], classExtends :: Extends, classBody :: [Stm] }
isClass :: Statement -> Bool
isClass (Class {}) = True
isClass _ = False


data Extends = ExtendsNone | Extends String

data Stm = DeclStm Decl
	| Def {defName :: String, defPars :: [Par], defRetType :: Maybe DataType, defBody :: Exp}
stmName :: Stm -> String
stmName (DeclStm d) = declName d
stmName (Def name [] _ _) = name
stmName (Def name pars _ _) = name ++ " " ++ unwords (map parName pars)

isDef :: Stm -> Bool
isDef Def {} = True
isDef _ = False


data Par = Par { parName :: String, parType :: String }

data Exp = Nop | IntConst Int | Braces [Exp]
	| If Exp Exp Exp
	| Self
	| NotEq Exp Exp | Eq Exp Exp
	| Dot Exp Exp
	| Ref String
	| Set Exp Exp
	| Call String [(String, Exp)]

data DataType = DataType String | DataTypeRef DataType

ind :: String -> String
ind = ("    " ++ )

instance Show Decl where
	show (Decl{declName = name, declDataType = dataType}) = show dataType ++ " " ++ name

instance Show Statement where
	show (Class{className = name, classFields = fields}) = "class " ++ name ++  "(" ++ strs' ", " fields ++ ")"
	show (CStatement stm) = show stm

instance Show DataType where
	show (DataType s) = s
	show (DataTypeRef r) = show r ++ "*"

showOp :: (Show a, Show b) => a -> String -> b -> String
showOp l op r = show l ++ " " ++ op ++ " " ++ show r
showOp' :: (Show a, Show b) => a -> String -> b -> String
showOp' l op r = show l ++ op ++ show r
instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show Nop = ""
	show Self = "self"
	show (NotEq l r) = showOp l "!=" r
	show (Eq l r) = showOp l "==" r
	show (Dot l r) = showOp' l "." r
	show (Ref s) = s
	show (Set l r) = showOp l "=" r
	show (Call n pars) = n ++ "(" ++ strs' ", " (map showPar pars) ++ ")"
		where
			showPar (name, e) = name ++ " = " ++ show e



