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
	stmName
) where

import qualified ObjC.Struct as C
import Data.List


data Decl = Decl {declMutableType :: MutableType, declName :: String, declDataType :: Maybe DataType, declDef :: Exp}

data MutableType = Var | Val

data Statement = 
	CStatement C.Statement 
	| Class { className :: String, classFields :: [Decl], extends :: Extends, classBody :: [Stm] }
isClass (Class _ _ _ _) = True
isClass _ = False


data Extends = ExtendsNone | Extends String 

data Stm = DeclStm Decl 
	| Def {defName :: String, defPars :: [Par], defRetType :: Maybe DataType, defBody :: Exp}
stmName :: Stm -> String
stmName (DeclStm d) = declName d
stmName (Def name [] _ _) = name
stmName (Def name pars _ _) = name ++ " " ++ (intercalate " " $ map parName pars)

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

