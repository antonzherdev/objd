module ObjD.Struct (
	Decl(..),
	Statement(..),
	MutableType(..),
	Extends(..),
	Stm(..),
	Exp(..),
	Par(..)
) where

import qualified ObjC.Struct as C


data Decl = Decl {declMutableType :: MutableType, declName :: String, declDataType :: String, declDef :: Exp}

data MutableType = Var | Val

data Statement = 
	CStatement C.Statement 
	| Class { className :: String, classFields :: [Decl], extends :: Extends, classBody :: [Stm] }

data Extends = ExtendsNone | Extends String 

data Stm = DeclStm Decl 
	| Def {defName :: String, defPars :: [Par], defRetType :: String, defBody :: Exp}
data Par = Par { parName :: String, parType :: String }

data Exp = Nop | IntConst Int | Braces [Exp]
	| If Exp Exp Exp
	| Self
	| NotEq Exp Exp | Eq Exp Exp
	| Dot Exp Exp
	| Ref String
	| Set Exp Exp
	| Call String [(String, Exp)]