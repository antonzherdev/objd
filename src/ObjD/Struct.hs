module ObjD.Struct (
	Decl(..),
	Statement(..),
	MutableType(..),
	Extends(..),
	Stm(..),
	Exp(..)
) where

import qualified ObjC.Struct as C


data Decl = Decl {declMutableType :: MutableType, declName :: String, declDataType :: String, declDef :: Exp}

data MutableType = Var | Val

data Statement = 
	CStatement C.Statement 
	| Class { className :: String, classFields :: [Decl], extends :: Extends, classBody :: [Stm] }

data Extends = ExtendsNone | Extends String 

data Stm = DeclStm Decl | Fun

data Exp = Nop | IntConst Int