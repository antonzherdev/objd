module ObjD.Text() where

import ObjC.Text
import ObjD.Struct
import Ex.String

instance Show Decl where
	show (Decl{declName = name, declDataType = dataType}) = dataType ++ " " ++ name

instance Show Statement where
	show (Class{className = name, classFields = fields}) = "class " ++ name ++  "(" ++ strs' ", " fields ++ ")"
	show (CStatement stm) = show stm