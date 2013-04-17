module ObjD.Text() where

import ObjC.Text
import ObjD.Struct
import Ex.String

ind = ("    " ++ )

instance Show Decl where
	show (Decl{declName = name, declDataType = dataType}) = show dataType ++ " " ++ name

instance Show Statement where
	show (Class{className = name, classFields = fields}) = "class " ++ name ++  "(" ++ strs' ", " fields ++ ")"
	show (CStatement stm) = show stm

instance Show DataType where
	show (DataType s) = s
	show (DataTypeRef r) = show r ++ "*"

showOp l op r = show l ++ " " ++ op ++ " " ++ show r
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
	show (Ref s _) = s
	show (Set l r) = showOp l "=" r
	show (Call n pars) = n ++ "(" ++ strs' ", " (map showPar pars) ++ ")"
		where
			showPar (n, e) = n ++ " = " ++ show e

