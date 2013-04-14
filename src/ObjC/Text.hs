module ObjC.Text() where

import Ex.String
import ObjC.Struct

ind = ("    " ++ )
showStms = unlines . stms
unlines' [] = ""
unlines' a = unlines a ++ "\n"

	
instance Show Statement where
	show (Import s) = "#import \"" ++ s ++ "\""
	show (ImportLib s) = "#import <" ++ s ++ ">"
	show (Interface name extends properties funs) = 
		"@interface " ++ name ++ " : " ++ extends ++ "\n"
		 ++ (unlines' . map (show)) properties
		 ++ (unlines  . map (( ++ ";") . show)) funs
		 ++ "@end"
	show i@(Implementation{implName = name, implFields = fields, implSynthenyzes = synzs, implFuns = funs}) = 
		"@implementation " ++ name 
		++ showImplFields fields
		++ showSynthenizes synzs
		++ showImplFuns funs
		++ "@end"

showImplFields [] = "\n"
showImplFields a = "{\n" 
	++ (unlines . map (ind . showImplField)) a
	++ "}\n"
showImplField (ImplField name tp) = tp ++ " " ++ name ++ ";"

showSynthenizes = unlines . map (showSynthenize)
showSynthenize (ImplSynthenyze name "") = "@synthenize " ++ name ++ ";" 
showSynthenize (ImplSynthenyze name var) = "@synthenize " ++ name ++ " = " ++ var ++ ";" 

showImplFuns = unlines . map (("\n" ++ ) . show)
instance Show ImplFun where
	show (ImplFun fun exps) = 
		show fun ++ " {\n"
		++ showStms exps
		++ "}\n"
instance Show Fun where
	show (Fun tp ret name pars) = show tp ++ " (" ++ ret ++ ")" ++ name ++ strs' " " pars
instance Show FunPar where
 	show (FunPar name tp var) = name ++ ":(" ++ tp ++ ")" ++ var 
instance Show FunType where
	show InstanceFun = "-" 
	show ObjectFun = "+" 


instance Show Property where
	show (Property name tp mods) = "@property (" ++ strs' ", " mods ++ ") " ++ tp ++ " " ++ name ++ ";"


instance Show PropertyModifier where
	show ReadOnly = "readonly"
	show NonAtomic = "nonatomic"

instance Show Exp where
	show Self = "self"
	show Super = "super"
	show (Call inst name pars) = "[" ++ show inst ++ " " ++ name ++ (strs " " . map (\(name, e) -> name ++ ":" ++ show e)) pars ++ "]"
	show (Ref name) = name
	show (IntConst i) = show i
	
instance Show Stm where
	show s = unlines $ stmLines s

stms = map (ind) . concatMap (stmLines)
stmLines :: Stm -> [String]
stmLines (If cond t f) = 
	["if(" ++ show cond ++ ") {"] ++ stms t ++ ["}"]
	++ (case f of
		[] -> []
		ff -> ["else {"] ++ stms ff ++ ["}"])
stmLines (Set name e) = [name ++ " = " ++ show e ++ ";"]
stmLines (Stm e) = [show e ++ ";"]
stmLines (Return e) = ["return " ++ show e ++ ";"]
stmLines (Nop) = [""]