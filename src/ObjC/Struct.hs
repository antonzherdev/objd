module ObjC.Struct
( Property(..),
  PropertyModifier(..),
  Statement(..),
  ImplSynthenyze(..),
  ImplFun(..),
  Fun(..),
  FunType(..),
  FunPar(..),
  Stm(..),
  Exp(..),
  ImplField(..)
) where

	

data Statement = 
	Import String | ImportLib String 
	| Interface { interfaceName :: String, extends :: String, properties :: [Property], funs :: [Fun] }
	| Implementation {implName :: String, implFields :: [ImplField], implSynthenyzes :: [ImplSynthenyze], implFuns :: [ImplFun]}

data Property = Property {propertyName :: String, propertyType :: String, propertyModifiers :: [PropertyModifier]}

data PropertyModifier = ReadOnly | NonAtomic deriving(Eq)

data ImplField = ImplField {implFieldName :: String, implFieldType :: String}

data ImplSynthenyze = ImplSynthenyze String String

data ImplFun = ImplFun {implFunType :: Fun, exps :: [Stm]}

data Fun = Fun {funType :: FunType, funReturnType :: String, funName :: String, funPars :: [FunPar]}
data FunType = ObjectFun | InstanceFun
data FunPar = FunPar {funParName :: String, funParDataType :: String, funParVar :: String}

{- EXPRESSIONS -}

data Stm = 
	If Exp [Stm] [Stm]
	| Set Exp Exp
	| Stm Exp
	| Return Exp
	| Nop

data Exp = 
	Self | Super 
	| Call {callInst :: Exp, callName :: String, callPars :: [(String, Exp)]}
	| Ref String
	| IntConst Int
	| Eq Exp Exp | NotEq Exp Exp
	| Dot Exp String
	