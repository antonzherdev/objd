module ObjD.Struct (
	Decl(..),
	FileStm(..),
	Extends,
	ClassStm(..),
	Exp(..),
	Par(..),
	DataType(..),
	File(..),
	Sources,
	ImportType(..),
	isStubDef,
	isClass, isImport,
	stmName,
	isDef, isDecl, isStub
) where
import           Ex.String

type Sources = [File]

data File = File {fileName :: String, fileStms :: [FileStm]}

data Decl = Decl {declName :: String, isDeclMutable :: Bool, declDataType :: Maybe DataType, declDef :: Exp}


data FileStm =
	Import {impString :: String, impType :: ImportType }
	| Class {isStruct :: Bool, className :: String, classFields :: [Decl], classExtends :: Extends, classBody :: [ClassStm] }
	| Stub {isStruct :: Bool, className :: String, classExtends :: Extends, classBody :: [ClassStm]}
	| StubDef {stubDefName :: String, stubDefPars :: [Par], stubDefRetType :: DataType}
isClass :: FileStm -> Bool
isClass (Class {}) = True
isClass _ = False
isStub :: FileStm -> Bool
isStub (Stub {}) = True
isStub _ = False
isImport :: FileStm -> Bool
isImport Import{} = True
isImport _ = False
isStubDef :: FileStm -> Bool
isStubDef (StubDef {}) = True
isStubDef _ = False
data ImportType = ImportTypeCUser | ImportTypeCLib | ImportTypeD

type Extends = Maybe String

data ClassStm = DeclStm {stmDecl :: Decl}
	| Def {defName :: String, defPars :: [Par], defRetType :: Maybe DataType, defBody :: Exp}
stmName :: ClassStm -> String
stmName (DeclStm d) = declName d
stmName (Def name [] _ _) = name
stmName (Def name pars _ _) = name ++ " " ++ unwords (map parName pars)

isDef :: ClassStm -> Bool
isDef Def {} = True
isDef _ = False

isDecl :: ClassStm -> Bool
isDecl DeclStm {} = True
isDecl _ = False


data Par = Par { parName :: String, parType :: DataType }

data Exp = Nop | IntConst Int | Braces [Exp]
	| If Exp Exp Exp
	| Self
	| NotEq Exp Exp | Eq Exp Exp
	| Dot Exp Exp
	| Ref String
	| Set Exp Exp
	| Call String [(String, Exp)]

data DataType = DataType String | DataTypeArr DataType

instance Show Decl where
	show (Decl{declName = name, declDataType = dataType}) = show dataType ++ " " ++ name

instance Show FileStm where
	show (Class{className = name, classFields = fields}) = "class " ++ name ++  "(" ++ strs' ", " fields ++ ")"
	show (Stub{className = name}) = "stub " ++ name
	show (StubDef{stubDefName = name}) = "stub def " ++ name
	show (Import name ImportTypeD) = "import " ++ name
	show (Import name ImportTypeCLib) = "import <" ++ name ++ ">"
	show (Import name ImportTypeCUser) = "import \"" ++ name ++ "\""

instance Show DataType where
	show (DataType s) = s
	show (DataTypeArr r) = "[" ++ show r ++ "]"

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
	show (IntConst i) = show i



