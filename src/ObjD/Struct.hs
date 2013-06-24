module ObjD.Struct (
	FileStm(..), Extends, ClassStm(..), Exp(..), Par(..), DataType(..), File(..), Sources, ImportType(..), EnumItem(..), CallPar, DefMod(..), 
	ClassMod(..), DeclAcc(..), DeclAccMod(..),
	isStubDef, isClass, isImport, stmName, isDef, isDecl, isStub, isEnum
) where
import           Ex.String

type Sources = [File]

data File = File {fileName :: String, fileStms :: [FileStm]}

data FileStm =
	Import {impString :: String, impType :: ImportType }
	| Class {classMods :: [ClassMod], className :: String, classFields :: [ClassStm], classExtends :: Extends, classBody :: [ClassStm] }
	| StubDef {stubDefName :: String, stubDefPars :: [Par], stubDefRetType :: DataType}
	| Enum {className :: String, classFields :: [ClassStm], classExtends :: Extends, enumItems :: [EnumItem], classBody :: [ClassStm]}
isClass :: FileStm -> Bool
isClass (Class {}) = True
isClass _ = False
isEnum :: FileStm -> Bool
isEnum (Enum {}) = True
isEnum _ = False
isStub :: FileStm -> Bool
isStub (Class {classMods = mods}) = ClassModStub `elem` mods
isStub _ = False
isImport :: FileStm -> Bool
isImport Import{} = True
isImport _ = False
isStubDef :: FileStm -> Bool
isStubDef (StubDef {}) = True
isStubDef _ = False
data ImportType = ImportTypeCUser | ImportTypeCLib | ImportTypeD

data ClassMod = ClassModStruct | ClassModStub deriving (Eq)

type Extends = Maybe String

data ClassStm = Decl {defMods :: [DefMod], defName :: String, defRetType :: Maybe DataType, defBody :: Exp, declAccs :: [DeclAcc]}
	| Def {defMods :: [DefMod],  defName :: String, defPars :: [Par], defRetType :: Maybe DataType, defBody :: Exp}
stmName :: ClassStm -> String
stmName (Decl _ name _ _ _) = name
stmName (Def _ name [] _ _) = name
stmName (Def _ name pars _ _) = name ++ " " ++ unwords (map parName pars)

data DefMod = DefModPrivate | DefModMutable | DefModStatic  deriving (Eq)
data DeclAcc = DeclAccRead [DeclAccMod] Exp | DeclAccWrite [DeclAccMod] Exp 
data DeclAccMod = DeclAccModPrivate
isDef :: ClassStm -> Bool
isDef Def {} = True
isDef _ = False

isDecl :: ClassStm -> Bool
isDecl Decl {} = True
isDecl _ = False

data EnumItem = EnumItem{enumItemName :: String, enumItemPars :: [CallPar]}

data Par = Par { parName :: String, parType :: DataType }

data Exp = Nop 
	| IntConst Int 
	| BoolConst Bool
	| FloatConst Int Int
	| Braces [Exp]
	| If Exp Exp Exp
	| Self
	| Nil
	| NotEq Exp Exp | Eq Exp Exp
	| Dot Exp Exp
	| Ref String
	| Set Exp Exp
	| Call String [CallPar]
type CallPar = (Maybe String, Exp)

data DataType = DataType String | DataTypeArr DataType

	
instance Show FileStm where
	show (Class{className = name, classFields = fields, classMods = mods, classBody = body}) = 
		tp ++ " " ++ name ++  "(" ++ strs' ", " fields ++ ")" ++ showClassBody body
		where 
			tp = (if ClassModStub `elem` mods then "stub " else "") ++ (if ClassModStruct `elem` mods then "struct" else "class")
	show (Enum{className = name, classFields = fields, classBody = body}) = "enum " ++ name ++  "(" ++ strs' ", " fields ++ ")" ++ showClassBody body
	show (StubDef{stubDefName = name}) = "stub def " ++ name
	show (Import name ImportTypeD) = "import " ++ name
	show (Import name ImportTypeCLib) = "import <" ++ name ++ ">"
	show (Import name ImportTypeCUser) = "import \"" ++ name ++ "\""

showClassBody :: [ClassStm] -> String
showClassBody stms = " {\n" ++ (unlines . map ( ind . show )) stms ++ "\n}"

instance Show ClassStm where
	show (Decl{defName = name, defRetType = Just dataType}) = name ++ " : " ++ show dataType
	show (Decl{defName = name, defRetType = Nothing}) = name
 	show (Def {defName = name }) = "def " ++ name

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
			showPar (Nothing, e) = show e
			showPar (Just name, e) = name ++ " = " ++ show e
	show (IntConst i) = show i
	show (BoolConst i) = show i
	show (FloatConst a b) = show a ++ "." ++ show b



