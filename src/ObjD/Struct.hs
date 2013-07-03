module ObjD.Struct (
	FileStm(..), Extends(..), ClassStm(..), Exp(..), Par(..), DataType(..), File(..), Sources, ImportType(..), EnumItem(..), CallPar, DefMod(..), 
	ClassMod(..), DeclAcc(..), DeclAccMod(..), MathTp(..), BoolTp(..), Generic(..), StubDefMod(..),
	isStubDef, isClass, isImport, stmName, isDef, isDecl, isStub, isEnum
) where
import           Ex.String
import 			 Data.Decimal
type Sources = [File]

data File = File {fileName :: String, fileStms :: [FileStm]}

data FileStm =
	Import {impString :: String, impType :: ImportType }
	| Class {classMods :: [ClassMod], className :: String, classFields :: [ClassStm], classExtends :: Maybe Extends, classBody :: [ClassStm]
		, classGenerics :: [Generic] }
	| StubDef {stubDefName :: String, stubDefPars :: [Par], stubDefRetType :: DataType, stubDefMods :: [StubDefMod]}
	| Enum {className :: String, classFields :: [ClassStm], classExtends :: Maybe Extends, enumItems :: [EnumItem], classBody :: [ClassStm]
		, classGenerics :: [Generic] }
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

data ClassMod = ClassModStruct | ClassModStub | ClassModTrait deriving (Eq)

data StubDefMod = StubDefModVal

data Generic = Generic String

data Extends = Extends String [DataType]

data ClassStm = Decl {defMods :: [DefMod], defName :: String, defRetType :: Maybe DataType, defBody :: Exp, declAccs :: [DeclAcc]}
	| Def {defMods :: [DefMod],  defName :: String, defGenerics :: [Generic], defPars :: [Par], defRetType :: Maybe DataType, defBody :: Exp}
stmName :: ClassStm -> String
stmName (Decl _ name _ _ _) = name
stmName (Def _ name _ [] _ _) = name
stmName (Def _ name _ pars _ _) = name ++ " " ++ unwords (map parName pars)

data DefMod = DefModPrivate | DefModMutable | DefModStatic | DefModWeak  deriving (Eq)
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
	| StringConst String
	| FloatConst Decimal
	| Tuple [Exp]
	| Arr [Exp]
	| Braces [Exp]
	| If Exp Exp Exp
	| Self
	| Nil
	| BoolOp BoolTp Exp Exp
	| Dot Exp Exp
	| Set (Maybe MathTp) Exp Exp
	| MathOp MathTp Exp Exp
	| PlusPlus Exp
	| MinusMinus Exp
	| Call String [CallPar] [DataType]
	| Index Exp Exp
	| Lambda [(String, Maybe DataType)] Exp
	| Val{valName :: String, valDataType :: Maybe DataType, valBody :: Exp, valMods :: [DefMod]}
type CallPar = (Maybe String, Exp)

data DataType = DataType String [DataType] | DataTypeArr DataType | DataTypeFun DataType DataType | DataTypeTuple [DataType] | DataTypeMap DataType DataType 
	| DataTypeOption DataType

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
	show (DataType s []) = s
	show (DataType s pars) = s ++ "<" ++ strs' ", " pars ++ ">"
	show (DataTypeArr r) = "[" ++ show r ++ "]"
	show (DataTypeMap k v) = "[" ++ show k ++ " : " ++ show v ++ "]"
	show (DataTypeOption s) = show s ++ "?"

showGens :: [DataType] -> String
showGens [] = ""
showGens a = "<" ++ strs' ", " a  ++ ">"

instance Show Exp where
	show (Braces exps) = "{\n"  ++ strs "\n" (map (ind . show) exps) ++ "\n}"
	show (Arr exps) = "["  ++ strs' ", " exps ++ "]"
	show (Tuple exps) = "("  ++ strs' ", " exps ++ ")"
	show (If cond t Nop) = "if(" ++ show cond ++ ") " ++ show t
	show (If cond t f) = "if(" ++ show cond ++ ") " ++ show t ++ "\nelse " ++ show f
	show Nop = ""
	show Self = "self"
	show (Dot l r) = showOp' l "." r
	show (Set Nothing l r) = showOp l "=" r
	show (Set (Just t) l r) = showOp l (show t ++ "=") r
	show (BoolOp t l r) = showOp l (show t) r
	show (MathOp t l r) = showOp l (show t) r
	show (PlusPlus e) = show e ++ "++"
	show (MinusMinus e) = show e ++ "--"
	show (Call n [] gens) = n ++ showGens gens
	show (Call n pars gens) = n ++ showGens gens ++ "(" ++ strs ", " (map showPar pars) ++ ")"
		where
			showPar (Nothing, e) = show e
			showPar (Just name, e) = name ++ " = " ++ show e
	show (IntConst i) = show i
	show (StringConst i) = show i
	show (BoolConst i) = show i
	show (FloatConst i) = show i
	show (Index v i) = show v ++ "[" ++ show i ++ "]"
	show (Lambda pars e) = strs' ", " (map (\(n, t) -> n ++ maybe "" (\tt -> " : " ++ show tt) t) pars) ++ " -> " ++ show e
	show (Val name tp body mods) = valVar ++ " " ++ name ++ maybe "" ((" : " ++) . show) tp ++ " = " ++ show body
		where valVar = if DefModMutable `elem` mods then "var" else "val"


