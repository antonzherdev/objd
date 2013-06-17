module ObjD.Index (
	build,
	Index, File, Class, Field, Type(..),
	getFile, getClass, getField, getFieldType
) where

import           Control.Concurrent.MVar
import qualified Data.Map                as Map
import qualified ObjD.Struct             as D

type Map = Map.Map

type Index = Map String File
data File = File (Map String Class)
data Class = Class ClassFieldsIndex
type ClassFieldsIndex = Map String Field
data Field = Field {fieldType :: Type}
data Type = TypePrimitive String | TypeRef Type | TypeVoid

findTp :: String -> String -> Map String a -> a
findTp tp name =  Map.findWithDefault (error $ "No " ++ tp ++ " found " ++ name) name

getFile :: String -> Index -> File
getFile = findTp "file"
getClass :: String -> File -> Class
getClass name (File clsMap) = findTp "class" name clsMap
getField :: String -> Class -> Field
getField name (Class m) = findTp "field" name m
getFieldType :: String -> Class -> Type
getFieldType name cls = fieldType $ getField name cls

instance Show Type where
	show (TypePrimitive x) = x
	show (TypeRef x) = show x ++ "*"
	show TypeVoid = "void"

type BIndex = Map String BFile
data BFile = BFile {bClass :: Map String BClass}
type BClass = Map String BDesc
data BDesc = BDesc D.ClassStm (MVar (Maybe Type))

build :: D.Sources -> IO Index
build src = do
	index <- buildB src
	bToIndex index
toMap :: (Functor m, Ord k) => m [(k, a)] -> m (Map k a)
toMap = fmap Map.fromList

buildB :: D.Sources -> IO BIndex
buildB src =  toMap $ mapM (idx fileIndex . tuple) src
	where
		tuple (D.File name stms) = (name, stms)
		idx :: (a -> IO b) -> (String, a) -> IO (String, b)
		idx f (k, v) = f v >>= (\b -> return (k, b))
		fileIndex :: [D.FileStm] -> IO BFile
		fileIndex stms = fmap BFile $ toMap $ mapM (idx classIndex . (\cls -> (D.className cls, cls))) $ filter D.isClass stms
		classIndex :: D.FileStm -> IO BClass
		classIndex (D.Class _ fields _ body) = toMap $ mapM (\stm -> newMVar Nothing >>= (\v ->
			return (D.stmName stm, BDesc stm v))) $ map D.DeclStm fields ++ body


bToIndex :: BIndex -> IO Index
bToIndex idx = liftMap $ Map.map fileIndex idx
	where
		liftMap :: Ord a => Map a (IO b) -> IO (Map a b)
		liftMap m =  toMap $ mapM (\(a, b) -> b >>= (\v -> return (a, v))) $ Map.toList m
		fileIndex :: BFile -> IO File
		fileIndex f = do
		 	clss <- liftMap $ Map.map classIndex $ bClass f
		 	return $ File clss
		classIndex :: BClass -> IO Class
		classIndex cls = do
			i <- liftMap $ Map.map field cls
			return $ Class i
		field :: BDesc -> IO Field
		field desc = do
			dt <- calcFieldType idx desc
			return $ Field dt

calcFieldType :: BIndex -> BDesc -> IO Type
calcFieldType idx (BDesc field dt) = withMVar dt (\ o -> case o of
	Just x -> return x
	Nothing -> calcStmType idx field)

calcStmType :: BIndex -> D.ClassStm -> IO Type
calcStmType idx (D.DeclStm (D.Decl {D.declDataType = (Just dt)})) = return $ toType idx dt
calcStmType idx (D.DeclStm (D.Decl {D.declDef = e})) = getExpType idx e
calcStmType idx (D.Def {D.defRetType = (Just dt)}) = return $ toType idx dt
calcStmType idx (D.Def {D.defBody = e}) = getExpType idx e

toType :: BIndex -> D.DataType -> Type
toType _ (D.DataType tp) = TypePrimitive tp
toType idx (D.DataTypeRef tp) = TypeRef $ toType idx tp

getExpType :: BIndex -> D.Exp -> IO Type
getExpType _ (D.IntConst _) = return $ TypePrimitive "int"
getExpType idx (D.Braces e) = getExpType idx $ last e
getExpType idx (D.If _ e _) = getExpType idx e
getExpType _ (D.Dot _ _) = return TypeVoid
getExpType _ x = error $ "No tExp for " ++ show x



