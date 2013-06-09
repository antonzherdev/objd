module ObjD.Index (
	build,
	Index, File, Class, Field, Type(..),
	getFile, getClass, getField, getFieldType
) where

import Control.Concurrent.MVar
import Control.Monad
import qualified ObjD.Struct as D
import qualified ObjD.Text as D
import qualified Data.Map as Map  

type Map = Map.Map

type Sources = [(String, [D.Statement])]

type Index = Map String File
data File = File {classes :: Map String Class}
data Class = Class ClassFieldsIndex
type ClassFieldsIndex = Map String Field
data Field = Field {fieldType :: Type}
data Type = TypePrimitive String | TypeRef Type | TypeVoid

getFile :: String -> Index -> File
getFile name idx = case Map.lookup name idx of
	Just x -> x
	Nothing -> error $ "No file found " ++ name
getClass :: String -> File -> Class
getClass name (File clsMap) = case Map.lookup name clsMap of
	Just x -> x
	Nothing -> error $ "No class found " ++ name
getField :: String -> Class -> Field
getField name (Class m) = case Map.lookup name m of
	Just x -> x
	Nothing -> error $ "No field found " ++ name
getFieldType :: String -> Class -> Type
getFieldType name cls = fieldType $ getField name cls

instance Show Type where
	show (TypePrimitive x) = x
	show (TypeRef x) = (show x) ++ "*"
	show TypeVoid = "void"

type BIndex = Map String BFile
data BFile = BFile {bClass :: Map String BClass}
type BClass = Map String BDesc
data BDesc = BDesc D.Stm (MVar (Maybe Type))

build :: Sources -> IO Index
build src = do 
	index <- buildB src
	bToIndex index

toMap l = fmap Map.fromList l
		
buildB :: Sources -> IO BIndex
buildB src =  toMap $ mapM (idx fileIndex) src
	where
		idx :: (a -> IO b) -> (String, a) -> IO (String, b)
		idx f (k, v) = f v >>= (\b -> return (k, b))
		fileIndex :: [D.Statement] -> IO BFile
		fileIndex stms = fmap (BFile) $ toMap $ mapM (idx classIndex) $ (map (\cls -> (D.className cls, cls)). filter (D.isClass)) stms
		classIndex :: D.Statement -> IO BClass
		classIndex (D.Class _ fields _ body) = toMap $ mapM (\stm -> newMVar Nothing >>= (\v -> 
			return (D.stmName stm, BDesc stm v))) $ (map D.DeclStm fields) ++ body
		

bToIndex :: BIndex -> IO Index
bToIndex idx = liftMap $ Map.map (fileIndex idx) idx
	where
		liftMap :: Ord a => Map a (IO b) -> IO (Map a b)
		liftMap m =  toMap $ sequence . map (\(a, b) -> b >>= (\v -> return (a, v))) $ Map.toList m
		fileIndex :: BIndex -> BFile -> IO File 
		fileIndex idx f = do 
		 	clss <- liftMap $ Map.map (classIndex idx) $ bClass f
		 	return $ File clss
		classIndex :: BIndex -> BClass -> IO Class
		classIndex idx cls = do
			i <- liftMap $ Map.map (field idx) cls
			return $ Class i
		field :: BIndex -> BDesc -> IO Field
		field idx desc = do
			dt <- calcFieldType idx desc 
			return $ Field dt

calcFieldType :: BIndex -> BDesc -> IO Type
calcFieldType idx (BDesc field dt) = withMVar dt (\ o -> case o of
	Just x -> return x
	Nothing -> calcStmType idx field)

calcStmType :: BIndex -> D.Stm -> IO Type
calcStmType idx (D.DeclStm (D.Decl {D.declDataType = (Just dt)})) = return $ toType idx dt
calcStmType idx (D.DeclStm (D.Decl {D.declDef = e})) = getExpType idx e
calcStmType idx (D.Def {D.defRetType = (Just dt)}) = return $ toType idx dt
calcStmType idx (D.Def {D.defBody = e}) = getExpType idx e

toType :: BIndex -> D.DataType -> Type
toType idx (D.DataType tp) = TypePrimitive tp
toType idx (D.DataTypeRef tp) = TypeRef $ toType idx tp

getExpType :: BIndex -> D.Exp -> IO Type
getExpType _ (D.IntConst _) = return $ TypePrimitive "int" 
getExpType idx (D.Braces e) = getExpType idx $ last e
getExpType idx (D.If _ e _) = getExpType idx e
getExpType idx (D.Dot _ _) = return TypeVoid
getExpType _ x = error $ "No tExp for " ++ show x



