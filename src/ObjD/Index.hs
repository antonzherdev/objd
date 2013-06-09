module ObjD.Index (
	build,
	Index, FileIndex, ClassIndex, Field, DT
) where

import Control.Concurrent.MVar
import Control.Monad
import ObjD.Struct
import ObjD.Text
import qualified Data.Map as Map  

type Map = Map.Map

type Sources = [(String, [Statement])]

type Index = Map String FileIndex
data FileIndex = FileIndex {classIndex :: Map String ClassIndex}
data ClassIndex = ClassIndex ClassFieldsIndex
type ClassFieldsIndex = Map String Field
data Field = Field DT
data DT = DTPrimitive String | DTRef DT | DTVoid

type BIndex = Map String BFileIndex
data BFileIndex = BFileIndex {bClassIndex :: Map String BClassIndex}
type BClassIndex = Map String BDesc
data BDesc = BDesc Stm (MVar (Maybe DT))

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
		fileIndex :: [Statement] -> IO BFileIndex
		fileIndex stms = fmap (BFileIndex) $ toMap $ mapM (idx classIndex) $ (map (\cls -> (className cls, cls)). filter (isClass)) stms
		classIndex :: Statement -> IO BClassIndex
		classIndex (Class _ fields _ body) = toMap $ mapM (\stm -> newMVar Nothing >>= (\v -> return (stmName stm, BDesc stm v))) $ (map DeclStm fields) ++ body
		

bToIndex :: BIndex -> IO Index
bToIndex idx = liftMap $ Map.map (fileIndex idx) idx
	where
		liftMap :: Ord a => Map a (IO b) -> IO (Map a b)
		liftMap m =  toMap $ sequence . map (\(a, b) -> b >>= (\v -> return (a, v))) $ Map.toList m
		fileIndex :: BIndex -> BFileIndex -> IO FileIndex 
		fileIndex idx f = do 
		 	clss <- liftMap $ Map.map (classIndex idx) $ bClassIndex f
		 	return $ FileIndex clss
		classIndex :: BIndex -> BClassIndex -> IO ClassIndex
		classIndex idx cls = do
			i <- liftMap $ Map.map (field idx) cls
			return $ ClassIndex i
		field :: BIndex -> BDesc -> IO Field
		field idx desc = do
			dt <- fieldDT idx desc 
			return $ Field dt

fieldDT :: BIndex -> BDesc -> IO DT
fieldDT idx (BDesc field dt) = withMVar dt (\ o -> case o of
	Just x -> return x
	Nothing -> calcStmDT idx field)

calcStmDT :: BIndex -> Stm -> IO DT
calcStmDT idx (DeclStm (Decl {declDataType = (Just dt)})) = return $ toDT idx dt
calcStmDT idx (DeclStm (Decl {declDef = e})) = getExpDT idx e
calcStmDT idx (Def {defRetType = (Just dt)}) = return $ toDT idx dt
calcStmDT idx (Def {defBody = e}) = getExpDT idx e

toDT :: BIndex -> DataType -> DT
toDT idx (DataType tp) = DTPrimitive tp
toDT idx (DataTypeRef tp) = DTRef $ toDT idx tp

getExpDT :: BIndex -> Exp -> IO DT
getExpDT _ (IntConst _) = return $ DTPrimitive "int" 
getExpDT idx (Braces e) = getExpDT idx $ last e
getExpDT idx (If _ e _) = getExpDT idx e
getExpDT idx (Dot _ _) = return DTVoid
getExpDT _ x = error $ "No tExp for " ++ show x



