module Main where

import Data.Maybe
import Ex.String
import ObjC.Generator
import Java.Generator
import ObjD.Parser
import ObjD.Link.Check as L
import ObjD.Link.Link as L
import ObjD.Link.Struct as L
import ObjD.Struct as D
import Java.Struct as J
import ObjC.Struct as C
import Control.Monad
import Control.Arrow
import System.Environment
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist, createDirectoryIfMissing)
import System.IO
import System.FilePath

debug :: [String]
debug = []


data Args = Args{includes :: [String], objCPath :: Maybe String, javaPath :: Maybe String, javaTestPath :: Maybe String}

main::IO()
main = 
	let
		root = "./"
		
		debugFile = Nothing 
		-- debugFile = Just "/Users/antonzherdev/dev/debug.txt"
		fullDebug = False 
		--fullDebug = True
		debugLinkedText = True 
		--debugLinkedText = False
	in do 
		args <- getArgs >>= return . processArgs
		-- putStrLn $ "Root: " ++ root
		files <- readOdFiles root 
		includeFiles <- fmap join $ mapM readOdFiles $ includes args
		putStrLn $ "Found " ++ show (length files) ++ " files"
		let 
			parsedFiles :: IO [(FilePath, D.File)]
			parsedFiles = parseFiles files
			parsedIncludeFiles :: IO [(FilePath, D.File)]
			parsedIncludeFiles = parseFiles includeFiles
			linkedFiles :: L.Lang -> IO ([(FilePath, L.File)], L.Core)
			linkedFiles lang = do
				fs <- parsedFiles
				is <- parsedIncludeFiles
				let 
					linked = L.link lang $ D.Sources (map snd fs) (map snd is)
					errors =  unlines $ map show $ checkErrors linked 
					check = if null errors then return () else error errors
					lf = zip (map fst fs) $ L.sourcesFiles linked
				debugFileHandle <- if isJust debugFile then fmap Just $ openFile (fromJust debugFile) WriteMode else return Nothing
				forM_ (filter ((\f -> f `elem` debug || fullDebug). L.fileName . snd) lf) (\(path, f) -> do
					putStrLn $ "= Linked " ++ path
					when(debugLinkedText) $ print f
					when(isJust debugFileHandle) $ hPrint (fromJust debugFileHandle) f)
				check
				when(isJust debugFile) $ hClose $ fromJust debugFileHandle
				return $ (lf, L.buildCore linked)
			linkedFilesWithRealStatements :: L.Lang -> IO ([(FilePath, L.File)], L.Core)
			linkedFilesWithRealStatements lang = do
				lf <- linkedFiles lang
				let containsRealStatement L.File{L.fileClasses = clss} = any (not . L.isStub) clss
				return $ (filter (containsRealStatement . snd) $ fst lf, snd lf)
			ocCompiledFiles :: IO [(FilePath, ((String, [C.FileStm]), (String, [C.FileStm])))]
			ocCompiledFiles = do
				lfs <- linkedFilesWithRealStatements L.ObjC
				return $ map (second $ toObjC (snd lfs)) $ fst lfs
			ocTextFiles :: IO [(FilePath, ((String, String), (String, String)))]
			ocTextFiles = liftM (map (second (toText *** toText))) ocCompiledFiles
				where toText = second (unlines . map show)
			javaCompiledFiles :: IO [J.File]
			javaCompiledFiles = liftM (concatMap (toJava . snd) . fst) $ linkedFiles L.Java
			javaTextFiles :: IO [(Bool, FilePath, String)]
			javaTextFiles = liftM (map (toText)) javaCompiledFiles
				where toText f@(J.File _ pack _ J.Class{J.className = nm}) = 
					(J.fileIsTest f, strs [pathSeparator] pack ++ [pathSeparator] ++ nm, show f)
			ocWrite :: String -> String -> String -> IO ()
			ocWrite _ _ [] = return ()
			ocWrite nm path txt = let fn =  replaceFileName path nm 
				in do
					e <- testFileEq fn txt
					when(not e) $ do
						print $ "Writing " ++ fn
						createDirectoryIfMissing True $ dropFileName fn
						writeFile fn txt
			testFileEq :: String -> String -> IO Bool
			testFileEq fn txt = doesFileExist fn >>= \e ->
				if e then withFile fn ReadMode $ \h -> checkFile (lines txt) h else return False
			checkFile :: [String] -> Handle -> IO Bool
			checkFile (x:xs) h = hIsEOF h >>= \eof -> if eof then return False else (hGetLine h >>= \l -> if l == x then checkFile xs h else return False)
			checkFile _ h = hIsEOF h
			in do
				when (isJust $ objCPath args) $ do
					let rootPath = fromJust $ objCPath args
					putStrLn $ "Generate Objective-C at " ++ rootPath
					txtFS <- ocTextFiles
					forM_ txtFS (\(path, ((hnm, h), (mnm, m)) ) -> do
						--putStrLn ("File: " ++ path)
						let path' = combine rootPath (drop 2 path)
						ocWrite hnm path' h
						ocWrite mnm path' m)
				when (isJust $ javaPath args) $ do
					let rootPath = fromJust $ javaPath args
					let rootTestPath = fromMaybe rootPath $ javaTestPath args
					putStrLn $ "Generate Java at " ++ rootPath ++ " and tests at " ++ rootTestPath
					txtFS <- javaTextFiles 
					forM_ txtFS (\(isTest, path, txt) -> do
						{-putStrLn ("File: " ++ path)-}
						let fn = addExtension (combine (if isTest then rootTestPath else rootPath) path) "java"
						createDirectoryIfMissing True $ dropFileName fn
						e <- testFileEq fn txt
						when(not e) $ do
							print $ "Writing " ++ fn
							writeFile  fn txt)
				putStrLn "Finished"

processArgs :: [String] -> Args
processArgs args = let
	pairs = zip ("":args) args
	val f = lookup f pairs 
	in Args (maybe [] (splitOn ';') $ val "--include") (val "--obj-c") (val "--java") (val "--java-test")

parseFiles :: [(FilePath, String)] -> IO [(FilePath, D.File)]
parseFiles = mapM parse
	where
		fn = dropExtension . takeFileName
		parse (path, txt) = case parseFile (fn path) txt of
			Left err -> error $ "Error in parse " ++ path ++ ":\n" ++ removeComments txt ++ "\n\nError:" ++ show err
			Right val -> do
				when (fn path `elem` debug) (do
					putStrLn $ "= Parsed " ++ path ++ ":"
					putStrLn $ show val)
				return (path, val)

readOdFiles :: FilePath -> IO [(FilePath, String)]
readOdFiles root = join $ liftM (mapM readOdFile) (odFiles root)
	where
		readOdFile :: FilePath -> IO (FilePath, String)
		readOdFile p = liftM (\content -> (p, content)) (readFile p)

odFiles :: FilePath -> IO [FilePath]
odFiles p = liftM (filter((".od" == ). takeExtension)) (getRecursiveContents p)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

