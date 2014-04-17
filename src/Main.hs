module Main where

import Data.Maybe
import Ex.String
import ObjC.Generator
import Java.Generator
import ObjD.Parser
import ObjD.Link as L
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

data Args = Args{objCPath :: Maybe String, javaPath :: Maybe String}

main::IO()
main = 
	let
		root = "./"
		--root = "/Users/antonzherdev/dev/trains3d/Trains3D/"
		
		debugFile = Nothing 
		-- debugFile = Just "/Users/antonzherdev/dev/debug.txt"
		fullDebug = False 
		--fullDebug = True
		debugLinkedText = True 
		--debugLinkedText = False
	in do 
		putStrLn $ "Root: " ++ root
		files <- readOdFiles root 
		putStrLn $ "Found " ++ show (length files) ++ " files"
		let 
			parsedFiles :: IO [(FilePath, D.File)]
			parsedFiles = parseFiles files
			linkedFiles :: IO [(FilePath, L.File)]
			linkedFiles = do
				fs <- parsedFiles
				let 
					linked = (uncurry zip . second L.link. unzip) fs
					errors =  unlines $ map show $ checkErrors $ map snd linked 
					check = if null errors then return () else error errors
				debugFileHandle <- if isJust debugFile then fmap Just $ openFile (fromJust debugFile) WriteMode else return Nothing
				forM_ (filter ((\f -> f `elem` debug || fullDebug). L.fileName . snd) linked) (\(path, f) -> do
					putStrLn $ "= Linked " ++ path
					when(debugLinkedText) $ print f
					when(isJust debugFileHandle) $ hPrint (fromJust debugFileHandle) f)
				check
				when(isJust debugFile) $ hClose $ fromJust debugFileHandle
				return linked
			linkedFilesWithRealStatements :: IO [(FilePath, L.File)]
			linkedFilesWithRealStatements = liftM (filter (containsRealStatement . snd)) linkedFiles
				where containsRealStatement L.File{L.fileClasses = clss} = any (not . L.isStub) clss
			ocCompiledFiles :: IO [(FilePath, ((String, [C.FileStm]), (String, [C.FileStm])))]
			ocCompiledFiles = liftM (map (second toObjC)) linkedFilesWithRealStatements
			ocTextFiles :: IO [(FilePath, ((String, String), (String, String)))]
			ocTextFiles = liftM (map (second (toText *** toText))) ocCompiledFiles
				where toText = second (unlines . map show)
			javaCompiledFiles :: IO [J.File]
			javaCompiledFiles = liftM (concatMap (toJava . snd)) linkedFiles
			javaTextFiles :: IO [(FilePath, String)]
			javaTextFiles = liftM (map (toText)) javaCompiledFiles
				where toText f@(J.File pack _ J.Class{J.className = nm}) = ((strs [pathSeparator] pack) ++ [pathSeparator] ++ nm, show f)
			ocWrite :: String -> String -> String -> IO ()
			ocWrite _ _ [] = return ()
			ocWrite nm path txt = let fn =  replaceFileName path nm 
				in do
					e <- testFileEq fn txt
					when(not e) $ do
						print $ "Writing " ++ fn
						writeFile fn txt
			testFileEq :: String -> String -> IO Bool
			testFileEq fn txt = doesFileExist fn >>= \e ->
				if e then withFile fn ReadMode $ \h -> checkFile (lines txt) h else return False
			checkFile :: [String] -> Handle -> IO Bool
			checkFile (x:xs) h = hIsEOF h >>= \eof -> if eof then return False else (hGetLine h >>= \l -> if l == x then checkFile xs h else return False)
			checkFile _ h = hIsEOF h
			in do
				args <- getArgs >>= return . processArgs
				when (isJust $ objCPath args) $ do
					putStrLn "Generate Objective-C"
					txtFS <- ocTextFiles
					forM_ txtFS (\(path, ((hnm, h), (mnm, m)) ) -> do
						{-putStrLn ("File: " ++ path)-}
						ocWrite hnm path h
						ocWrite mnm path m)
				when (isJust $ javaPath args) $ do
					let rootPath = fromJust $ javaPath args
					putStrLn $ "Generate Java at " ++ rootPath
					txtFS <- javaTextFiles
					forM_ txtFS (\(path, txt) -> do
						{-putStrLn ("File: " ++ path)-}
						let fn = addExtension (combine rootPath path) "java"
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
	in Args (val "--obj-c") (val "--java")

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

