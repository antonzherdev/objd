module Main where

import Data.Maybe
import ObjD.ToObjC
import ObjD.Parser
import ObjD.Link as L
import ObjD.Struct as D
import ObjC.Struct as C
import Control.Monad
import Control.Arrow
import System.Directory (doesDirectoryExist, getDirectoryContents, doesFileExist)
import System.IO
import System.FilePath

debug :: [String]
debug = []

main::IO()
main = 
	let
		root = "/Users/antonzherdev/dev/trains3d/Trains3D/"
		
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
			compiledFiles :: IO [(FilePath, ((String, [C.FileStm]), (String, [C.FileStm])))]
			compiledFiles = liftM (map (second toObjC) . filter (containsRealStatement . snd)) linkedFiles
			containsRealStatement :: L.File -> Bool
			containsRealStatement L.File{L.fileClasses = clss} = any (not . L.isStub) clss
			textFiles :: IO [(FilePath, ((String, String), (String, String)))]
			textFiles = liftM (map (second (toText *** toText))) compiledFiles
			toText :: (String, [C.FileStm]) -> (String, String)
			toText = second (unlines . map show)
			write :: String -> String -> String -> IO ()
			write _ _ [] = return ()
			write nm path txt = let fn =  replaceFileName path nm 
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
				txtFS <- textFiles
				forM_ txtFS (\(path, ((hnm, h), (mnm, m)) ) -> do
					{-putStrLn ("File: " ++ path)-}
					write hnm path h
					write mnm path m)
				putStrLn "Finished"

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

