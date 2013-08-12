module Main where

import ObjD.ToObjC
import ObjD.Parser
import ObjD.Link as L
import ObjD.Struct as D
import ObjC.Struct as C
import Control.Monad
import Control.Arrow
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath


{- main::IO()
main = putStr "dsa" -}
debug :: [String]
debug = []
main::IO()
main = 
	let
		root = "/Users/antonzherdev/dev/trains3d/Trains3D/"
	in do 
		putStrLn $ "Root: " ++ root
		t <- odFiles root
		print t
		files <- readOdFiles root 
		putStrLn $ "Found " ++ show (length files) ++ " files"
		let 
			parsedFiles :: IO [(FilePath, D.File)]
			parsedFiles = parseFiles files
			linkedFiles :: IO [(FilePath, L.File)]
			linkedFiles = do
				fs <- parsedFiles
				let linked = (uncurry zip . second L.link. unzip) fs
				let errors = checkErrors $ map snd linked 
				forM_ errors (\e -> print e)
				forM_ (filter ((`elem` debug). L.fileName . snd) linked) (\(path, f) -> do
					putStrLn $ "= Linked " ++ path
					print f)
				return linked
			compiledFiles :: IO [(FilePath, ([C.FileStm], [C.FileStm]))]
			compiledFiles = liftM (map (second toObjC) . filter (containsRealStatement . snd)) linkedFiles
			containsRealStatement :: L.File -> Bool
			containsRealStatement L.File{L.fileClasses = clss} = any (not . L.isStub) clss
			textFiles :: IO [(FilePath, (String, String))]
			textFiles = liftM (map (second (toText *** toText))) compiledFiles
			toText :: [C.FileStm] -> String
			toText = unlines . map show
			write :: String -> String -> String -> IO ()
			write _ _ [] = return ()
			write ext path txt = writeFile (replaceExtension path ext) txt 
			in do
				txtFS <- textFiles
				forM_ txtFS (\(path, (h, m)) -> do
					{-putStrLn ("File: " ++ path)-}
					write ".h" path h
					write ".m" path m)
				

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

