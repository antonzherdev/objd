module Main where

import ObjD.ToObjC
import ObjD.Parser
import ObjD.Link
import ObjD.Struct as D


{- main::IO()
main = putStr "dsa" -}

main::IO()
main = 
	let
		fileName = "EGStat"
		path = "/Users/antonzherdev/dev/Trains3D/Trains3D/Engine/Time/" ++ fileName
	in do 
		txt <- readFile $ path ++ ".od"
		putStrLn txt
		putStrLn "=== Parsing ==="
		od <- (case parseFile txt  of
			Left err -> error $ show err
			Right val -> return val)
		putStrLn $ unlines $ map (show) od 
		(let 
				lnk = link [D.File fileName od]
			in do 
				putStrLn "=== Linking ==="
				putStrLn $ unlines $ map (show) lnk 
				putStrLn "=== Compiling ==="
				(int, impl) <- return $ toObjC $ head $ lnk
				putStrLn "=== h ==="
				putStrLn $ unlines $ map (show) int
				putStrLn "=== m ==="
				putStrLn $ unlines $ map (show) impl
				writeFile (path ++ ".h") (unlines $ map (show) int)
				writeFile (path ++ ".m") (unlines $ map (show) impl)
				)