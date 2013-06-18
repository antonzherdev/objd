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
		txt = 
			"stub CECtrl {\n\
			\   def update\n\
			\}\n\
			\stub CRRailForm\n\
			\stub CEIPoint\n\
			\\n\
			\class CRSwitch(form1 : CRRailForm, form2 : CRRailForm, tile : CEIPoint) extends CECtrl {\n\
			\   var state = 0\n\
			\ \n\
			\   def set(state : int) {\n\
			\      if(self.state == state) {\n\
			\         self.state = state\n\
			\         self.update()\n\
			\      }\n\
			\   }\n\
			\}"
	in do 
		putStrLn txt
		putStrLn "=== Parsing ==="
		od <- (case parseFile txt  of
			Left err -> error $ show err
			Right val -> return val)
		putStrLn $ unlines $ map (show) od 
		(let 
				lnk = link [D.File "main" od]
			in do 
				putStrLn "=== Linking ==="
				putStrLn $ unlines $ map (show) lnk 
				putStrLn "=== Compiling ==="
				(int, impl) <- return $ toObjC $ head $ lnk
				putStrLn "=== h ==="
				putStrLn $ unlines $ map (show) int
				putStrLn "=== m ==="
				putStrLn $ unlines $ map (show) impl)

			
