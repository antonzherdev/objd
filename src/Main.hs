module Main where

import ObjD.Struct
import ObjD.ToObjC
import ObjD.Parser
import qualified ObjD.Index as I


{- main::IO()
main = putStr "dsa" -}

main::IO()
main = 
	let 
		txt = 
			"class CRSwitch(form1 : CRailForm*, form2 : CRRailForm*, tile : CEIPoint) extends CECtrl {\n\
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
		od <- (case parseFile txt  of
			Left err -> error $ show err
			Right val -> return val)
		idx <-  I.build [File "main" od]
		(int, impl) <- return $ toObjC (I.getFile "main" idx) od
		putStrLn $ unlines $ map (show) int
		putStrLn $ unlines $ map (show) impl

