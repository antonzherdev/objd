module Main where

import ObjD.Struct
import ObjD.ToObjC
import ObjC.Text
import ObjD.Text
import ObjD.Parser


{- main::IO()
main = putStr "dsa" -}

main::IO()
main = 
	let 
		txt = 
			"class CRSwitch(form1 : CRailForm*, form2 : CRRailForm*, tile : CEIPoint) extends CECtrl {\
			\   var state : int = 0\
			\}"
		(int, impl) = toObjC $ case parseFile txt  of
			Left err -> error $ show err
			Right val -> val
	in do 
		putStrLn $ unlines $ map (show) int
		putStrLn $ unlines $ map (show) impl

