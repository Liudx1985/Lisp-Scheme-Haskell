-- file: ch05/Main.hs

module Main (main) where
import System.IO

import SimpleJSON



main = do
	let obj = JObject [("foo", JNumber 1), ("bar", JBool False)]
	print obj
	writeFile "a.json" (show obj)
	hJson = hReadFile