-- file.hs copy from a file to another.
import System.IO

main = do {
	hFile_read 	<- getAndOpenFile "Copy from :" ReadMode;
	hFile_write	<- getAndOpenFile "Copy To :" WriteMode;
	contents	<- hGetContents hFile_read;
	hPutStr hFile_write contents;
	hClose hFile_write;
	putStrLn "Done.";
}

getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode = do 
	do putStr prompt;
	name <- getLine;
	catch (openFile name mode)
		  (\_ -> do putStrLn ("Can not open file" ++ name ++ "\n");
					getAndOpenFile prompt mode)
	