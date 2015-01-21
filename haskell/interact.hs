import System.IO;
import qualified Data.Char(toUpper)
-- to translate all input on stdin to upper,then dump them on stdout
-- [runghc interact.hs ]
-- [runghc interact.hs > "output.txt"] to dump on file
main = do
 	interact $map Data.Char.toUpper



trans_upper = do 
   inh <- openFile "todo.txt" ReadMode
   outh <- openFile "output.txt" WriteMode
   inpStr <- hGetContents inh
   hPutStr outh (map Data.Char.toUpper inpStr)
   hClose inh
   hClose outh