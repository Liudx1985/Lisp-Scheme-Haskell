-- file: ch04/InteractWith.hs
-- use : "gch InteractWith.hs to InteractWith.exe
-- InteractWith.exe "hello-in.txt" "hello-out.txt"

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
        args <- getArgs
        case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

--replace "id" with the name of our function below
myFunction = unwords.reverse.words
