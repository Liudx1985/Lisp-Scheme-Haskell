-- file: ch16/csv1.hs
import Text.ParserCombinators.Parsec
import System.Environment 
import System.IO

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

-- parse csv string 
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input


--Root>RUNHASKELL Parsec_Csv.hs < prices.csv
main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r
