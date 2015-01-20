import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import System.IO 

expr :: Parser Integer
expr = buildExpressionParser table factor <?> "expression" 

whitespaceChar = oneOf " \n\r\f" >> return ()
              <?> "whitespace character"

whitespace = skipMany whitespaceChar <?> "whitespace"

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft],
		 [op "+" (+) AssocLeft, op "-" (-) AssocLeft]]
		where op s f assoc
			= Infix (do { string s; return f} ) assoc

factor = do { char '('
		; x <- expr
		; char ')'
		; return x
		}
		<|> number
		<?> "simple expression"

number :: Parser Integer
number = do{ 
			; try(whitespace)
			; ds <- many1 digit
			; try(whitespace)
			; return (read ds)
		}
		<?> "number"


run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "(unknown)" input) of
	Left err -> do{
		putStr "parse error at ";
		print err
	}
	Right x -> print x


main = do	
	putStrLn "Please enter any expr: "
	input <- getLine
	run expr input
	if length input <= 0 then
		return ()
	else 
		main