
import Text.ParserCombinators.Parsec
import System.Environment 
import System.IO 
import Data.Char
import Numeric (readHex, readSigned, readFloat)

word ::Parser [Char]
word = many1 letter <?> "word";

--  run sentence "hi,di, hi."
--  run sentence "hi,di,hi?"
sentence :: Parser [String]
sentence = do{ 
			words <- sepBy1 word separator
			; oneOf ".?!" <?> "end of sentence"
			; return words 
		}

-- match space | ',' return Parser (){many1 will return matched context}
separator :: Parser ()
separator = skipMany1(space <|> char ',')


--testOr ::Parser [Char]
-- testOr0 = string "(a)" <|> string "(b)" can not parse "(b)"
testOr = do {
			try (string "(a"); char ')'; return "(a)" 
		}
		<|> string "(b)"

parens :: Parser ()
parens = do{ char '('
			; parens
			; char ')'
			; parens
		}
		<|> return()

-- The nesting parser returns the nesting level of the parenthesis. 
nesting :: Parser Int
nesting = do{ char '('
			; n <- nesting
			; char ')'
			; m <- nesting
			; return (max (n+1) m)
		}
		<|> return 0

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "(unknown)" input) of
	Left err -> do{
		putStr "parse error at ";
		print err}
	Right x -> print x

main = run parens "(()(()))"
	>> run nesting "(()(()))"
	>> run testOr "(a)"
	>> run sentence "hehllo,world!"
	>> run sentence "hehllo,Liudx"
