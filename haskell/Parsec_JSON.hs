
import Text.ParserCombinators.Parsec
import System.Environment 
import System.IO 
import Data.Char
import Numeric (readHex, readSigned, readFloat)
import Control.Monad (liftM4)
import System.IO (Handle)
import Control.Applicative((<*>), (<*), (*>), (<$>), (<$), empty)

data JValue = JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            | JString String
              deriving (Eq, Ord, Show)

-- top level function : parse a json text:
p_text :: CharParser () JValue
p_text = spaces *> text
     <?> "JSON text"
    where text = (p_object)
             <|> (p_array)

p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left parser right =
    between (char left <* spaces) (char right) $
            (parser <* spaces) `sepBy` (char ',' <* spaces)

p_array = JArray <$> p_series '[' p_value ']'

p_object = JObject <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* spaces <* char ':' <* spaces) <*> p_value_choice

p_value :: CharParser () JValue
p_value = value <* spaces
  where value = JNumber <$> p_number
            <|> JBool <$> p_bool
            <|> JNull <$ string "null"
            <|> JString <$> p_string 
            <|> p_object
            <|> p_array
            <?> "JSON value"

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"

p_value_choice = value <* spaces
  where value = choice [JBool <$> p_bool
                       , JNull <$ string "null"	
                       , JString <$> p_string
                       , JNumber <$> p_number
                       , p_object
           			   , p_array                
                       ]
                <?> "JSON value"

p_number :: CharParser () Double
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (p_escape <|> p_unicode)
              <|> satisfy (`notElem` "\"\\")

p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x


run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "(unknown)" input) of
	Left err -> do{
		putStr "parse error at ";
		print err}
	Right x -> print x


test = run p_value "\"FuckP\""
	>> run p_value "true"
	>> run p_value "null"
	>> run p_text "[3.1415926, 1.1 ] "
	>> run p_text "{ \"id\": 0x001f, \"name\" : \"Liudx\" }"

-- another json parser @ http://lpaste.net/55972
{-- test.json content:
{
    "name 0": "value",
    "name 1": "",
    "name 2": "true",
    "name 3": "false",
    "name 4": "null",
    "name 5": "0",
    "name 6": "-5",
    "name 7": "1.1",
    "name 8": "-956.45e+4",
    "name 8": "5956.45E-11",
    "name 9":
    [
        "1",
        "2",
        "3",
        "4"
    ],
    "name 10":
    {
        "a": "b"
    }
}

--}
main = do 
	cont <- readFile "D:/WorkSpace/_Test/test.json"
	run p_text cont