import Control.Monad
import Data.Char
import System.Environment
-- we can parse three different types of terms
data Parsed = Digit Integer | Hex Integer | Word String deriving Show
 
-- attempts to add a character to the parsed representation of a hex digit
parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex n) c = if isHexDigit c then
                            return (Hex ((n*16) + (toInteger (digitToInt c))))
                          else
                            mzero
parseHexDigit _       _ = mzero
 
-- attempts to add a character to the parsed representation of a decimal digit
parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit n) c = if isDigit c then
                           return (Digit ((n*10) + (toInteger (digitToInt c))))
                         else
                           mzero
parseDigit _         _ = mzero
 
-- attempts to add a character to the parsed representation of a word
parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word s) c = if isAlphaNum c then -- isAlpha
                         return (Word (s ++ [c]))
                       else
                         mzero
parseWord _        _ = mzero
 
-- tries to parse the digit as a hex value, a decimal value and a word
-- the result is a list of possible parses
parse :: Parsed -> Char -> [Parsed]
parse p c = (parseHexDigit p c) `mplus` (parseDigit p c) `mplus` (parseWord p c)
 
-- parse an entire String and return a list of the possible parsed values
parseArg :: String -> [Parsed]
parseArg s = do init <- (return (Hex 0)) `mplus` (return (Digit 0)) `mplus` (return (Word ""))
                foldM parse init s

main = do
	print $ map parseArg ["7970", "FE", "A780"]