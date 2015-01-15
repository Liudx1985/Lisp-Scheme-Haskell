import qualified Data.Char(toUpper)
-- translate all input to upper case
main = do
	interact $ map $ Data.Char.toUpper
