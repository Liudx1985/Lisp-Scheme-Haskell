import Data.Char
import Control.Monad
import Control.Monad.Cont
{- We use the continuation monad to perform "escapes" from code blocks.
   This function implements a complicated control structure to process
   numbers:
 
   Input (n)       Output                       List Shown
   =========       ======                       ==========
   0-9             n                            none
   10-199          number of digits in (n/2)    digits of (n/2)
   200-19999       n                            digits of (n/2)
   20000-1999999   (n/2) backwards              none
   >= 2000000      sum of digits of (n/2)       digits of (n/2)
-}  
fun :: IO String
fun = do n <- (readLn::IO Int)         -- this is an IO monad block
         return $ (`runCont` id) $ do  -- this is a Cont monad block
           str <- callCC $ \exit1 -> do
             when (n < 10) (exit1 (show n))
             let ns = map digitToInt (show (n `div` 2))
             n' <- callCC $ \exit2 -> do
               when ((length ns) < 3) (exit2 (length ns))
               when ((length ns) < 5) (exit2 n)
               when ((length ns) < 7) $ do let ns' = map intToDigit (reverse ns)
                                           exit1 (dropWhile (=='0') ns')
               return $ sum ns
             return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
           return $ "Answer: " ++ str

main = fun