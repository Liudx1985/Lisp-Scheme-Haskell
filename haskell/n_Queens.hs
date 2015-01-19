{-
FROM https://www.haskell.org/haskellwiki/All_About_Monads#The_MonadTrans_and_MonadIO_classes
-}
import Data.Maybe
import Data.Char
import Control.Monad
import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.State

-- this is the type of our problem description
data NQueensProblem = NQP {board::Board,
                           ranks::[Rank],   files::[File],
                           asc::[Diagonal], desc::[Diagonal]}
 
-- initial state = empty board, all ranks, files, and diagonals free
initialState = let fileA = map (\r->Pos A r) [1..8]
                   rank8 = map (\f->Pos f 8) [A .. H]
                   rank1 = map (\f->Pos f 1) [A .. H]
                   asc   = map Ascending (nub (fileA ++ rank1))
                   desc  = map Descending (nub (fileA ++ rank8))
               in NQP (Board []) [1..8] [A .. H] asc desc
 
-- this is our combined monad type for this problem
type NDS a = WriterT [String] (StateT NQueensProblem []) a
 
-- Get the first solution to the problem, by evaluating the solver computation with
-- an initial problem state and then returning the first solution in the result list,
-- or Nothing if there was no solution.
getSolution :: NDS a -> NQueensProblem -> Maybe (a,[String])
getSolution c i = listToMaybe (evalStateT (runWriterT c) i)
 
-- add a Queen to the board in a specific position
addQueen :: Position -> NDS ()
addQueen p = do (Board b) <- gets board
                rs <- gets ranks
                fs <- gets files
                as <- gets asc
                ds <- gets desc
                let b'  = (Piece Black Queen, p):b
                    rs' = delete (rank p) rs
                    fs' = delete (file p) fs
                    (a,d) = getDiags p
                    as' = delete a as
                    ds' = delete d ds
                tell ["Added Queen at " ++ (show p)]
                put (NQP (Board b') rs' fs' as' ds')
 
-- test if a position is in the set of allowed diagonals
inDiags :: Position -> NDS Bool
inDiags p = do let (a,d) = getDiags p
               as <- gets asc
               ds <- gets desc
               return $ (elem a as) && (elem d ds)
 
-- add a Queen to the board in all allowed positions
addQueens :: NDS ()
addQueens = do rs <- gets ranks
               fs <- gets files
               allowed <- filterM inDiags [Pos f r | f <- fs, r <- rs]
               tell [show (length allowed) ++ " possible choices"]
               msum (map addQueen allowed)
 
-- Start with an empty chess board and add the requested number of queens,
-- then get the board and print the solution along with the log
main :: IO ()
main = do args <- getArgs
          let n    = read (args!!0)
              cmds = replicate n addQueens
              sol  = (`getSolution` initialState) $ do sequence_ cmds
                                                       gets board
          case sol of
            Just (b,l) -> do putStr $ show b    -- show the solution
                             putStr $ unlines l -- show the log
            Nothing    -> putStrLn "No solution"