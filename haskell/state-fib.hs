-- Simple implement of system random func
import Control.Monad.State
import Control.Monad
import System.Random
-- State s a
tick :: State Int Int
tick = do n <- get
          put (n + 1)
          return n

plusOne :: Int -> Int
plusOne n = execState tick n
-- execState get final state(int), evalState to get final value
plus n x = execState (sequence $ replicate n tick) x

-----------------
data FibState = F {previous, current :: Integer} deriving(Show)
fibState0 = F {previous = 1, current = 0}

currentFib :: State FibState Integer
currentFib = gets current

nextFib :: State FibState Integer
nextFib = do
    F p c <- get
    let n = p + c
    put (F c n)
    return n

getNFibs :: Int -> State FibState [Integer]
getNFibs k = replicateM k nextFib

main :: IO ()
main = do
	print $ plus 100 (plusOne 1)
	print $ evalState (getNFibs 10) fibState0
	-- print $ evalState (liftM2 (:) currentFib (getNFibs 10) ) fibState0

