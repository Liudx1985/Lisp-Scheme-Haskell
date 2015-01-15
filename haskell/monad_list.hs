import Data.List
import Control.Monad
-- guard  :: (MonadPlus m) => Bool -> m ()
-- guard True   =  return ()
-- guard False  =  mzero

guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = []

isPrime :: Int -> Bool
isPrime n 
	| n <= 2  = True
	| (any (\x -> n `mod` x == 0) [2..(n `div` 2)]) = False
	| otherwise = True

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
		x <- [1..n]
		y <- [x..n]
		guarded (x * y == n) $ 
			return (x, y)

multiplyTo2 :: Int -> [(Int, Int)]
multiplyTo2 n = [(x, y) | x<- [1..n], y <- [1..n], x * y == n]

-- define a dividable: if x can dividable by n return Just x, else zero.
dividable :: (Integral a, MonadPlus m) => a -> a -> m a
x `dividable` n = guard ((x `mod` n) == 0) >> return x

factor n = 
		case find (\x -> n `mod` x == 0) [2..n `div` 2] of -- [2..n/.2]!!a
			Nothing -> [n]
			Just a -> a : factor (n `div` a)

debug = False

main = do
	when debug (putStrLn "debuging")
	unless debug (print $ multiplyTo 8)
	let x = (123 `dividable` 3) :: Maybe Int in
		print $ x
	print $ multiplyTo 27
	print $ multiplyTo2 49
	print $ multiplyTo2 121
	print $ factor 123
	