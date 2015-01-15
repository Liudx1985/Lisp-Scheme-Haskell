import Data.List;
import Data.Char;
-- list :Pattern Match example 
asum :: Num a => [a] -> a
asum []  = 0
asum [x] = 1
asum (x:y:z:xs) = 3
asum (x:y:xs) = 2

-- wild-cards pattern
head' (x:_)             = x
tail' (_:xs)            = xs
-- reuse the s = x:xs
reuse s@(x:xs) = x:s

-- other pattern match example
fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

-- other pattern match example
leftOfEither :: Either a b -> a
leftOfEither (Left a) = a
leftOfEither (Right a) = error "Either.leftOfEither: Nothing"

-- other pattern match example
rightOfEither :: Either a b -> b
rightOfEither (Right a) = a
rightOfEither (Left a) = error "Either.rightOfEither: Nothing"

-- same as: fib = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]
fib@(1:tfib)    = 1 : 1 : [ a+b | (a,b) <- zip fib tfib]

-- 质数分解 Primes of 2[3,5,7,11..]
decompose :: Int ->[Int]
decompose n = 
		case find (\x -> n `mod` x == 0) [2..n `div` 2] of
			Nothing -> [n]
			Just a -> a : decompose (n `div` a)

-- Main Entry routine
main = do
	let a = Just 1
	let b = Nothing
	let c = Left 1
	let d = Right 2
	print $ asum[1];
	print $ asum[1,2];
	print $ asum[1,2,3];
	print (fromJust a)
	print (leftOfEither c, rightOfEither d)
	print $reuse [1, 2]
	print $take 20 fib
	print $ decompose 123