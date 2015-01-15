{--
 using recursing.
--}
--------------------------------
take' :: (Num i, Ord i) => i -> [a] -> [a]   
take' n _   
    | n <= 0   = []   
take' _ []     = []   

take' n (x:xs) = x : take' (n-1) xs
--------------------------------
nth :: (Num i, Ord i) => i -> [a] -> a 
nth n _ 
	| n < 0 = error "underflow"

nth _ [] = error "underflow"

nth 0 (x:xs) = x
nth n (x:xs) = nth (n - 1) xs

--------------------------------
fib :: (Num a1, Num a, Eq a1) => a1 -> a
fib n 
	| (n == 0) = a
	| (n == 1) = b
	| otherwise = fib (n - 1) + fib (n - 2)
	where a = 1; b = 1

--------------------------------
filter' fn [] = []
filter' fn (x:xs)
	| (fn x) = x : (filter' fn xs)
	| otherwise = (filter' fn xs)

--------------------------------
map' :: (t -> t1) -> [t] -> [t1]
map' _ [] = [];
map' f (x:xs) = f x : map' f xs


--------------------------------
replicate' :: Int -> t -> [t]
replicate' n a 
	| (n < 0) = []
	| otherwise = a : replicate (n - 1) a



-- Main entry.
main = do 
	let x = [1..]
	print(nth 100 x)
	print(fib 15)
	{--
		Notice can not use filter on infinite seq, may never ends 
	--}
	print(filter' (\x -> x < 5) [1..10]) -- Lambda (x) return x < 5


