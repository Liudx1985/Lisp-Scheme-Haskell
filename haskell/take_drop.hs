-- drop fake.
myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)


not_null = not.null	-- :i> not_null :: [a] -> Bool
-- take fake.			  
take' :: (Num i, Ord i) => i -> [a] -> [a]   -- declare a function take' with two params type Num & type ord, return a list.
take' n _   
    | n <= 0   = [] -- if Num n | n <= 0 return [] 
take' _ []     = []   -- if list is empty ,return []
take' n (x:xs) = x : take' (n-1) xs -- common style.

reverse' :: [a] -> [a]   -- reverse list.
reverse' [] = []   -- bound condtion.
reverse' (x:xs) = reverse' xs ++ [x]				
				
first (a, b) = a
second (a, b) = b

headof (a,_,_) = a

main = do
	print(reverse' [1..10])