sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum'(xs);


-- main entry:
main = do
	print("hello world!");
	print("sum 1..100= ", sum'([1..100]));	
	let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
	print(rightTriangles);