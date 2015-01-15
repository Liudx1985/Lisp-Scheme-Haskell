zip' ::[a]->[b]->[(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip xs ys

main = do
	let a = [1..5]
	let b = [2..6]
	print(zip' a b)