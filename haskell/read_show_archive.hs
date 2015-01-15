years = [1999, 2010, 2012]

main = do
	writeFile "years.txt" (show years)
	input <- readFile "years.txt"
	let years_2 = (read input)::[Int]
	print years_2