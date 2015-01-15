-- Qsort using list Comprehension
qsort [] = []
qsort (x:xs) = qsort[y | y <-xs , y < x] 
			++ [x]
			++ qsort[y | y <-xs , y >= x]
			
-- a better qsort using filter
qsort2 [] =[]
qsort2 (x:xs) = qsort2 (filter (< x) xs) 
			++ [x] 
			++ qsort2 (filter (>= x) xs)
	
{--
public static <T extends Comparable<T>> void BubleSort(T []args)
{
	for (int i = 0; i < args.length; ++i) {
		for (int j = args.length - 1; j > i; --j){
			if (args[j].compareTo(args[j - 1]) > 0){
				T t = args[j];
				args[j] = args[j-1];
				args[j - 1] = t;
			}
		}
	}
}
--}

--buble_sort [] = [] 
--buble_sort l = let (la, lb) = partition (maximum l =) l in la ++ (sort lb) 

	
-- main entry:
main =
	print(qsort [10,5,2,8,1,3,7])