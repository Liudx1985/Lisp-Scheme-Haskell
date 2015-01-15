import Data.Function (on)
import Data.List (sortBy)
main = do 
	let x = [(1,10),(2,9),(3,8),(4,7),(5,6),(6,5),(7,4),(8,3),(9,2),(10,1)]
	print (sortBy (compare `on` snd) x)