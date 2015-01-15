data TrafficLight = Red | Yellow | Green
-- class Eq/LT
-- class Show/Read

instance Ord TrafficLight where
    Green `compare` Yellow  = LT
    Yellow `compare` Red  = LT
    Green `compare` Red  = LT  
    -- Green `compare` Green = EQ
    -- Yellow `compare` Yellow = EQ
    -- Red `compare` Red = EQ
    _ `compare` _ = GT

instance Eq TrafficLight where   
    Red == Red = True   
    Green == Green = True   
    Yellow == Yellow = True   
    _ == _ = False
	
instance Show TrafficLight where   
    show Red = "Red light"   
    show Yellow = "Yellow light"   
    show Green = "Green light"
	
main = do
	let x = Red 
	let	y = Green
	print [x, y]
	print (x < y, x == y, x > y, x /= y)