data Person = Person { firstName :: String   
                     , lastName :: String   
                     , age :: Int   
                     , height :: Float   
                     , phoneNumber :: String   
                     , flavor :: String   
                     } deriving (Show)
					 
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
		   
main = do
	let liPingr = Person "Li" "Ping,r" 22 156 "022-884sb" "EatFood"
	print liPingr
	
	let hCar = Car {company="Ford", model="Mustang", year=1967}
	print $ company hCar
	
	
	-- test Day(read, Eq.==, Ord.</>, Bounded.minBound/maxBound, Enum.pred/succ
	
	print (read "Saturday" :: Day)
	
	print (Saturday == Monday)
	print (Saturday `compare` Monday)
	print $ succ Monday
	print $ pred Friday
	
	print (minBound :: Day)
	print (maxBound :: Day)