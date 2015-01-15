import qualified Data.Map as Map 
-- Deinfe locker state
data LockerState = Taken | Free deriving (Show, Eq) 

-- class/type/Data :首写字符大写

type Code = (Int, String)
type LockerMap = Map.Map Int (LockerState, Code)

-- Define locker look up
lockerLookup :: Int -> LockerMap -> Either String Code 
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!" 
        Just (state, code) -> if state /= Taken 
                                then Right code 
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap 
lockers = Map.fromList 
    [(100,(Taken, (0 ,"ZD39I"))) 
    ,(101,(Free, (1, "JAH3I")))
    ,(103,(Free, (3, "IQSA9")))
    ,(105,(Free, (5, "QOTSA")))
    ,(109,(Taken, (9, "893JJ")))
    ,(110,(Taken, (10, "99292")))
    ]

main = do
	print(lockers)
	print(lockerLookup 100 lockers)
	print(lockerLookup 101 lockers)
	print(lockerLookup 102 lockers)