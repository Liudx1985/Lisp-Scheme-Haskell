-- Monad fake print string append a LF
import Control.Monad


printStr :: String -> IO ()
printStr str = putStrLn str >> putChar '\n'

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b   
applyMaybe Nothing f  = Nothing   
applyMaybe (Just x) f = f x

justH :: Maybe Char   
justH = do   
    (x:xs) <- Just "hello"   
    return x

wopwop :: Maybe Char   
wopwop = do   
    (x:xs) <- Just ""   
    return x

listMonad = do
	show $ [1..5] >>= \x ->[x*2]
	--show $ [1..5] >>= \x ->return (x *2)

show_seven = do
	[ x | x <- [1..50], '7' `elem` show x ]  

-- Get X^-1.
x_1 :: Float ->Maybe Float
x_1 x = if x == 0 then fail "div by zero" else Just (1 / x)

-- you  can use map instead mapM to get [just 1, just 2, just 23] instead of Just[..]
test_mapM = 
	let abs' = \x -> if x < 0 then Just (0 - x) else Just x in mapM abs' [-1,-2,23]



getName name = do let db = [("John", "Smith John"), ("Mike", "Caine Michael")]
                  liftM swapNames (lookup name db) 
                  where swapNames name = unwords.reverse.words $ name

data MyType = MT Int Bool Char Int deriving Show
 
main =  do
	print(Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing,
		Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing)
	-- Find equation that x + y = 7, x,y @[1,6]
	print([(n1, n2) | n1 <- [1..6], n2 <- [1..6], n1 + n2 == 7])
	print $ mapM x_1 [1..5]
	print $ Just 6 >>= x_1
	print $ test_mapM
	print $ getName "John"

