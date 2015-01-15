
import Data.List (permutations);

type KnightPos = (Int,Int)
moveKnight :: KnightPos -> [KnightPos]   
moveKnight (c,r) = filter onBoard   
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)   
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)   
    ]   
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]


--in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
in3 :: KnightPos -> [KnightPos]   
in3 start = do    
    first <- moveKnight start   
    second <- moveKnight first   
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool   
canReachIn3 start end = end `elem` in3 start

main = do
	let [x,y] = [(6,2) `canReachIn3` (6,1),	(6,2) `canReachIn3` (6,4)]
	print $ [x,y]
--------------------------------------------------------------------------------------
-- eight queen promble
-- x @ pos 
safe_pos y [] _= True
-- suppose xs are safe already.
safe_pos y (x:xs) n
	| ((y == x)	|| (y == x + n + 1) || (y == x - n - 1))  = False	
	| otherwise = safe_pos y xs (n + 1)

is_safe [] = True
is_safe (x:xs) = safe_pos x xs 0 && is_safe(xs)


-- Eight -queen promble fuc
queen_n n = filter is_safe (permutations[1..n])
