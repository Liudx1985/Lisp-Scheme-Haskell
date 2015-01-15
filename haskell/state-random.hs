
{--古老的LCG(linear congruential generator)代表了最好的伪随机数产生器算法。
主要原因是容易理解，容易实现，而且速度快。
这种算法数学上基于X(n+1) = (a * X(n) + c) % m 
模m, m > 0
系数a, 0 < a < m
增量c, 0 <= c < m
原始值(种子) 0 <= X(0) < m
--部分编译器使用的各个参数值：-------------
Source                 m            a            c          rand() / Random(L)的种子位
Numerical Recipes                  
                       2^32         1664525      1013904223    
Borland C/C++                      
                       2^32         22695477     1          位30..16 in rand(), 30..0 in lrand()
glibc (used by GCC)                
                       2^32         1103515245   12345      位30..0
ANSI C: Watcom, Digital Mars, CodeWarrior, IBM VisualAge C/C++
                       2^32         1103515245   12345      位30..16
Borland Delphi, Virtual Pascal
                       2^32         134775813    1          位63..32 of (seed * L)
Microsoft Visual/Quick C/C++
                       2^32         214013       2531011    位30..16
Apple CarbonLib
                       2^31-1       16807        0          见Park–Miller随机数发生器
--}

import Control.Monad.State
import Control.Monad

-----------------
data RandState = R {previous, current :: Integer} deriving(Show)
seed = R {previous = 13, current = 97} -- seed

currentRand :: State RandState Integer
currentRand = gets current

-- X(n+1) = (a * X(n) + c) % m 
--m =  2^32, a = 1103515245 , c = 12345
nextRand :: State RandState Integer
nextRand = do
    R prev cur <- get
    let m = 2^32
    	a = 1103515245
    	c = 12345
    	next = (a * prev + c) `mod` m
    put (R cur next)
    return next

-- Get N Randoms.
getNRands :: Int -> State RandState [Integer]
getNRands k = replicateM k nextRand 

--nextRandR :: (Int, Int) -> RandState -> Integer
nextRandR (min, max) = 
	(evalState nextRand seed) `mod` (max - min)

{--
TODO:  envelop a state in data struct, to get random number
--}
main :: IO ()
main = do
	print $ map (`mod` 100) $ evalState (getNRands 10) seed
	print $ nextRandR (1, 100)