import Control.Monad
import Control.Monad.Writer   
import Control.Monad.Reader

-- gcd' :: Int -> Int -> Int   
-- gcd' a b    
--     | b == 0    = a   
--     | otherwise = gcd' b (a `mod` b)
-- With Writer Monad, it looks like:

gcd' :: Int -> Int -> Writer [String] Int 
gcd' a b   
  | b == 0 = do
      tell ["Finished with " ++ show a]   
      return a   
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]   
      gcd' b $ a `mod` b

-- get the real result of.
lcm' :: Int -> Int -> Int
lcm' a b =  a * b `div` (fst $ runWriter (gcd' a b))

--------------------------------------------------------
-- State
type Stack = [Int]   
   
pop :: Stack -> (Int,Stack)   
pop (x:xs) = (x,xs)   
 
push :: Int -> Stack -> ((),Stack)   
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)   
-- stackManip stack = let   
--  ((),newStack1) = push 3 stack   
--  (a ,newStack2) = pop newStack1   
--  in pop newStack2
stackManip = do
    push 3
    a <- pop   
    pop

testStack = stackManip [5,8,2]

--powerset generate 一个集合所有子集所形成的集合
powerset :: [a] -> [[a]]   
powerset xs = filterM (\x -> [True, False]) xs

--  Reader Monad (->) r 不只是一个 functor 跟一个 applicative functor，
-- 他也是一个 monad。就如其他 monadic value 一般，一个函数也可以被想做是包含一个 context 的。
-- 这个 context 是说我们期待某个值，他还没出现，但我们知道我们会把他当作函数的参数，调用函数来得到结果。
-- 
addStuff :: Int -> Int   
addStuff = do   
  a <- (*2)   
  b <- (+10)   
  return (a+b)


main = do
    print $ fst $ runWriter (gcd' 8 12)
    mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
    print $ addStuff 3
    print $ powerset [1..3]
	--let env = ""
	--runReader (resolve) env