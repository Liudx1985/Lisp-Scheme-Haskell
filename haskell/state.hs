-- Simple State Implement

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
  return a = State $ \s -> (a, s)

  State act >>= k = State $ \s ->
    let (a, s') = act s
    in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

execState :: State s a -> s -> s
execState act = snd . runState act

evalState :: State s a -> s -> a
evalState act = fst . runState act

-- Test tick
tick :: State Int Int
tick = do n <- get
          put (n + 1)
          return n

plusOne :: Int -> Int
plusOne n = execState tick n


tick2 :: State Int Int
tick2 = tick >> tick


-- execState get final state(int), evalState to get final value
plus n x = execState (sequence $ replicate n tick) x

main :: IO ()
main = do
  print (execState tick2 1);
	print $ plus 100 (plusOne 1)
