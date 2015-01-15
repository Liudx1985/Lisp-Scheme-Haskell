import System.Random
import Control.Monad(when)
import Control.Monad.State


data MyType = MT Int Bool Char Int deriving Show
 
{- Using the State monad, we can define a function that returns
   a random value and updates the random generator state at
   the same time.
-}
getAny :: (Random a) => State StdGen a
getAny = do g      <- get
            (x,g') <- return $ random g
            put g'
            return x
 
-- similar to getAny, but it bounds the random value returned
getOne :: (Random a) => (a,a) -> State StdGen a
getOne bounds = do g      <- get
                   (x,g') <- return $ randomR bounds g
                   put g'
                   return x
 
{- Using the State monad with StdGen as the state, we can build
   random complex types without manually threading the
   random generator states through the code.
-}   
makeRandomValueST :: StdGen -> (MyType, StdGen)
makeRandomValueST = runState (do n <- getOne (1,100)
                                 b <- getAny
                                 c <- getOne ('a','z')
                                 m <- getOne (-n,n)
                                 return (MT n b c m))



testS = runState (do x <- getAny    return x)

main = do
    gen <- getStdGen
    print $ testS gen
    print $ fst $ makeRandomValueST gen