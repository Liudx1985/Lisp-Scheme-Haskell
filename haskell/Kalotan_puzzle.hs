{-
FROM https://www.haskell.org/haskellwiki/All_About_Monads#The_MonadTrans_and_MonadIO_classes
-}
import Data.Maybe
import Control.Monad
import Control.Monad
import Control.Monad.State


type Var   = String
type Value = String
data Predicate = Is    Var Value            -- var has specific value
               | Equal Var Var              -- vars have same (unspecified) value
               | And   Predicate Predicate  -- both are true
               | Or    Predicate Predicate  -- at least one is true
               | Not   Predicate            -- it is not true
               deriving (Eq, Show)
 
type Variables = [(Var,Value)]
 
-- test for a variable NOT equaling a value
isNot :: Var -> Value -> Predicate
isNot var value = Not (Is var value)
 
-- if a is true, then b must also be true
implies :: Predicate -> Predicate -> Predicate
implies a b = Not (a `And` (Not b))
 
-- exclusive or
orElse :: Predicate -> Predicate -> Predicate
orElse a b = (a `And` (Not b)) `Or` ((Not a) `And` b)
 
-- Check a predicate with the given variable bindings.
-- An unbound variable causes a Nothing return value.
check :: Predicate -> Variables -> Maybe Bool
check (Is var value) vars = do val <- lookup var vars
                               return (val == value)
check (Equal v1 v2)  vars = do  val1 <- lookup v1 vars
                                val2 <- lookup v2 vars
                                return (val1 == val2)
check (And p1 p2)    vars = liftM2 (&&) (check p1 vars) (check p2 vars)
check (Or  p1 p2)    vars = liftM2 (||) (check p1 vars) (check p2 vars)
check (Not p)        vars = liftM (not) (check p vars)

-- this is the type of our logic problem
data ProblemState = PS {vars::Variables, constraints::[Predicate]}
 
-- this is our monad type for non-determinstic computations with state
type NDS a = StateT ProblemState [] a
 
-- lookup a variable
getVar :: Var -> NDS (Maybe Value)
getVar v = do vs <- gets vars
              return $ lookup v vs
 
-- set a variable
setVar :: Var -> Value -> NDS ()
setVar v x = do st <- get
                vs' <- return $ filter ((v/=).fst) (vars st)
                put $ st {vars=(v,x):vs'}
 
-- Check if the variable assignments satisfy all of the predicates.
-- The partial argument determines the value used when a predicate returns
-- Nothing because some variable it uses is not set.  Setting this to True
-- allows us to accept partial solutions, then we can use a value of
-- False at the end to signify that all solutions should be complete.
isConsistent :: Bool -> NDS Bool
isConsistent partial = do cs <- gets constraints
                          vs <- gets vars
                          let results = map (\p->check p vs) cs
                          return $ and (map (maybe partial id) results)
 
-- Return only the variable bindings that are complete consistent solutions.
getFinalVars :: NDS Variables
getFinalVars = do c <- isConsistent False
                  guard c
                  gets vars
 
-- Get the first solution to the problem, by evaluating the solver computation with
-- an initial problem state and then returning the first solution in the result list,
-- or Nothing if there was no solution.
getSolution :: NDS a -> ProblemState -> Maybe a
getSolution c i = listToMaybe (evalStateT c i)
 
-- Get a list of all possible solutions to the problem by evaluating the solver
-- computation with an initial problem state.
getAllSolutions :: NDS a -> ProblemState -> [a]
getAllSolutions c i = evalStateT c i

{-
--apply the predicate language to solve:
 The Kalotans are a tribe with a peculiar quirk:
    their males always tell the truth. 
    Their females never make two consecutive true statements, or two consecutive untrue statements.
 An anthropologist (let's call him Worf) has begun to study them. 
 Worf does not yet know the Kalotan language. One day,
 he meets a Kalotan (heterosexual) couple and their child Kibi. 
 Worf asks Kibi: ``Are you a boy? The kid answers in Kalotan,
 which of course Worf doesn't understand. 
 Worf turns to the parents (who know English) for explanation.
      One of them says: "Kibi said: `I am a boy.'" 
      The other adds: "Kibi is a girl. Kibi lied." 
      Solve for the sex of Kibi and the sex of each parent.
-}
-- if a male says something, it must be true
said :: Var -> Predicate -> Predicate
said v p = (v `Is` "male") `implies` p
 
-- if a male says two things, they must be true
-- if a female says two things, one must be true and one must be false
saidBoth :: Var -> Predicate -> Predicate -> Predicate
saidBoth v p1 p2 = And ((v `Is` "male") `implies` (p1 `And` p2))
                       ((v `Is` "female") `implies` (p1 `orElse` p2))
 
-- lying is saying something is true when it isn't or saying something isn't true when it is
lied :: Var -> Predicate -> Predicate
lied v p = ((v `said` p) `And` (Not p)) `orElse` ((v `said` (Not p)) `And` p)
 
-- Test consistency over all allowed settings of the variable.
tryAllValues :: Var -> NDS ()
tryAllValues var = do (setVar var "male") `mplus` (setVar var "female")
                      c <- isConsistent True
                      guard c

main :: IO ()
main = do let variables   = []
              constraints = [ Not (Equal "parent1" "parent2"),
                              "parent1" `said` ("child" `said` ("child" `Is` "male")),
                              saidBoth "parent2" ("child" `Is` "female")
                                                 ("child" `lied` ("child" `Is` "male")) ]
              problem     = PS variables constraints
          print $ (`getSolution` problem) $ do tryAllValues "parent1"
                                               tryAllValues "parent2"
                                               tryAllValues "child"
                                               getFinalVars