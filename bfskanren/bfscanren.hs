-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE Rank2Types #-}
import Control.Monad
import Control.Applicative
import Text.Show.Functions
import CustomQueue

type Var = Integer
data Term = Atom String | Pair Term Term | Var Var deriving (Show, Eq)

-- same old stuff, nothing is changed here
type Subst = [(Var, Term)]

walk :: Term -> Subst -> Term
walk (Var x) s = case lookup x s of Nothing -> Var x
                                    Just us -> walk us s
walk v s = v

extendSubst :: Var -> Term -> Subst -> Subst
extendSubst x v s = (x,v):s

unify :: Term -> Term -> Subst -> Maybe Subst
unify u v s = un (walk u s) (walk v s)
  where un (Var x1) (Var x2) | x1 == x2 = return s
        un (Var x) v = return $ extendSubst x v s
        un v (Var x) = return $ extendSubst x v s
        un (Pair h1 t1) (Pair h2 t2) = unify h1 h2 s >>= unify t1 t2
        un (Atom a) (Atom b) | a == b = return s
        un _ _ = mzero

type State = (Subst, Integer)
getSubst = fst
getCounter = snd
emptyState = ([], 0)

--still no changes
type Task = Term -> Goal
data Goal = Fresh Task | Conj [Goal] | Disj [Goal] | Term :===: Term deriving Show
type DelayedGoal = (State, Goal)

reach :: Goal -> [Subst]
reach g = reach' $ enqueueAll [(emptyState, g)] emptyQueue

reach' :: Queue DelayedGoal -> [Subst]
reach' queue = case dequeue queue of
  Nothing -> []
  Just (delayedGoal, queue') -> tryToReach delayedGoal queue'

tryToReach :: DelayedGoal -> Queue DelayedGoal -> [Subst]
tryToReach (state@(subst, counter), goal) queue = let
    evalFresh task f  = tryToReach ((subst, counter + 1), f (task $ Var counter)) queue
    evalDisj goals f  = reach' (enqueueAll (zip (repeat state) $ map f goals) queue)
    evalConj goals f  = tryToReach (state, f goals) queue
    evalUnify t1 t2 f = case unify t1 t2 subst of
        Nothing     -> []
        Just subst' -> f subst
  in case goal of
  Fresh t -> evalFresh t id
  Disj goals -> evalDisj goals id
  Conj goals -> case goals of
    []     -> subst:(reach' queue)
    (x:xs) -> case x of
      Fresh task  -> evalFresh task (Conj . (:xs))
      Disj goals' -> evalDisj goals' (Conj . (:xs))
      Conj goals' -> evalConj goals' (Conj . (++xs))
      t1 :===: t2 -> evalUnify t1 t2 (\subst' -> tryToReach ((subst', counter), Conj xs) queue)
  t1 :===: t2 -> evalUnify t1 t2 (:(reach' queue))


solve :: Task -> [Subst]
solve t = reach $ Fresh t


--enqueueAll []


--TODO
--traverseGoal :: State -> Goal -> KList State
fives_fail x = Disj [fives_fail x, x :===: Atom "5"]