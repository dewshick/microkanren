{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad
import Control.Applicative

-- 1. Terms, subscitutions, lookup

type Var = Integer
type Subst = [(Var, Term)]
data Term = Atom String | Pair Term Term | Var Var deriving (Show, Eq)

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

-- 2. KList MonadPlus, lazy interleaving stream of substitutions
-- do we need to keep Delay? maybe we'd be better off redefining MonadPlus for list?

newtype KList a = KList { getList :: [a] } deriving (Functor, Applicative, Alternative, Show)
instance Monad KList where
  KList [] >>= f = mzero
  KList (x:xs) >>= f = f x `mplus` (KList xs >>= f)

instance MonadPlus KList where
  mzero = KList []
  (KList []) `mplus` (KList xs) = KList xs
  (KList (x:xs)) `mplus` (KList ys) = KList $ x:(ys `mplus` xs)

liftKList f = KList . f . getList

-- 3. MicroKanren operators & progrm-related types

type State = (Subst, Integer)
getSubst = fst
getCounter = snd

type Goal = State -> KList State

infixr 4 ===
(===) :: Term -> Term -> Goal
u === v = \(s, c) -> case unify u v s of
  Nothing -> mzero
  Just s' -> return (s', c)

infixr 2 |||
(|||) :: Goal -> Goal -> Goal
p1 ||| p2 = \sc -> (p1 sc) `mplus` (p2 sc)

infixr 3 &&&
(&&&) :: Goal -> Goal -> Goal
p1 &&& p2 = \sc -> (p1 sc) >>= p2

callFresh :: (Term -> Goal) -> Goal
callFresh f = \(s, c) -> f (Var c) (s, c+1)

callFree :: (Term -> Goal) -> Goal
callFree f = \(s, c) -> let freeVar = Var c
                            isNotBound = \(s', c') -> walk freeVar s' == freeVar
                        in liftKList (filter isNotBound) $ callFresh f (s, c)

--(filter (\st -> walk (Var c) st)) (callFresh f (s, c))

-- 4. run, run-star and so on

getAllSolutions :: (Term -> Goal) -> [State]
getAllSolutions f = getList $ callFresh f emptyState

getSolutionForGoal :: Goal -> [State]
getSolutionForGoal g = getList $ g emptyState

--getNSolutions :: Integer -> (Term -> Goal) -> 
--getFirstSolution :: Term -> Goal -> 

emptyState = ([], 0)


-- Tests

five =  (=== (Atom "5"))
fives_ x = x === (Atom "5") ||| (fives_ x) ||| (sixes_ x)
sixes_ x = x === (Atom "6") ||| (fives_ x)

a_and_b = (callFresh (\a -> a === Atom "7")) &&& (callFresh (\b -> b === Atom "5" ||| b === Atom "6"))

boundForall = callFree (\a -> a === Atom "7")
unboundForall = callFree (\a -> callFresh (\b -> b === Pair a a))
boundWithExistential = callFresh (\a -> callFree (\b -> b === Pair a a))

-- bugs related to unification ordering (they want variable on the left for some reason)



