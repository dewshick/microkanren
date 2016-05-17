{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad
import Control.Applicative

-- 1. Terms, subscitutions, lookup

type Var = Integer
type Subst = [(Var, Term)]
data Term = Atom String | Pair Term Term | Var Var | EigenVar Var deriving (Show, Eq)

walk :: Term -> Subst -> Term
walk (Var x) s = case lookup x s of Nothing -> Var x
                                    Just us -> walk us s
walk v s = v

extendSubst :: Var -> Term -> Subst -> Subst
extendSubst x v s = (x,v):s

unify :: Term -> Term -> Subst -> Maybe Subst
unify u v s = un (walk u s) (walk v s)
  where unifyVar x v s = if eigenFail x v then Nothing else Just $ extendSubst x v s
        un (Var x1) (Var x2) | x1 == x2 = return s
        --un (EigenVar v1) (Var v2) = if v1 > v2 then  else
        un (Var x) v = unifyVar x v s
        un v (Var x) = unifyVar x v s
        un (EigenVar x) _ = mzero
        un _ (EigenVar x) = mzero
        un (Pair h1 t1) (Pair h2 t2) = unify h1 h2 s >>= unify t1 t2
        un (Atom a) (Atom b) | a == b = return s
        un _ _ = mzero

eigenFail :: Var -> Term -> Bool
eigenFail n term = case term of
  EigenVar v -> n < v
  Pair t1 t2 -> (eigenFail n t1) || (eigenFail n t2)
  _          -> False

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

--fresh :: ([Term] -> Goal) -> Goal
--fresh terms goal = foldl callFresh 

--simplest eigen
callEigen :: (Term -> Goal) -> Goal
callEigen f = \(s, c) -> f (EigenVar c) (s, c+1)

--simplest disequality

--(filter (\st -> walk (Var c) st)) (callFresh f (s, c))

-- 4. run, run-star and so on

getAllSubstitutions :: (Term -> Goal) -> [State]
getAllSubstitutions f = getList $ callFresh f emptyState

getSubstitutionsForGoal :: Goal -> [State]
getSubstitutionsForGoal g = getList $ g emptyState

getAllSolutions :: (Term -> Goal) -> [Term]
getAllSolutions g = fmap (walk (Var 0) . getSubst) $ getAllSubstitutions g

--getNSolutions :: Integer -> (Term -> Goal) -> 
--getFirstSolution :: Term -> Goal -> 

emptyState = ([], 0)


-- Tests

five =  (=== (Atom "5"))
fives_ x = x === (Atom "5") ||| (fives_ x) ||| (sixes_ x)
sixes_ x = x === (Atom "6") ||| (fives_ x)

a_and_b = (callFresh (\a -> a === Atom "7")) &&& (callFresh (\b -> b === Atom "5" ||| b === Atom "6"))

boundForall = callEigen (\a -> a === Atom "7")
unboundForall = callEigen (\a -> callFresh (\b -> b === Pair a a))
boundWithExistential = callFresh (\a -> callEigen (\b -> b === Pair a a))
eigenFailTest =  callFresh (\b -> callEigen (\a -> Pair a a === b))


eigentest1 = null $ getSubstitutionsForGoal $ callFresh (\q -> callEigen (\x -> x === q))
eigentest2 = not $ null $ getSubstitutionsForGoal $ callEigen (\q -> callFresh (\x -> x === q))
eigentest3 = null $ getSubstitutionsForGoal $ callFresh (\q -> callEigen (\x -> (callFresh (\y -> x === y &&& q === y))))
eigentest4 = null $ getSubstitutionsForGoal $ callFresh (\q -> callEigen (\x -> (callFresh (\y -> x === y &&& y === q))))
eigentest5 = null $ getSubstitutionsForGoal $ callFresh (\q -> callEigen (\x -> (callFresh (\y -> Pair x x === y &&& y === q))))
eigentest6 = let f q x = callFresh (\y -> x === y &&& y === q) in null $ getSubstitutionsForGoal $ callFresh (\q -> (callEigen (\x -> f q x)))
--tests 7-13 + eigen-numbero-? + 
-- are related to the constrains: symbolo, numbero, absento, =/=
eigenEqTest1 = null $ getSubstitutionsForGoal $ callEigen (\e -> e === Atom "5")
eigenEqTest2 = null $ getSubstitutionsForGoal $ callEigen (\a -> callEigen (\b -> a === b))
--eigenEqTest3 is the same as 2, and related to varargs in eigen
eigenEqTest3b = null $ getSubstitutionsForGoal $ callEigen (\e1 -> callEigen (\e2 -> callFresh (\x -> callFresh (\y -> x === y &&& x === e1 &&& y === e2))))
eigenEqTest3c = null $ getSubstitutionsForGoal $ callEigen (\e1 -> callEigen (\e2 -> callFresh (\x -> callFresh (\y -> x === e1 &&& x === y &&& y === e2))))
eigenEqTest3d = null $ getSubstitutionsForGoal $ callEigen (\e1 -> callEigen (\e2 -> callFresh (\x -> callFresh (\y -> x === e1 &&& y === e2 &&& x === y))))
eigenEqTest4  = null $ getSubstitutionsForGoal $ callEigen (\e1 -> callFresh (\x -> callFresh (\y -> (e1 === Pair x y))))
eigenSimple = null $ getSubstitutionsForGoal $ callFresh (\q -> callEigen (\x -> q === x))
eigenSimple2 = not $ null $ getSubstitutionsForGoal $ callFresh (\q -> callEigen (\x -> callFresh (\r -> r === x)))
eigenBusted = null $ getSubstitutionsForGoal $ callFresh (\x -> callEigen (\e -> callFresh (\y -> (Pair y (Atom "nil") === x) &&& y === e )))
eigenBusted2 = null $ getSubstitutionsForGoal $ callFresh (\x -> callEigen (\e -> callFresh (\y ->  y === e &&& (Pair y (Atom "nil") === x))))

names = ["eigentest1", "eigentest2", "eigentest3", "eigentest4", "eigentest5", "eigentest6", "eigenEqTest1", "eigenEqTest2", "eigenEqTest3b", "eigenEqTest3c", "eigenEqTest3d", "eigenEqTest4", "eigenSimple", "eigenSimple2", "eigenBusted", "eigenBusted2"]
testsFailed = filter (not . snd) $ zip names $ [eigentest1, eigentest2, eigentest3, eigentest4, eigentest5, eigentest6, eigenEqTest1, eigenEqTest2, eigenEqTest3b, eigenEqTest3c, eigenEqTest3d, eigenEqTest4, eigenSimple, eigenSimple2, eigenBusted, eigenBusted2]

--how to avoid this stuff with different implementation of eigen?
unboundedEigen = callEigen (\x -> fives_ x)

-- bugs related to unification ordering (they want variable on the left for some reason)

-- the problem:

-- this shit works
fives_ok x = x === (Atom "5") ||| (fives_ok x)
-- this shit does not work
fives_fail x = (fives_fail x) ||| x === (Atom "5")
