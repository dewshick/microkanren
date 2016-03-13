import Control.Monad
import Control.Applicative

-- 1. Terms, subscitutions, lookup

type Var = Integer
type Subst = [(Var, Term)]
data Term = Atom String | Pair Term Term | Var Var deriving Show

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

data KList a = Nil | Cons a (KList a) | Delay (KList a)
delay = Delay

instance Functor KList where
  fmap f a = error "FUCK OFF, HASKELL"

instance Applicative KList where
  f <*> a = error "FUCK OFF, HASKELL"
  pure x = Cons x Nil

instance Alternative KList where
  a <|> b = error "you know, right?"
  empty = mzero

instance Monad KList where
  Nil >>= f = mzero
  x `Cons` xs >>= f = f x `mplus` (xs >>= f)
  Delay xs >>= f = Delay $ xs >>= f

instance MonadPlus KList where
  mzero = Nil
  Nil `mplus` xs = xs
  (x `Cons` xs) `mplus` ys = x `Cons` (ys `mplus` xs) -- swapped per sect. 6
  Delay xs `mplus` ys = Delay (ys `mplus` xs)

kToList Nil = []
kToList (Cons x xs) = x:(kToList xs)
kToList (Delay xs) = kToList xs

instance Show a => Show (KList a) where
  show xs = show $ kToList xs

-- 3. MicroKanren operators & progrm-related types

type State = (Subst, Integer)
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

callForall :: (Term -> Goal) -> Goal
callForall f = error "dunno"

-- 4. run, run-star and so on

getAllSolutions :: (Term -> Goal) -> [State]
getAllSolutions f = kToList $ (callFresh f) emptyState

--getNSolutions :: Integer -> (Term -> Goal) -> 
--getFirstSolution :: Term -> Goal -> 

emptyState = ([], 0)
five =  (\x -> x === (Atom "5"))

-- bugs related to unification ordering (they want variable on the left for some reason)



