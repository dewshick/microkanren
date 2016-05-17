module CustomQueue where

data Queue a = Queue [a] [a] deriving Show
emptyQueue = Queue [] []

enqueue x (Queue input output) = Queue (x:input) output
enqueueAll xs queue = foldl (flip enqueue) queue xs

dequeue (Queue [] []) = Nothing
dequeue (Queue input []) = dequeue (Queue [] (reverse input))
dequeue (Queue input (x:output)) = Just (x, Queue input output)

data ITree = Node Int [ITree] | Leaf deriving Show

bfsTree t = bfs' (enqueue t emptyQueue)
bfs' queue = case dequeue queue of
  Nothing -> []
  Just (tree, queue') -> case tree of
    Leaf            -> bfs' queue'
    Node i children -> i:(bfs' $ enqueueAll children queue')

fullTree n | n <= 0    = Leaf
           | otherwise = Node n $ map fullTree [0..n-1]

--todo: implement lazy queue usable for infinite data structures

--failure = fst $ dequeue $ foldr enqueue emptyQueue [1..]

-- previous queue was not lazy!
--data NumList a = NumList Int [a]
--emptyNL = NumList 0 

----data Queue a = Queue [a] [a] deriving Show

----emptyQueue = Queue [] []
----enqueue x (Queue input output) = Queue (x:input) output
----dequeue (Queue [] []) = undefined
----dequeue (Queue input []) = dequeue (Queue [] (reverse input))
----dequeue (Queue input (x:output)) = (x, Queue input output)
