-- Inf2d Assignment 1 2019-2020
-- Matriculation number: s1813674
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy, elemIndices, elemIndex)
import ConnectFourWithTwist




{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file when you have finished the assignment.

-- The deadline is the  10th March 2020 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search



-- The Node type defines the position of the agent on the graph.
-- The Branch type synonym defines the branch of search through the graph.
type Node = Int
type Branch = [Node]
type Graph = [Node]

numNodes::Int
numNodes = 4

-- indexing list
js :: [Node]
js = [0..numNodes-1]

bestValMax :: Int
bestValMax = -2

bestValMin :: Int
bestValMin = 2

-- 


-- The next function should return all the possible continuations of input search branch through the graph.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.
next::Branch -> Graph ->  [Branch]
-- Starting with (((take (numNodes)).drop (b*numNodes)) g) : This drops the rows with dont care about and takes the row we are working with
-- js is just the indexing bit that we zip with our rows to get tuples of the form (column, value)
-- we then check if the value is greater than 0 and return the index and the branch
next [] g = []
next branch [] = []
next branch g = [y:[b]| b <- branch, (y,z) <- zip js (((take (numNodes)).drop (b*numNodes)) g), z > 0]
    

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode

explored::Node-> [Node] ->Bool
explored point exploredList = elem point exploredList

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch:: Graph -> Node->(Branch ->Graph -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch g destination next [] exploredList = Nothing
breadthFirstSearch [] destination next branches exploredList = Nothing
breadthFirstSearch g destination next (branch:branches) exploredList
    |checkArrival destination (head branch) = Just branch 
    |explored (head branch) exploredList = breadthFirstSearch g destination next branches exploredList 
    |otherwise = breadthFirstSearch g destination next (branches ++ backtrack) (exploredList ++ [head branch]) -- Does a breadth first search on the new search agenda with the previous node added to the explored list
        where backtrack = [x ++ (drop 1 branch)| x <- next [head branch] g] -- This adds the full branch to the search agenda, that is, it uses "next" to find the next nodes
                                                                            -- to expand but it also adds to it the previous nodes (drop 1 branch) so that we can get the full branch when we return it.
                                                                            -- The reason why we drop 1 node is to avoid a duplicate of the parent node.
                                                                            -- This is put at the end of the search agenda as we first expand shallowest nodes in a breadth first search.
   

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int->[Node]-> Maybe Branch
depthLimitedSearch g destination next [] d exploredList = Nothing
depthLimitedSearch [] destination next branches d exploredList = Nothing
depthLimitedSearch g destination next (branch:branches)  d exploredList
    | d == (-1) = Nothing -- stops the loop at -1 becaue we index the levels starting at 0 instead of 1.
    | checkArrival destination (head branch) = Just branch
    | explored (head branch) exploredList = depthLimitedSearch g destination next branches d exploredList
    | otherwise = depthLimitedSearch g destination next (backtrack ++ branches) (d-1) (exploredList ++ [head branch]) -- Here we swap the order in which we put the new branches and put them at the front as depth limited expands deepest unexpanded node
        where backtrack = [x ++ (drop 1 branch)| x <- next [head branch] g] -- same as breadth first search



-- | Section 4: Informed search


-- | AStar Helper Functions

-- | The cost function calculates the current cost of a trace. The cost for a single transition is given in the adjacency matrix.
-- The cost of a whole trace is the sum of all relevant transition costs.
cost :: Graph ->Branch  -> Int
-- I ziped the branch with itself minus the first element to be able to create tuples where the first element
-- is the starting node and the second element the node to go to.
-- This also allows to get the row and column value of the adjacency matrix (graph) we need to check (gr!!(row*numNodes + col))
-- Finally the sum allows to have the cost of the whole trace.
cost gr branch = sum [gr!!(row*numNodes + col)| (row,col) <- zip branch (drop 1 branch)]


    
-- | The getHr function reads the heuristic for a node from a given heuristic table.
-- The heuristic table gives the heuristic (in this case straight line distance) and has one entry per node. It is ordered by node (e.g. the heuristic for node 0 can be found at index 0 ..)  
getHr:: [Int]->Node->Int
getHr hrTable node = hrTable!!node 


-- | A* Search
-- The aStarSearch function uses the checkArrival function to check whether a node is a destination position,
---- and a combination of the cost and heuristic functions to determine the order in which nodes are searched.
---- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

aStarSearch::Graph->Node->(Branch->Graph -> [Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch g destination next getHr hrTable cost (branch:branches) exploredList = undefined
--    |checkArrival destination (head branch) = Just branch
--    |explored (head branch) exploredList = aStarSearch g destination next getHr hrTable cost branches exploredList
--    |otherwise = aStarSearch g destination next getHr hrTable cost (bestpath ++ branches) (exploredList ++ [head branch])
--        where 
 --           bestpath = [y| x <- next [head branch] g, (y,z) <- zip x totalHeuristic, z == minimum totalHeuristic]
  --          totalHeuristic = [(cost g x) + getHr hrTable (head x)]

-- | Section 5: Games
-- See ConnectFourWithTwist.hs for more detail on  functions that might be helpful for your implementation. 



-- | Section 5.1 Connect Four with a Twist

 

-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game -> Int
eval game 
    | (checkWin game 1) = -1
    | (checkWin game 0) = 1
    | otherwise = 0

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 
alphabeta:: Role -> Game -> Int
alphabeta  player (g:game)
    | (terminal game) = eval game
    | (player == 1) =  maxValue g 1 (-2) 2 
    | otherwise =  minValue g 0 (-2) 2 


-- | OPTIONAL!
-- You can try implementing this as a test for yourself or if you find alphabeta pruning too hard.
-- If you implement minimax instead of alphabeta, the maximum points you can get is 10% instead of 15%.
-- Note, we will only grade this function IF YOUR ALPHABETA FUNCTION IS EMPTY.
-- The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.
minimax:: Role -> Game -> Int
minimax player game=undefined
{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}

maxValue :: Int -> Int -> Int -> Int -> Int
maxValue node role alpha beta 
  | beta <= alpha  = -2
  | otherwise = bestValMax
    where 
        value = maxValue node 1 alpha beta
        bestValMax = max bestValMax value
        alpha = max alpha bestValMax

minValue :: Int -> Int -> Int -> Int -> Int
minValue node role alpha beta 
  | beta <= alpha  = 2
  | otherwise = bestValMin
    where 
        value = minValue node 0 alpha beta
        bestValMin = min bestValMin value
        alpha = min alpha bestValMin


