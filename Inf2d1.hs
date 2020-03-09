-- Inf2d Assignment 1 2019-2020
-- Matriculation number: s1813674
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy, elemIndices, elemIndex, sort, group)
import ConnectFourWithTwist
import Data.Function
import Data.Ord
import           Control.Applicative (liftA2)
import           Data.Tuple          (swap)


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


-- 


-- The next function should return all the possible continuations of input search branch through the graph.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.
next::Branch -> Graph ->  [Branch]
-- Starting with (((take (numNodes)).drop (branch*numNodes)) g) : This drops the rows with dont care about and takes the row we are working with
-- js is just the indexing bit that we zip with our rows to get tuples of the form (column, value)
-- we then check if the value is greater than 0 (i.e there is an edge) and return the index (i.e the next node) and the branch
next [] g = []
next branch [] = []
next branch g = [y:branch|(y,z) <- zip [0..num-1] (((take (num)).drop ((branch!!0)*num)) g), z > 0]
    where num = (ceiling . sqrt . fromIntegral . length $ g)
    

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
    |otherwise = breadthFirstSearch g destination next (branches ++ (next branch g)) (exploredList ++ [head branch]) 

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int->[Node]-> Maybe Branch
depthLimitedSearch g destination next [] d exploredList = Nothing
depthLimitedSearch [] destination next branches d exploredList = Nothing
depthLimitedSearch g destination next (branch:branches)  d exploredList
    | checkArrival destination (head branch)= Just branch
    | length branch > d = depthLimitedSearch g destination next branches d exploredList --If our branch is bigger then the depth we look for other solutions
    | otherwise = depthLimitedSearch g destination next (newAgenda ++ branches) d ([head branch] ++ exploredList) -- Here we swap the order in which we put the new branches and put them at the front as depth limited expands deepest unexpanded node
        where newAgenda = [b | b <- next branch g, notElem (head b) (tail b)] -- If there's a loop then we skip the branch. 
                                                                              -- We check for a loop by searching if the head of the branch is already in the branch
                                                                              -- This allows us to not put nodes we might need later in the explored list


-- | Section 4: Informed search


-- | AStar Helper Functions

-- | The cost function calculates the current cost of a trace. The cost for a single transition is given in the adjacency matrix.
-- The cost of a whole trace is the sum of all relevant transition costs.
cost :: Graph ->Branch  -> Int
-- I ziped the branch with itself minus the first element to be able to create tuples where the first element
-- is the starting node and the second element the node to go to.
-- This also allows to get the row and column value of the adjacency matrix (graph) we need to check (gr!!(row*numNodes + col))
-- Finally the sum allows to have the cost of the whole trace.
cost [] branch = 0
cost gr [] = 0
cost gr branch = sum [gr!!(row*num + col)| (row,col) <- zip (tail branch) branch]
    where num = (ceiling . sqrt . fromIntegral . length $ gr)


    
-- | The getHr function reads the heuristic for a node from a given heuristic table.
-- The heuristic table gives the heuristic (in this case straight line distance) and has one entry per node. It is ordered by node (e.g. the heuristic for node 0 can be found at index 0 ..)  
getHr:: [Int]->Node->Int
getHr hrTable node = hrTable!!node 


-- | A* Search
-- The aStarSearch function uses the checkArrival function to check whether a node is a destination position,
---- and a combination of the cost and heuristic functions to determine the order in which nodes are searched.
---- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

aStarSearch::Graph->Node->(Branch->Graph -> [Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch [] destination next getHr hrTable cost branches exploredList = Nothing
aStarSearch g destination next getHr hrTable cost [] exploredList = Nothing
aStarSearch g destination next getHr hrTable cost (branch:branches) exploredList
    |checkArrival destination (head branch) = Just branch
    |explored (head branch) exploredList = aStarSearch g destination next getHr hrTable cost branches exploredList
    |otherwise = aStarSearch g destination next getHr hrTable cost (sortedpaths) (exploredList ++ [head branch])
       where 
            sorted = sortBy (compare `on` snd) [(y,z)| xs <- (next branch g ++ branches), (y,z) <- zip [xs] [(cost g xs) + getHr hrTable (head xs)]] -- The zip gives tuples of the form (branch, cost + heuristic). This function sorts the zip by total cost.
            sortedpaths = [ys|(ys,z) <- sorted] -- This function returns the sorted branches
        

-- | Section 5: Games
-- See ConnectFourWithTwist.hs for more detail on  functions that might be helpful for your implementation. 



-- | Section 5.1 Connect Four with a Twist

 

-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game -> Int
eval game 
    | checkWin game 1 = 1
    | checkWin game 0 = -1
    | otherwise = 0

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state. 

alphabeta:: Role -> Game -> Int
alphabeta  player game
    | terminal game = eval game
    | player == 0 = maxValue game player (-2) 2
    | otherwise = minValue game player (-2) 2



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


maxValue :: Game -> Role -> Int -> Int -> Int
maxValue game player alpha beta  
    | terminal game = eval game
    | otherwise = forLoopMax outcomes player (-2) alpha beta
        where outcomes = (movesAndTurns game player)

minValue :: Game -> Role -> Int -> Int -> Int
minValue game player alpha beta
    | terminal game = eval game
    | otherwise = forLoopMin outcomes player 2 alpha beta
        where outcomes = (movesAndTurns game player)

-- This function does the "for each a in Actions (state)" of the alpha beta pruning algorithm
forLoopMin :: [Game] -> Role -> Int -> Int -> Int -> Int 
forLoopMin [] player v alpha beta = v
forLoopMin (game:games) player v alpha beta 
    | v <= alpha =  newValue
    | otherwise = forLoopMax games player v alpha newBeta
        where 
            newValue = maxValue game (switch player) alpha beta
            newBeta = min beta newValue

forLoopMax :: [Game] -> Role -> Int -> Int -> Int -> Int
forLoopMax [] player v alpha beta = v
forLoopMax (game:games) player v alpha beta 
    | v >= beta = newValue
    | otherwise = forLoopMax games player v newAlpha beta
        where 
            newValue = minValue game (switch player) alpha beta
            newAlpha = max alpha newValue