module AAlgorithm (
  startAlgorithm
) where
import Map
import Data.List
import Data.Maybe

-- starts the algorithm finding the shortest path in the solved map 
startAlgorithm :: Map -> [Node]
startAlgorithm m =
  let startPos = getStart m
      targetPos = getTarget m
      startNode = nodeAt m (getStart m)
      mapWithoutStart = deleteFromMap m [startNode]
      solvedMap = (algorithm mapWithoutStart [startNode], startPos, targetPos)
  in getShortPath solvedMap targetPos

-- finds the shortest path (from target to start)
getShortPath :: Map -> Point -> [Node]
getShortPath closedNodes currentNode =
  if   nodeAtMaybe closedNodes currentNode == Nothing
  then []
  else
    let surrNodes = getSurrNodes closedNodes currentNode
    in if null surrNodes then [nodeAt closedNodes currentNode]
      else let nextNode = searchMinNodeBack surrNodes
            in  if   getPos nextNode == getStart closedNodes
                then (nodeAt closedNodes currentNode):[nextNode]
                else (nodeAt closedNodes currentNode):(getShortPath closedNodes (getPos nextNode))

-- a* algorithm
algorithm :: Map -> [Node] -> [Node]
algorithm allNodes [] = []
algorithm allNodes openList =
  let minNode = searchMinNode openList
  in if   getPos minNode == getTarget allNodes
     then [minNode]
     else
       let newNodes = calcNodeCosts allNodes minNode (removeWall (getSurrNodes allNodes (getPos minNode)))
           newOpenList = newNodes ++ (delete minNode openList)
       in (minNode:(algorithm (deleteFromMap allNodes newOpenList) newOpenList))

-- deletes nodes from map
deleteFromMap :: Map -> [Node] -> Map
deleteFromMap mapt [] = mapt
deleteFromMap mapt@(nodes, start, target) (x:xs) =
  let newMap@(nodesx, startx, targetx) = deleteFromMap mapt xs
  in ([nodez | nodez <- nodesx, not (getPos nodez == getPos x)], start, target)

-- search min node on the way back
searchMinNodeBack :: [Node] -> Node
searchMinNodeBack [x] = x
searchMinNodeBack (first:rest) =
  let nextNode = searchMinNodeBack rest
  in  if   (getG first) < (getG nextNode)
      then first
      else nextNode

-- search min node
searchMinNode :: [Node] -> Node
searchMinNode [x] = x
searchMinNode (first:rest) =  
  let nextNode = searchMinNode rest
  in  if   (getF first) < (getF nextNode)
      then first
      else nextNode

surrPositions = [(1,0), (-1,0), (0,1), (0,-1)]

-- helper to get all nodes surrounding a position on the map
getSurrNodes :: Map -> Point -> [Node]
getSurrNodes allNodes (x,y) = (mapMaybe (nodeAtMaybe allNodes) [(x + dx, y + dy) | (dx, dy) <- surrPositions])

-- removes all walls from a node list
removeWall :: [Node] -> [Node]
removeWall [] = []
removeWall ((Wall _):rest) = removeWall rest
removeWall (a:rest) = (a:(removeWall rest))

-- calculate node costs for all nodes
calcNodeCosts :: Map -> Node -> [Node] -> [Node]
calcNodeCosts allNodes currentNode [] = []
calcNodeCosts allNodes currentNode (x:xs) =
  ((calcNodeCostsHelper allNodes currentNode (getPos x)) : (calcNodeCosts allNodes currentNode xs))

-- calculates costs of a single node
calcNodeCostsHelper :: Map -> Node -> Point -> Node
calcNodeCostsHelper allNodes currentNode point =
  let weight = getWeight (nodeAt allNodes point)
      g = (getG currentNode) + weight
      h = calcDistance point (getTarget allNodes)
      f = g + h
  in Node f g h point weight

-- calculates the (manhattan) distance between to points
calcDistance :: Point -> Point -> Int
calcDistance (x,y) (xs, ys) = abs (x - xs) + abs (y - ys)
