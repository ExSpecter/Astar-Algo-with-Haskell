module AAlgorithm (
  startAlgorithm
) where
import Map
import Data.List
import Data.Maybe

startAlgorithm :: Map -> [Node]
startAlgorithm m =
  let startNode = [(nodeAt m (getStart m))]
  --in (algorithm (deleteFromMap m startNode) startNode)
  in getShortPath ((algorithm (deleteFromMap m startNode) startNode), (getStart m), (getTarget m)) (getTarget m)

getShortPath :: Map -> Point -> [Node]
getShortPath closedNodes currentNode =
  let surrNodes = getSurrNodes closedNodes currentNode
  in  if (length surrNodes) == 0 then [(nodeAt closedNodes currentNode)]
    else let nextNode = searchMinNodeBack surrNodes
          in  if  (getPos nextNode) == (getStart closedNodes) then [nextNode]
              else (nextNode:(getShortPath closedNodes (getPos nextNode)))

algorithm :: Map -> [Node] -> [Node]
algorithm allNodes [] = []
algorithm allNodes openList =
  let minNode = searchMinNode openList
  in if (getPos minNode) == (getTarget allNodes) then [minNode]
    else
      let newNodes = calcNodeCosts allNodes minNode (removeWall (getSurrNodes allNodes (getPos minNode)))
          newOpenList = newNodes ++ (delete minNode openList)
      in (minNode:(algorithm (deleteFromMap allNodes newOpenList) newOpenList))

deleteFromMap :: Map -> [Node] -> Map
deleteFromMap mapt [] = mapt
deleteFromMap mapt@(nodes, start, target) (x:xs) =
  let newMap@(nodesx, startx, targetx) = deleteFromMap mapt xs
  in ([nodez | nodez <- nodesx, not ((getPos nodez) == (getPos x))], start, target)

searchMinNodeBack :: [Node] -> Node
searchMinNodeBack [x] = x
searchMinNodeBack (first:rest) =
  let   nextNode = searchMinNodeBack rest
  in    if (getG first) < (getG nextNode)
          then first
          else nextNode

searchMinNode :: [Node] -> Node
searchMinNode [x] = x
searchMinNode (first:rest) =  let   nextNode = searchMinNode rest
                              in    if (getF first) < (getF nextNode)
                                      then first
                                      else nextNode

surrPositions = [(1,0), (-1,0), (0,1), (0,-1)]

getSurrNodes :: Map -> Point -> [Node]
getSurrNodes allNodes (x,y) = (mapMaybe (nodeAtMaybe allNodes) [(x + dx, y + dy) | (dx, dy) <- surrPositions])

removeWall :: [Node] -> [Node]
removeWall [] = []
removeWall ((Wall _):rest) = removeWall rest
removeWall (a:rest) = (a:(removeWall rest))
--removeWall (a@(Node f _ _ _ _):rest) = if f == 0 then (a:(removeWall rest)) else removeWall rest

calcNodeCosts :: Map -> Node ->[Node] -> [Node]
calcNodeCosts allNodes currentNode [] = []
calcNodeCosts allNodes currentNode (x:xs) =
  ((calcNodeCostsHelper allNodes currentNode (getPos x)) : (calcNodeCosts allNodes currentNode xs))

calcNodeCostsHelper :: Map -> Node -> Point -> Node
calcNodeCostsHelper allNodes currentNode point =
  let weight = getWeight currentNode
      g = (getG currentNode) + weight
      h = calcDistance point (getTarget allNodes)
      f = g + h
  in Node f g h point weight

calcDistance :: Point -> Point -> Int
calcDistance (x,y) (xs, ys) = abs (x - xs) + abs (y - ys)
