module AAlgorithm (
  startAlgorithm
) where
import Map
import Data.List
import Data.Maybe

startAlgorithm :: Map -> [Node]
startAlgorithm m =
  let startNode = [(nodeAt m (getStart m))]
  in algorithm (deleteFromMap m startNode) startNode

algorithm :: Map -> [Node] -> [Node]
algorithm allNodes [] = []
algorithm allNodes openList =
  let minNode = searchMinNode openList
  in if (getPos minNode) == (getTarget allNodes) then [minNode]
    else
      let newNodes = calcNodeCosts allNodes (getSurrNodes allNodes (getPos minNode))
          newOpenList = newNodes ++ (delete minNode openList)
      in (minNode:(algorithm (deleteFromMap allNodes newOpenList) newOpenList))

deleteFromMap :: Map -> [Node] -> Map
deleteFromMap mapt [] = mapt
deleteFromMap mapt@(nodes, start, target) (x:xs) =
  let newMap = deleteFromMap mapt xs
  in ([nodez | nodez <- nodes, not ((getPos nodez) == (getPos x))], start, target)

searchMinNode :: [Node] -> Node
searchMinNode [x] = x
searchMinNode (first:rest) =  let   nextNode = searchMinNode rest
                              in    if (getF first) > (getF nextNode)
                                      then first
                                      else nextNode

surrPositions = [(1,0), (-1,0), (0,1), (0,-1)]

getSurrNodes :: Map -> Point -> [Node]
getSurrNodes allNodes (x,y) = removeWall (mapMaybe (nodeAtMaybe allNodes) [(x + dx, y + dy) | (dx, dy) <- surrPositions])

removeWall :: [Node] -> [Node]
removeWall [] = []
removeWall ((Wall _):rest) = removeWall rest
removeWall (a@(Node f _ _ _):rest) = if f == 0 then (a:(removeWall rest)) else removeWall rest

calcNodeCosts :: Map -> [Node] -> [Node]
calcNodeCosts allNodes [] = []
calcNodeCosts allNodes (x:xs) =
  ((calcNodeCostsHelper allNodes (getPos x)) : (calcNodeCosts allNodes xs))

calcNodeCostsHelper :: Map -> Point -> Node
calcNodeCostsHelper allNodes point =
  let g = calcDistance point (getStart allNodes)
      h = calcDistance point (getTarget allNodes)
      f = g + h
  in Node f g h point

calcDistance :: Point -> Point -> Integer
calcDistance (x,y) (xs, ys) = abs (x - xs) + abs (y - ys)
