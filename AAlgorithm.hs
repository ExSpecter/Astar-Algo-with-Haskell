module AAlgorithm (
  startAlgorithm
) where
import Map
import Data.List

startAlgorithm :: Map -> [Node] -> [Node]
startAlgorithm allNodes [] = []
startAlgorithm allNodes openList =
  let minNode = searchMinNode openList
  in if (getPos minNode) == (getTarget allNodes) then [minNode]
    else
      let newNodes = calcNodeCosts (getSurrNodes (getPos minNode))
          newOpenList = delete minNode openList
      in startAlgorithm allNodes newOpenList



searchMinNode :: [Node] -> Node
searchMinNode (first:rest) =  let   nextNode = searchMinNode rest
                              in    if (getF first) > (getF nextNode)
                                      then first
                                      else nextNode

surrPositions = [(1,0), (-1,0), (0,1), (0,-1)]

getSurrNodes :: Map -> Point -> [Node]
getSurrNodes allNodes (x,y) = [nodeAt allNodes (x + dx, y + dy) | (dx, dy) <- surrPositions]

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
