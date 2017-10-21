module Map (
    Node(Node, Wall),
    Map,
    Point,
    parse,
    getNodes,
    getStart,
    getTarget,
    nodeAt,
    nodeAtMaybe,
    getPos,
    getF,
    getG,
    getH
) where

import Data.List
import Data.List.Split

type Map = ([Node], Point, Point)
type Point = (Integer, Integer)

data Node = Node {f::Integer, g::Integer, h::Integer, pos::Point}
          | Wall {pos::Point}
    deriving(Eq, Show)

parse :: String -> Map
parse s =
    let
        rows = splitOn "\n" s
        rawNodes = concat [map (\(x, a) -> (a, (x, y))) (zip [0..] row) | (y, row) <- zip [0..] rows]
        start = head [pos | (c, pos) <- rawNodes, c == 's']
        target = head [pos | (c, pos) <- rawNodes, c == 'x']
    in
        (map toNode rawNodes, start, target)

toNode :: (Char, Point) -> Node
toNode ('#', p) = Wall p
toNode (c, p) = Node 0 0 0 p

getNodes :: Map -> [Node]
getNodes (nodes, _, _) = nodes

getStart :: Map -> Point
getStart (_, p, _) = p

getTarget :: Map -> Point
getTarget (_, _, p) = p

nodeAt :: Map -> Point -> Node
nodeAt (nodes, _, _) p = head [n | n <- nodes, pos n == p]

nodeAtMaybe :: Map -> Point -> Maybe Node
nodeAtMaybe (nodes, _, _) p =
  let nodeList = [n | n <- nodes, pos n == p]
  in if (length nodeList) == 0 then Nothing else Just (head nodeList)

getF :: Node -> Integer
getF n = f n

getG :: Node -> Integer
getG n = g n

getH :: Node -> Integer
getH n = h n

getPos :: Node -> Point
getPos n = pos n
