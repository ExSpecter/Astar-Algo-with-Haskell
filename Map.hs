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
    getF,
    getG,
    getH,
    getPos,
    getWeight,
    toString
) where

import Data.Char
import Data.List
import Data.List.Split

type Map = ([Node], Point, Point)
type Point = (Int, Int)

data Node = Node {f::Int, g::Int, h::Int, pos::Point, weight::Int}
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
toNode (c, p)
    | isHexDigit c = Node 0 0 0 p (digitToInt c)
    | otherwise = Node 0 0 0 p 1

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

getF :: Node -> Int
getF n = f n

getG :: Node -> Int
getG n = g n

getH :: Node -> Int
getH n = h n

getPos :: Node -> Point
getPos n = pos n

getWeight :: Node -> Int
getWeight n = weight n

toString :: Map -> [Node] -> String
toString m@(nodes, start, end) path =
    let width = maximum (map (\n -> fst (pos n)) nodes)
        height = maximum (map (\n -> snd (pos n)) nodes)
        ps = map pos path
        positions :: [[Point]]
        positions = [[(x,y) | x <- [0..width]] | y <- [0..height]]
    in
        intercalate "\n" (map (\row -> map (\p -> positionToChar m ps p) row) positions)

positionToChar :: Map -> [Point] -> Point -> Char
positionToChar m path p
    | p `elem` path = '*'
    | otherwise = nodeToChar (nodeAtMaybe m p)

nodeToChar :: Maybe Node -> Char
nodeToChar (Nothing) = '?'
nodeToChar (Just (Wall _)) = '#'
nodeToChar _ = ' '