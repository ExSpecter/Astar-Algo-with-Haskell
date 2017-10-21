module Map (
    Node(Node),
    Map,
    parse
) where

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
        end = head [pos | (c, pos) <- rawNodes, c == 'x']
    in
        (map toNode rawNodes, start, end)

toNode :: (Char, Point) -> Node
toNode ('#', p) = Wall p
toNode (c, p) = Node 0 0 0 p