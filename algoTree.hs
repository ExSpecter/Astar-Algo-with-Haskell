module AlgoTree(
  Node(Node),
  Tree(Leaf, Node)
) where

data Node a = Node {f::Integer, g::Integer, h::Integer, pos::a}

data Tree = Leaf Node | Node [Node]
