-- sortedtree.hs
-- Jeremy.Singer@glasgow.ac.uk
-- Example code for #FLhaskell course

-- Nodes contain integers, Leaves are empty
data Tree = Leaf | Node Int Tree Tree deriving Show



treeDepth :: Tree -> Int
-- longest path from root to a leaf
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)


isSortedTree :: Tree -> Int -> Int -> Bool
-- is the tree sorted in-order?
-- the two Int params indicate min and max
-- for the current subtree
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted



addNewMax :: Tree -> Tree
-- add a new max element to tree
-- will go down rightmost path to Leaf
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree


-- following code by Wes Ellis as practice

treeA = Node 42 (Node 17 (Node 3 Leaf Leaf) (Node 30 Leaf Leaf)) (Node 73 (Node 51 Leaf Leaf) (Node 88 Leaf Leaf))

-- Write a function to insert a value into a Tree in order
insertNode :: Int -> Tree -> Tree
insertNode x Leaf = Node x Leaf Leaf
insertNode x (Node y l r)
  = if x <= y then Node y (insertNode x l) r
    else Node y l (insertNode x r)

-- Write a function to convert from a tree into a list
treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node x Leaf _) = [x]
treeToList (Node x l r) = treeToList l ++ [x] ++ treeToList r
