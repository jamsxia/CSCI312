-- problem2
data IntTree = Empty | Node IntTree Int IntTree deriving (Eq, Show)

isLeaf :: IntTree -> Bool
isLeaf Empty = False 
isLeaf (Node Empty _ Empty) = True  
isLeaf (Node _ _ _) = False 

sumTree :: IntTree -> Int
sumTree Empty = 0  
sumTree (Node left value right) = value + sumTree left + sumTree right 

fringe :: IntTree -> [Int]
fringe Empty = []  
fringe (Node Empty value Empty) = [value]  
fringe (Node left _ right) = fringe left ++ fringe right

-- problem 3
isBST :: IntTree -> Bool
isBST tree = isBSTHelper tree Nothing Nothing
  where
    -- Helper function with valid range (min, max) for each subtree
    isBSTHelper :: IntTree -> Maybe Int -> Maybe Int -> Bool
    isBSTHelper Empty _ _ = True  
    isBSTHelper (Node left value right) minVal maxVal =
      -- Check if the current node's value is within the valid range
      case (minVal, maxVal) of
        (Just minV, Just maxV) -> value > minV && value < maxV
        (Just minV, Nothing) -> value > minV
        (Nothing, Just maxV) -> value < maxV
        (Nothing, Nothing) -> True
      -- Recursively check the left and right subtrees with updated ranges
      && isBSTHelper left minVal (Just value)
      && isBSTHelper right (Just value) maxVal

tree:: IntTree
--tree1:: IntTree
--tree2:: IntTree
tree =  Node (Node Empty 1 (Node Empty 2 Empty)) 
            5 (Node (Node Empty 7 Empty) 10 Empty)
tree1 = Node (Node(Node Empty 2 Empty) 3 (Node Empty 4 Empty))
            5 (Node Empty 7 (Node Empty 8 Empty))
tree2 = Node (Node(Node Empty 8 Empty) 3 (Node Empty 4 Empty)) 5 (Node Empty 6 Empty)

main :: IO()
main = do
    let treeSum = sumTree tree
    putStrLn $ "Sum of all values in the tree: " ++ show treeSum
    let treeFringe = fringe tree
    putStrLn $ "Fringe (leaves from left to right): " ++ show treeFringe
    print $ isBST tree1  -- Output: True
    print $ isBST tree2  -- Output: False