data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show, Eq)

{-
 -
 - -}
nNodes :: BTree a -> Int
nNodes Void = 0
nNodes (Node x ni nd) = 1 + nNodes ni + nNodes nd

{-
 - 
 - -}
nLeaves :: BTree a -> Int
nLeaves Void = 0
nLeaves (Node x Void Void) = 1
nLeaves (Node x ni nd) = nLeaves ni + nLeaves nd

{-
 -
 - -}
nni :: BTree a -> Int
nni Void = 0
nni (Node x Void Void) = 0
nni (Node x ni nd) = 1 + nni ni + nni nd
