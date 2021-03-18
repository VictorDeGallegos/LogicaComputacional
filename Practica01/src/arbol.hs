data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show, Eq)

{-
 - 1. Regresa el número de nodos de un árbol.
 - -}
nNodes :: BTree a -> Int
nNodes Void = 0
nNodes (Node x ni nd) = 1 + nNodes ni + nNodes nd

{-
 - 2. Regresa el número de hojas de un árbol.
 - -}
nLeaves :: BTree a -> Int
nLeaves Void = 0
nLeaves (Node x Void Void) = 1
nLeaves (Node x ni nd) = nLeaves ni + nLeaves nd

{-
 - 3. Regresa el número de nodos internos de un árbol
 - -}
nni :: BTree a -> Int
nni Void = 0
nni (Node x Void Void) = 0
nni (Node x ni nd) = 1 + nni ni + nni nd
