
module Arbol where
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

{-
 - 4. Nos dice si un elemento está contenido en un árbol ordenado.
 - -}
contains :: (Ord a, Eq a) => a -> BTree a -> Bool
contains x Void = False
contains x (Node a izquierda derecha)
  | x == a = True
  | x < a = contains x izquierda
  | x > a = contains x derecha

  {-
   - 5. Recorrido inorder.
   - -}
inorder :: BTree a -> [ a ]
inorder Void = []
inorder (Node x izquierda derecha) = inorder izquierda ++ (x : inorder derecha)

  {-
   - 6. Recorrido preorder.
   - -}
preorder :: BTree a -> [a]
preorder  Void      = []
preorder (Node x izquierda derecha) = x : (preorder izquierda ++ preorder derecha)

   {-
    - 7. Recorrido postorder.
    - -}
postorder :: BTree a -> [a]
postorder Void     = []
postorder (Node x izquierda derecha) = postorder izquierda ++ postorder derecha ++ [x]

  {-
  -8. Agrega un elemento a un Arbol binario de manera ordenada
  -}
