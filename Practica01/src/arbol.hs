
module Arbol where
data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show, Eq)

{-
 - 1. Regresa el número de nodos de un árbol.
 - -}
nNodes :: BTree a -> Int
nNodes Void           = 0
nNodes (Node x ni nd) = 1 + nNodes ni + nNodes nd

{-
 - 2. Regresa el número de hojas de un árbol.
 - -}
nLeaves :: BTree a -> Int
nLeaves Void               = 0
nLeaves (Node x Void Void) = 1
nLeaves (Node x ni nd)     = nLeaves ni + nLeaves nd

{-
 - 3. Regresa el número de nodos internos de un árbol
 - -}
nni :: BTree a -> Int
nni Void               = 0
nni (Node x Void Void) = 0
nni (Node x ni nd)     = 1 + nni ni + nni nd

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
add :: (Ord a) => a -> BTree a -> BTree a
add x Void =   Node x Void Void
add x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (add x left) right
    | x > a  = Node a left (add x right)
{-
- ATENCION myfilter ES UNA FUNCION AUXILIAR
- myfilter es la version recursiva de filter
- myfilter p xs es la lista de los elementos de xs que cumplen la propiedad p.
-}
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ []= []
myfilter p (x:xs) | p x = x : myfilter p xs
                  | otherwise = myfilter p xs

{-
- 9.Pasa una lista a un árbol binario de forma ordenada
- La lista (x:xs) es convertida en arbol y la raiz es x.
- En el caso de ser vacia la lista, el arbol es Void, en otras palabras vacio.
-
- En el caso de no ser vacio:
- El sub-árbol de la izquierda es un árbol creado de la siguiente manera:
- xs tienen que ser menores a x.
- El sub-árbol de la derecha es el árbol esta formado por los elementos
- mayores a x.
-}
fromList :: ( Ord a ) => [ a ] -> BTree a
fromList [] = Void
fromList (x:xs) = Node x (fromList (myfilter (<x) xs))
                         (fromList (myfilter (>x) xs))
