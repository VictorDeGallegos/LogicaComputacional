module Lista where
data List a = Void | Cons a (List a) deriving (Show)


{-
 - 1. Función que regresa tal vez la cabeza de la lista.
 - -}
myHead :: List a -> Maybe a
myHead Void = Nothing
myHead (Cons e l) = Just e

{-
 - 2. Función que regresa tal vez la cola de la lista.
 - -}
myTail :: List a -> Maybe (List a)
myTail Void = Nothing
myTail (Cons e l) = Just l

{-
 - 3. Función que regresa tal vez el último elemento de la lista.
 - -}
myLast :: List a -> Maybe a
myLast Void = Nothing
myLast (Cons e Void) = Just e
myLast (Cons e l) = myLast l

{-
 - 4. Función que regresa la longitud de la lista.
 - -}
myLen :: List a -> Int
myLen Void = 0
myLen (Cons e l) = 1 + myLen l

{-
 - 5. Función que nos dice si un elemento está en una lista.
 - -}
isElem :: (Eq a) => List a -> a -> Bool
isElem Void _ = False
isElem (Cons e l) x = if e == x then True else isElem l x

myConcat :: List a -> List a -> List a
myConcat Void l = l
myConcat (Cons e l) l2 = Cons e (myConcat l l2)

{-
 - 6. Función que regresa la reversa de una lista.
 - -}
myReverse :: List a -> List a
myReverse Void = Void
myReverse (Cons e l) = myConcat (myReverse l) (Cons e Void)

{-
 - 7.  Función que pasa una de nuestras listas a las listas de haskell
 - -}
toHaskell :: List a -> [a]
toHaskell Void = []
toHaskell (Cons e l) = e:(toHaskell l)

{-
 - 8. Función que pasa una lista de haskell a nuestras listas.
 - -}
fromHaskell :: [a] -> List a
fromHaskell [] = Void
fromHaskell (x:xs) = (Cons x (fromHaskell xs))
