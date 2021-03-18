data List a = Void | Cons a (List a) deriving (Show)

  
{-
 -
 - -}
myHead :: List a -> Maybe a
myHead Void = Nothing
myHead (Cons e l) = Just e

{-
 -
 - -}
myTail :: List a -> Maybe (List a)
myTail Void = Nothing
myTail (Cons e l) = Just l

{-
 -
 - -}
myLast :: List a -> Maybe a
myLast Void = Nothing
myLast (Cons e Void) = Just e
myLast (Cons e l) = myLast l

{-
 -
 - -}
myLen :: List a -> Int
myLen Void = 0
myLen (Cons e l) = 1 + myLen l
 
{-
 - -}
isElem :: (Eq a) => List a -> a -> Bool
isElem Void _ = False
isElem (Cons e l) x = if e == x then True else isElem l x

myConcat :: List a -> List a -> List a
myConcat Void l = l
myConcat (Cons e l) l2 = Cons e (myConcat l l2)

{-
 - -}
myReverse :: List a -> List a
myReverse Void = Void
myReverse (Cons e l) = myConcat (myReverse l) (Cons e Void)

{-
 - -}
toHaskell :: List a -> [a]
toHaskell Void = []
toHaskell (Cons e l) = e:(toHaskell l)

{-
 -
 - -}
fromHaskell :: [a] -> List a 
fromHaskell [] = Void
fromHaskell (x:xs) = (Cons x (fromHaskell xs))
