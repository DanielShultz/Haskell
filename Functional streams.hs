module Stream where

import Control.Arrow
import Control.Applicative

import Stream.Internal

-- Defined in Stream.Internal:
--     data Stream a = a :> Stream a
--     infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (x :> _) = x

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> xs) = xs


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS a = (a :> (repeatS a))

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = (x :> (iterateS f (f x)))

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS xs = f xs xs where
  f a [] = f xs xs
  f a (x:xs) = x :> (f a xs)

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS = iterateS (+ 1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = iterateS (+ s) x

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x (foldrS f xs)

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x:>xs) | p x = x :> (filterS p xs)
                  | otherwise = filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i s | i<=0 = [] 
          | otherwise = headS s : takeS (i-1) (tailS s)

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i s | i<=0 = s
          | otherwise = dropS (i-1) (tailS s)

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = (,) (takeS i s) (dropS i s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = (f x y) :> (zipWithS f xs ys)

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

fstS :: Stream (a, b) -> Stream a
fstS ((x,y) :> xs) = x :> (fstS xs)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> (fmap f xs)

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure = repeatS

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (<*>) (f :> fs) (x :> xs) = f x :> ( (<*>) fs xs)

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = fstS $ iterateS f (0, 1) where
  f (x, y) = (y, x + y)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = filterS (\x -> (f x 2)) (fromS 2) where
  f x i | i==x = True
        | (mod x i) == 0 = False
        | otherwise = f x (i+1)