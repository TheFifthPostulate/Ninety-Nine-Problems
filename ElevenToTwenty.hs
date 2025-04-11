module ElevenToTwenty (
    encodeModified, decodeModified, encodeDirect, dupli,
    repli, dropEvery
) where

import OneToTen(encode, pack)


-- Problem 11
data Encoding a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified = map encodeHelper . encode
    where 
        encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

-- Problem 12
decodeModified :: [Encoding a] -> [a]
decodeModified = foldr f []
    where 
        f (Single a) r = a : r
        f (Multiple 1 a) r = a : r
        f (Multiple n a) r = a : (f (Multiple (n-1) a) r)

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) = (assemble x (counter xs)) : (encodeDirect $ dropWhile (== x) xs)
    where
        counter [] = 1
        counter [e] | e == x = 2
        counter (e:es) 
            | e /= x = 1
            | otherwise = 1 + counter es
        assemble b 1 = Single b
        assemble b n = Multiple n b

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli l n = concatMap (f n) l
    where 
        f 1 x = [x]
        f n x = x : f (n-1) x

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery _ n | n < 2 = []
dropEvery l n = 
   let filterUnit :: Int -> [Bool]
       filterUnit i
         | (i<2) = [False]
         | otherwise = True : (filterUnit (i-1))
   in map (\(a,_) -> a) (filter (\(x,b) -> b==True) (zip l (cycle $ filterUnit n)))
