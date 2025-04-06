module OneToTen (
    myLast, myButLast, elementAt, myLength, 
    myReverse, myReverse', isPalindrome, 
    myFlatten, compress, compress', compress'',
    pack, encode
) where

import Data.List

-- Problem 1
myLast :: [a] -> a
myLast = foldr1 (const id)


-- Problem 2
myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs


-- Problem 3
elementAt :: Int -> [a] -> a
elementAt _ [] = error "elementAt: Empty List"
elementAt 1 (x:_) = x
elementAt n (_:xs)
    | n < 1 = error "elementAt: Index out of bounds"
    | otherwise = elementAt (n-1) xs


--Problem 4
myLength :: [a] -> Int
myLength = foldr (\x -> (+) 1) 0


--Problem 5
myReverse :: [a] -> [a]
myReverse list = myRev list []
    where myRev [] a = a
          myRev (x:xs) a = myRev xs (x:a)

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

{- myReverse'' :: [a] -> [a]
myReverse'' list = (foldr construct (\acc -> acc) list) []
    where construct x r = \acc -> r ((flip (:)) acc x) -}


-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == myReverse list


-- Problem 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten a = flt' a []
    where flt' (Elem x) xs = x:xs  -- f to be applied in foldr
          flt' (List (x:ls)) xs = flt' x (flt' (List ls) xs) -- Same as foldr
          flt' (List []) xs = xs --  Edge case of foldr


-- Problem 8
compress :: (Eq a) => [a] -> [a]  -- Inefficient: tail to head evaluation
compress [] = []
compress xs = foldr f [] xs
    where f x [] = [x]
          f x y
             | x == (head y) = y
             | otherwise = (x:y)

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : (compress' $ dropWhile (==x) xs)

compress'' :: (Eq a) => [a] -> [a] --  Reverse of compress: head to tail evaluation
compress'' (x:ys@(y:_))
    | x == y = compress'' ys
    | otherwise = x : compress'' ys
compress'' ys = ys


-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack l@(x : xs) = (takeWhile (== x) l) : (pack $ dropWhile (== x) l)

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode l = map (\x -> (length x, head x)) (pack l)