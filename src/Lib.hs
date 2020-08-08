module Lib where

-- Custom types
data NestedList a = Elem a | List [NestedList a]

data Elem a = Multiple Int a | Single a deriving (Eq, Show)

-- Problem 01
myLast :: [a] -> Maybe a
myLast []       = Nothing
myLast [x     ] = Just x
myLast (_ : xs) = myLast xs

-- Problem 02
myButLast :: [a] -> Maybe a
myButLast []       = Nothing
myButLast (x : xs) = case length xs of
  0 -> Nothing
  1 -> Just x
  _ -> myButLast xs

-- Problem 03
elementAt :: [a] -> Int -> Maybe a
elementAt xs n = if n <= 0 || n > length xs
  then Nothing
  else if n == 1 then Just $ head xs else elementAt (tail xs) $ n - 1

-- Problem 04
myLength :: [a] -> Int
myLength []       = 0
myLength (x : xs) = 1 + myLength xs

-- Problem 05
myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 06
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- Problem 07
flatten :: NestedList a -> [a]
flatten (Elem x ) = [x]
flatten (List xs) = concatMap flatten xs

-- Problem 08
compress :: Eq a => [a] -> [a]
compress []       = []
compress [x     ] = [x]
compress (x : xs) = if x == head y then y else [x] ++ y where y = compress xs

-- Problem 09
pack :: Eq a => [a] -> [[a]]
pack []       = []
pack [x     ] = [[x]]
pack (x : xs) = if x == head (head y)
  then [head y ++ [x]] ++ tail y
  else [[x]] ++ y
  where y = pack xs

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode []       = []
encode [x     ] = [(1, x)]
encode (x : xs) = if x == h then [(hc + 1, h)] ++ t else [(1, x)] ++ y
 where
  y       = encode xs
  (hc, h) = head y
  t       = tail y

-- Problem 11
tupleToElem :: (Int, a) -> Elem a
tupleToElem (1, x) = Single x
tupleToElem (n, x) = Multiple n x

encodeModified :: Eq a => [a] -> [Elem a]
encodeModified = (map tupleToElem) . encode

-- Problem 12
elemToTuple :: Elem a -> (Int, a)
elemToTuple (Single x    ) = (1, x)
elemToTuple (Multiple n x) = (n, x)

tupleToList :: (Int, a) -> [a]
tupleToList (0, x) = []
tupleToList (n, x) = [x] ++ tupleToList (n - 1, x)

decodeModified :: [Elem a] -> [a]
decodeModified xs = concatMap (tupleToList . elemToTuple) xs

-- Problem 13
encodeDirect :: Eq a => [a] -> [Elem a]
encodeDirect []       = []
encodeDirect [x     ] = [Single x]
encodeDirect (x : xs) = if x == h
  then [Multiple (num + 1) x] ++ t
  else [Single x] ++ y
 where
  y        = encodeDirect xs
  (num, h) = case head y of
    Multiple n0 h0 -> (n0, h0)
    Single h0      -> (1, h0)
  t = tail y

-- Problem 14
dupli :: [a] -> [a]
dupli []       = []
dupli (x : xs) = [x, x] ++ dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli []       _ = []
repli [x     ] n = if n <= 0 then [] else [x] ++ repli [x] (n - 1)
repli (x : xs) n = (repli [x] n) ++ (repli xs n)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n =
  if n <= 0 then [] else (take (n - 1) xs) ++ (dropEvery (drop n xs) n)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split []       _ = ([], [])
split (x : xs) n = if n <= 0 then ([], x : xs) else ([x] ++ h, t)
  where (h, t) = split xs (n - 1)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs l r = drop (l - 1) (take r xs)

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs d =
  let n = length xs
  in  if d < 0 || d >= n
        then rotate xs $ rem ((rem d n) + n) n
        else slice xs (1 + d) n ++ slice xs 1 d

-- Problem 20
removeAt :: [a] -> Int -> (Maybe a, [a])
removeAt xs n = (elementAt xs n, take (n - 1) xs ++ drop n xs)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n - 1) xs ++ [x] ++ drop (n - 1) xs

-- Problem 22
range :: Integer -> Integer -> [Integer]
range l r = if l > r then [] else l : range (l + 1) r
