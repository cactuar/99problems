main :: IO()
main = putStrLn "99 problems!"

-- 01
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 02
myButLast :: [a] -> a
myButLast = last . init

-- 03
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "empty list"
elementAt (_:xs) k
  | k < 1 = error "index too big"
  | otherwise = elementAt xs (k - 1)

-- 04
myLength :: [a] -> Int
myLength = foldl (\acc x -> acc +1) 0

-- 05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 06
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == myReverse x

-- 07
data NestedList a = Elem a | List [NestedList a]
myflatten :: NestedList a -> [a]
myflatten (Elem x) = [x]
myflatten (List x) = concatMap myflatten x

-- 08
--compress :: [a] -> [a]
compress [] = []
compress (y:ys) = foldl (\a x -> if x == last a then a else a ++ [x]) [y] ys
