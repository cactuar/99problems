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
compress :: Eq a => [a] -> [a]
compress (y:ys) = foldl (\a x -> if x == last a then a else a ++ [x]) [y] ys

-- compress' (y:ys) = foldr (\a x -> if x == head a then a else x:a) [y] ys

-- 09
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (y:ys) = foldl (\a x -> if x /= head(last a)
                             then a ++ [[x]]
                             else let i = init a
                                      l = last a in i ++ [l ++ [x]])
              [[y]] ys

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode y = foldr (\x a -> (length x, head x):a) [] $ pack y
