main :: IO()
main = putStrLn "hello 99 problems!"

-- 01
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 02
myButLast :: [a] -> a
myButLast = last . init

-- 02

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "empty list"
elementAt (_:xs) k
  | k < 1 = error "index too big"
  | otherwise = elementAt xs (k - 1)
