main :: IO ()
main = do
    print $ havelHakimi [4, 4, 2, 2, 1]

cartesian :: [a] -> [b] -> [(a,b)]
cartesian ai bi = concat [[(a, b) | b <- bi] | a <- ai]

-- N_1
naturalNumbers :: [Int]
naturalNumbers = iterate (+1) 1

-- random algorithm from an exercise
ex1 :: [(Int, Int)] -> [(Int, Int)]
ex1 i = filter (\a -> fst a <= snd a) i

-- Representation of V(n, r) as code
v :: Int -> Int -> Int
v _ 0 = 1
v n r = n * v (n-1) (r-1)

-- following the example
optimizedPow :: Int -> Int -> Int
optimizedPow n 0 = 1
optimizedPow n 1 = n
optimizedPow n p = case mod p 2 of
  0 -> optimizedPow n (div p 2) * optimizedPow n (div p 2)
  m -> (optimizedPow n (p-m)) * (optimizedPow n m)


havelHakimi :: [Int] -> Bool
havelHakimi l
 | not (maxS <= length l + (-1)) = False
 | otherwise = ((==0)
    . last . last
    . take (length l)
    . iterate (\x -> applySub (tail x) (head x))) l

    where
        applySub :: [Int] -> Int -> [Int]
        applySub x n = map naturalSub (take n x) ++ drop n x

        naturalSub :: Int -> Int
        naturalSub 0 = 0
        naturalSub x = x - 1

        maxS :: Int
        maxS = foldl (max) 0 l
