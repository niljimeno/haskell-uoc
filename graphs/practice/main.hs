import Data.List

main :: IO ()
main = do
    print $ havelHakimi [2, 2, 4, 3, 3, 2, 3, 5]
    print $ havelHakimi [2, 2, 4, 3, 3, 2, 3, 5, 1]

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
havelHakimi s
 | not (maxS <= length s + (-1)) = False
 | otherwise = ((==0)
    . last . last
    . take (length s)
    . iterate ( sort'
        . (\x -> applySub (tail x) (head x))
    )) (sort' s)

    where
        applySub x n = map (+(-1)) (take n x) ++ drop n x
        sort' = sortBy (flip compare)
        maxS = foldl max 0 s
