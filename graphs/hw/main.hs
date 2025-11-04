import Control.Monad

main :: IO ()
main = do
    -- print $ preuDorsals 13794
    print ex2
    ex3

preuDorsals = preuDorsals' 1

preuDorsals' :: Int -> Int -> Int
preuDorsals' digits restants
 | restants < preuT = div restants preu
 | otherwise = q + preuDorsals' (digits+1) (restants-preuT)
    where
        q = 10^digits - 10^(digits-1)
        preu = digits * 2
        preuT = preu * q


ex2 = combinations 5 (take 18 naturalNumbers)

naturalNumbers :: [Int]
naturalNumbers = iterate (+1) 1

combinations :: Int -> [Int] -> Int
combinations n r = moves n offset
    where
        offset = (length r) - lastPosition
        lastPosition = (n * 2) - 1

moves :: Int -> Int -> Int
moves depht offset
 | depht == 1 = 1 -- id
 | otherwise = offset * moves (depht-1) (div (offset + 1) 2)


-----------------------------------------------------------

type Vector = (Int, Int)
type Edge = (Vector, Vector)
vecEq a b = fst a == fst b && snd a == snd b

data Graph = Graph {
    vertices :: [Vector],
    edges :: [Edge]
}

ex3 = printMatrix [[1, 1], [1, 1]]

printMatrix :: [[Int]] -> IO ()
printMatrix i = do
    putStrLn "["
    mapM_ (\x -> do
        putStr " "
        print x) i
    putStrLn "],"
