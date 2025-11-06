main :: IO ()
main = do
    putStrLn "hello"
    print $ truthTable 4
    print $ map op1 $ truthTable 4
    mapM_ print $ truthTable 4
    mapM_ print $ map (\x -> x ++ (op2 x)) $ truthTable 4


truthTable :: Int -> [[Int]]
truthTable 1 = [[0],[1]]
truthTable n = [ s : b | s <- [0,1], b <- truthTable (n-1) ]

op1 :: [Int] -> Int
op1 l
 | b == 3 = b
 | elem b [0..2] = b+1
 | otherwise = -1
     where
         a = l!!0*2 + l!!1
         b = l!!2*2 + l!!3

ex2 = mapM_ print $ map op2 $ truthTable 4

op2 :: [Int] -> [Int]
op2 input = i2 : i1 : i3 : m : i4 : i5 : i6 : s : [] where
    a = input!!0
    b = input!!1
    c = input!!2
    d = input!!3

    i2
     | a + d < 2 = 0
     | otherwise = 1

    i1 = mod (a + d) 2

    i3
     | i2 == 0 = c
     | i2 == 1 = b

    m = i1*(2^1) + i3*(2^0)

    mMap
     | m == 0 = 0b10
     | m == 1 = 0b00
     | m == 2 = 0b11
     | m == 3 = 0b01

    i4 = mod mMap 2

    i5 = div mMap 2

    i6 = lAnd i1 i4

    s = fromEnum $ lXor' i6 i5

lAnd :: Int -> Int -> Int
lAnd a b = fromEnum $ (>0) $ a + b

lXor :: Int -> Int -> Int
lXor a b = fromEnum $ lXor' a b

lXor' :: Int -> Int -> Bool
lXor' a b = (==1) $ a + b
