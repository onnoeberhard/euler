-- Dynamic programming through memoization
paths :: Int -> Int -> Int
paths i j = paths' !! i !! j
    where 
    paths' = [[p m n | m <- [0..]] | n <- [0..]]
    p m n 
        | m == 0 || n == 0 = 1
        | m == n           = 2 * paths' !! m !! (n-1)
        | otherwise        = paths' !! m !! (n-1) + paths' !! (m-1) !! n

solution = paths 20 20

main = print solution
