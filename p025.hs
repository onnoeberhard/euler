fib :: Int -> Integer
fib n = fib' !! (n - 1)
    where fib' = [1, 1] ++ zipWith (+) fib' (tail fib')

solution = head $ filter (\n -> (length . show . fib) n >= 1000) [1..]

main = print solution
