fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = fib (n - 1) + fib (n - 2)

solution = sum [fib n | n <- [1..35], fib n <= 4000000, even (fib n)]

main = do
    print solution
