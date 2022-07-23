-- Sieve of Eratosthenes (from p3)
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\z -> z `mod` x /= 0) xs)

-- Large factors <= n
lfacs :: Int -> [Int]
lfacs n = [last (takeWhile (< n) (scanl1 (*) (repeat x))) | x <- sieve [2..n]]

solution = product (lfacs 20)

main = do
    print solution
