-- Sieve of Eratosthenes (from p3)
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\z -> z `mod` x /= 0) xs)

-- Large factors <= n
lfacs :: Int -> [Int]
lfacs n = [lfac x n | x <- sieve [2..n]]
    where
        lfac x n = last (takeWhile (< n) (scanl (*) 1 (repeat x)))

solution = product (lfacs 20)

main = do
    print solution
