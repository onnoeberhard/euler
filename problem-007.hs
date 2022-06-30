-- False Sieve of Eratosthenes (from p3)
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\z -> z `mod` x /= 0) xs)

-- Prime number generator (from p3)
primes :: [Int]
primes = sieve [2..]

solution = primes !! 10000

main = do
    print solution
