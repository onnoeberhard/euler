-- Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\z -> z `mod` x /= 0) xs)

-- Prime number generator
primes :: [Int]
primes = sieve [2..]

-- Prime factor decomposition
fac :: Int -> [Int]
fac n
    | null factors = []
    | otherwise    = let k = head factors in k:fac (n `div` k)
    where factors = filter (\x -> n `mod` x == 0) (takeWhile (<= n) primes)

solution = maximum (fac 600851475143)

main = do
    print solution
