-- Sieve of Eratosthenes
filt :: Int -> [Bool]
filt n = replicate (n - 1) True ++ (tail . cycle $ False:replicate (n - 1) True)

sieve :: Int -> [Bool]
sieve 1 = repeat True
sieve n = zipWith (&&) (filt n) (sieve (n - 1))

primes :: Int -> [Int]
primes n = [x | (x, prime) <- zip [2..n] $ sieve (ceiling . sqrt $ fromIntegral n), prime]

solution = sum $ primes 2000000

main = do
    print solution
