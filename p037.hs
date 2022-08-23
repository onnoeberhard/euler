import Data.Numbers.Primes (isPrime, primes)

interestingl :: Int -> Bool
interestingl n = isPrime n && (n < 10 || (interestingl . read . tail . show) n)

interestingr :: Int -> Bool
interestingr n = isPrime n && (n < 10 || (interestingr . read . init . show) n)

solution = sum . take 11 . filter (\n -> interestingl n && interestingr n && n > 10) $ primes

main = print solution
