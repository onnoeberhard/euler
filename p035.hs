import Data.Numbers.Primes (isPrime, primes)

isCirc :: Int -> Bool
isCirc = all (isPrime . read) . circs . show

circs s = s : takeWhile (/= s) [tail x ++ [head x] | x <- circs s]

solution = length . filter isCirc . takeWhile (< 1000000) $ primes

main = print solution
