import Data.Numbers.Primes (isPrime)

isCirc :: Int -> Bool
isCirc = all (isPrime . read) . circs . show

circs s = s : takeWhile (/= s) [tail x ++ [head x] | x <- circs s]

solution = length . filter isCirc $ [1..1000000]

main = print solution
