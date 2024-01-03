import Data.List (sort)
import Data.Numbers.Primes (isPrime, primes)

-- Generate n-pandigital numbers by inserting n at all places in all (n-1)-pandigital numbers.
pans :: Int -> [Int]
pans 1 = [1]
pans n = concat [dist n x | x <- pans (n - 1)]

dist :: Int -> Int -> [Int]
dist x y = [read (take n ys ++ xs ++ drop n ys) | n <- [0..length ys]]
    where
        ys = show y
        xs = show x

pd :: Int -> Bool
pd n = sort s == take (length s) "123456789"
    where s = show n

solution = maximum . filter isPrime . concatMap pans $ [1..9]

main = print solution
