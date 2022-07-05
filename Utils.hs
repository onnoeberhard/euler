import Data.List.Ordered (minus, union)

primes :: [Integer]
primes = 2:[3..] `minus` foldr (\p r -> p*p : union [p*p+p, p*p+2*p..] r) [] primes
-- i don't understand...
