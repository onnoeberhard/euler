-- Sum of proper divisors of a number n
d :: Int -> Int
d n = sum . filter (\x -> n `mod` x == 0) $ [1..(n `div` 2)]

ami :: Int -> Bool
ami a = let b = d a in b /= a && a == d b

solution = sum . filter ami $ [1..9999]

main = print solution
