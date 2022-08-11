import Data.List (sort, nub)

-- Check all products of the form 2 digits * 3 digits = 4 digits and 1 digit * 4 digits = 4 digits
pd :: Int -> Int -> Int -> Bool
pd a b c = (sort . concatMap show) [a, b, c] == "123456789"

solution = (sum . nub) ([c | a <- [1..9], b <- [1000..9999], let c = a * b, c < 10000, pd a b c]
    ++ [c | a <- [10..99], b <- [100..999], let c = a * b, c < 10000, pd a b c])

main = print solution
