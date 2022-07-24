import Data.List (delete)

permutations :: [Int] -> [[Int]]
permutations [x] = [[x]]
permutations xs = concat [[x:y | y <- permutations (delete x xs)] | x <- xs]

solution = concatMap show $ permutations [0..9] !! (1000000 - 1)

main = putStrLn solution
