import Data.List (nub)
solution = length . nub $ [x^y | x <- [2..100], y <- [2..100]]

main = print solution
