import Data.List (maximumBy)
import Data.Function (on)

-- Pythagorean Triples
pyts :: Int -> [(Int, Int, Int)]
pyts n = [(a, b, c) | a <- [1 .. n`div`2], b <- [a .. n`div`2], let c = n - a - b, a^2 + b^2 == c^2]

solution = maximumBy (compare `on` length . pyts) [1..1000]

main = print solution
