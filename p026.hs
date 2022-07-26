import Data.List (maximumBy)
import Data.Function (on)

-- Long division
ldiv :: Int -> Int -> [Int]
ldiv 0 _ = []
ldiv a b = a `div` b : ldiv (a `mod` b * 10) b

-- Find cycle length with long division (doesn't use ldiv)
cyc :: Int -> Int -> [Int] -> Int
cyc 0 _ _ = 0
cyc a b c
    | a `elem` c = 1 + (length . takeWhile (/= a)) c
    | otherwise = cyc (a `mod` b * 10) b (a:c)

cyc' :: Int -> Int
cyc' n = cyc 1 n []

solution = fst . maximumBy (compare `on` snd) $ map (\n -> (n, cyc' n)) [2..1000]

main = print solution
