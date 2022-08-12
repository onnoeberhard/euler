import Data.Ratio

curious :: Int -> Int -> Bool
curious a b = 
    a % b `elem` [c % d | i <- [0, 1], j <- [0, 1], let (c, d, e, f) = (x !! i, y !! j, x !! (1-i), y !! (1-j)), 
        e == f, d /= 0, e /= 0]
    where
        x = [a `div` 10, a `mod` 10]
        y = [b `div` 10, b `mod` 10]

solution = denominator $ product [a % b | a <- [10..99], b <- [(a + 1)..99], curious a b]

main = print solution
