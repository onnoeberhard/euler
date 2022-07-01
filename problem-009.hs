triplets :: Int -> [(Int, Int, Int)]
triplets n = [(a, b, c) | c <- [2..n], b <- [1..c-1], let a = n - b - c]

pythagorean :: (Int, Int, Int) -> Bool
pythagorean (a, b, c) = a^2 + b^2 == c^2

pt :: Int -> (Int, Int, Int)
pt n = head . dropWhile (not . pythagorean) $ triplets n

solution = let (a, b, c) = pt 1000 in a * b * c

main = do
    print solution
