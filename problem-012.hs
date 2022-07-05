-- Triangle numbers
tri :: [Int]
tri = scanl1 (+) [1..]

-- Number of factors
fac :: Int -> Int
fac n =
    length [x | x <- [1..s], n `mod` x == 0] * 2 - fromEnum (n == s^2)
    where s = floor . sqrt $ fromIntegral n

solution = fst . head . filter (\(a, b) -> b > 500) . map (\n -> (n, fac n)) $ tri

main = do
    print solution
