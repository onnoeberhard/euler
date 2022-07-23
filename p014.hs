collatz :: Int -> [Int]
collatz n = takeWhile (/= 0) $ iterate rule n
    where rule :: Int -> Int
          rule n
              | n == 1    = 0
              | even n    = n `div` 2
              | otherwise = 3*n + 1

max' :: Ord b => (a, b) -> (a, b) -> (a, b)
max' (x1, y1) (x2, y2)
    | y1 >= y2 = (x1, y1)
    | otherwise = (x2, y2)

solution = fst . foldl1 max' . map (\n -> (n, length . collatz $ n)) $ [1..999999]

main = print solution
