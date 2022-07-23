numDays m y
    | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | m `elem` [4, 6, 9, 11] = 30
    | otherwise = if leap then 29 else 28
    where leap = y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0

days = [[let s = prev y m in [s + 1 .. s + numDays m y] | m <- [1..12]] | y <- [1900..2000]]
    where
    prev y m
        | m > 1 = last $ days !! (y - 1900) !! (m - 2)
        | y > 1900 = last $ days !! (y - 1901) !! 11
        | otherwise = 0

solution = sum $ map (sum . map (\ m -> fromEnum $ head m `mod` 7 == 0)) $ tail days

main = print solution
