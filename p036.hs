-- Returns number in binary, but in reverse order
toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n = n `mod` 2 : toBin (n `div` 2)

pal :: Int -> Bool
pal n = (p . show) n && (p . toBin) n
    where p x = x == reverse x

solution = sum . filter pal $ [1..999999]

main = print solution
