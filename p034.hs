import Data.Char (digitToInt)
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

curious :: Int -> Bool
curious n = (sum . map (fac . digitToInt) . show) n == n

solution = sum [x | x <- [10..(7 * fac 9)], curious x]    -- cannot get larger than 7 * fac 9

main = print solution