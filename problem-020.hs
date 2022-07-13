import Data.Char (digitToInt)

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

solution = sum . map digitToInt . show $ fac 100

main = print solution
