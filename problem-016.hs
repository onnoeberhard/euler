import Data.Char (digitToInt)

solution = sum . map digitToInt . show $ 2^1000

main = print solution
