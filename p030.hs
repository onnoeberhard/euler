import Data.Char (digitToInt)

digitSum5 :: Int -> Int
digitSum5 = sum . map ((^5) . digitToInt) . show

solution = sum . filter (\n -> n == digitSum5 n) $ [10..1000000]

main = print solution
