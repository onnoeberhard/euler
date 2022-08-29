import Data.Char (digitToInt)

chap :: String
chap = concatMap show [1..]

solution = product $ map (digitToInt . (chap !!) . pred . (10^)) [0..6]

main = print solution
