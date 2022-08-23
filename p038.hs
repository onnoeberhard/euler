import Data.List (sort)
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)

pan :: Int -> Maybe Int
pan n
    | (map digitToInt . sort) s /= [1..9] = Nothing
    | otherwise = Just (read s :: Int)
    where s = last . takeWhile (\x -> length x <= 9) . scanl1 (++) $ map (show . (*n)) [1..9]

solution = maximum . mapMaybe pan $ [1..500000]

main = print solution
