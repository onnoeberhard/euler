import Data.Numbers.Primes (primeFactors)
import Data.Set (Set, fromAscList, deleteMin, member, findMin, empty)

powerset :: Eq a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = let p = powerset xs in p ++ [y | s <- p, let y = x:s, y `notElem` p]

factors :: Int -> [Int]
factors = map product . powerset . primeFactors

properFactors :: Int -> [Int]
properFactors n = filter (/= n) $ factors n

abundand :: Int -> Bool
abundand n = (sum . properFactors) n > n

-- Possible to write n as a sum of two numbers in the given list?
possible :: Set Int -> Int -> Bool
possible s n
    | null s = False
    | otherwise = (n - findMin s) `member` s || possible (deleteMin s) n

solution = sum $ filter (not . possible as) [1..k]
    where
        k = 28123
        as = fromAscList $ filter abundand [1..k]

main = print solution
