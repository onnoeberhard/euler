import Data.List.Split ( splitOn )
import Data.List ( sort )
import Data.Char ( ord )

-- Predicate to check if n is a triangle number
tri :: Int -> Bool
tri n = trimin n n

-- Predicate to check if n is a triangle number smaller than the mth triangle number
trimin :: Int -> Int -> Bool
trimin n 0 = False
trimin n m = n == t || trimin n (m - 1)
    where t = m * (m + 1) `div` 2

solution :: [Char] -> Int
solution = length . filter tri . map value . splitOn "\",\"" . tail . init
    where value = sum . map (\c -> ord c - ord 'A' + 1)

-- Start as runhaskell p042 < p042_words.txt
main = do
    words <- getContents
    print $ solution words
