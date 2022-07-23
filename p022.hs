import Data.List.Split ( splitOn )
import Data.List ( sort )
import Data.Char ( ord )

solution = sum . zipWith (*) [1..] . map value . sort . splitOn "\",\"" . tail . init
    where value name = sum $ map (\c -> ord c - ord 'A' + 1) name

-- Start as runhaskell p022 < p022_names.txt
main = do
    names <- getContents
    print $ solution names
