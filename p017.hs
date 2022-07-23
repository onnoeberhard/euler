say :: Int -> String
say 1 = "one"
say 2 = "two"
say 3 = "three"
say 4 = "four"
say 5 = "five"
say 6 = "six"
say 7 = "seven"
say 8 = "eight"
say 9 = "nine"
say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"
say 20 = "twenty"
say 40 = "forty"
say 1000 = "onethousand"
say n 
    | n < 20 = say' (n - 10) ++ "teen"
    | n < 100 && n `mod` 10 == 0 = say' (n `div` 10) ++ "ty"
    | n < 100 = say (n `div` 10 * 10) ++ say (n `mod` 10)
    | n < 1000 && n `mod` 100 == 0 = say (n `div` 100) ++ "hundred"
    | n < 1000 = say (n `div` 100 * 100) ++ "and" ++ say (n `mod` 100)
    | otherwise = ""

say' 3 = "thir"
say' 5 = "fif"
say' 8 = "eigh"
say' n = say n

solution = sum . map (length . say) $ [1..1000]

main = print solution
