-- Check if number is palindrome
pal :: Int -> Bool
pal x = let s = show x in s == reverse s

solution = maximum [z | x <- [1..999], y <- [1..999], let z = x * y, pal z]

main = do
    print solution
