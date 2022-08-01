gaps = concatMap (replicate 4) [2,4..]    -- just scan with (+).

solution = sum . takeWhile (<= 1001^2) $ scanl (+) 1 gaps

main = print solution
