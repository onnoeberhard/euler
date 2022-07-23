ssq :: Int -> Int
ssq n = sum $ map (^2) [1..n]

sqs :: Int -> Int
sqs n = sum [1..n] ^ 2

sqd :: Int -> Int
sqd n = sqs n - ssq n

solution = sqd 100

main = do
    print solution
