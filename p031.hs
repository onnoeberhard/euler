-- Number of ways to generate x from the selection of coins
num :: Int -> Int
num = num' coins
    where
        num' _ 0 = 1
        num' c x = sum [num' (filter (<= y) coins) z | y <- c, let z = x - y, z >= 0]
        coins = [1, 2, 5, 10, 20, 50, 100, 200]

solution = num 200

main = print solution

-- Previous attempt:
-- where num' = 1 : map (\x -> sum [num' !! z | y <- [1, 2, 5, 10, 20, 50, 100, 200], let z = x - y, z >= 0]) [1..]
-- this solves a different problem! (doubly counting the paths) here, memoization is crucial
