-- nth element

nth :: Int -> [a] -> a
nth n xs = head (drop (n) xs)