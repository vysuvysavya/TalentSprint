-- Take mod 2 of first three numbers
-- OOO => 3
-- EOO => 2
-- EEO => 1
-- EEE => 0

lookFor :: [Int] -> (Int -> Bool)
lookFor ns = if (sum $ map (mod 2) $ take 3 ns) > 1
             then even
             else odd

outlier :: [Int] -> Int
outlier ns = head $ filter (lookFor ns) ns

main = do
  print $ outlier [1, 2, 4, 6]
  print $ outlier [1, 2, 3, 7]
  print $ outlier [2, 2, 4, 3]
  print $ outlier [1, 5, 4, 7]
  
