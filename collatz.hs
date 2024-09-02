collatz :: Int -> [Int]
collatz n
    | n==4 = [4,2,1]
    | odd n = 3*n+1 : collatz 3*n+1
    | even n = n `div` 2 : collatz n `div` 2
main = do 
    collatz (1)