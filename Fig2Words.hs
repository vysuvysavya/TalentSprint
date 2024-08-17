import qualified Data.Map as M
import Data.Maybe(fromMaybe)

oneWords :: [String]
oneWords = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", 
            "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", 
            "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"]

tens :: [String]
tens = ["", "", "Twenty", "Thirty", "Forty", "Fifty",
        "Sixty", "Seventy", "Eighty", "Ninety"]

indian :: M.Map Int String
indian = M.fromList [(10000000, "Crore"), (100000, "Lakh"), 
                     (1000, "Thousand"), (1, "")]

western :: M.Map Int String
western = M.fromList [(10^9, "Billion"), (10^6, "Million"),
                      (1000, "Thousand"), (1, "")]

twoDigits :: Int -> [String] 
twoDigits n = if n < 20
              then [oneWords !! n]
              else [tens !! div n 10, oneWords !! mod n 10]

threeDigits :: Int -> [String]
threeDigits n | n < 100        = twoDigits n
threeDigits n | mod n 100 == 0 = [oneWords !! div n 100, "Hundred"]
threeDigits n | otherwise      = [oneWords !! div n 100, "Hundred and"] 
                                  <>  twoDigits (mod n 100)

toWords :: Int -> String
toWords = unwords . threeDigits

splitInto :: [Int] -> Int -> [(Int, Int)]
splitInto [] _      = []
splitInto (d: ds) n = (div n d, d) : splitInto ds (mod n d)

pieces :: [Int] -> Int -> [(Int, Int)]
pieces ds n = filter (\x -> fst x /= 0) $ splitInto ds n

lookUp :: M.Map Int String -> Int -> String
lookUp dict n = fromMaybe "" $ M.lookup n dict

iLookUp :: Int -> String
iLookUp = lookUp indian
indis :: [Int]
indis = reverse $ M.keys indian

wLookUp :: Int -> String
wLookUp = lookUp western
wests :: [Int]
wests = reverse $ M.keys western


indianFigToWords :: Int -> String
indianFigToWords n = let values = reverse $ M.keys indian in 
        concat [toWords x <> " " <> iLookUp y <> " " | (x, y) <- pieces values n]

westernFigToWords :: Int -> String
westernFigToWords n = let values = reverse $ M.keys western in 
        concat [toWords x <> " " <> wLookUp y <> " " | (x, y) <- pieces values n]     

