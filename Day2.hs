import Data.Maybe
import Text.Read (readMaybe)


main = interact linefun

--------------------- Part 1 -------------------- 

part1 :: [Int] -> Int
part1 = checksum1

checksum1 :: [Int] -> Int
checksum1 xs = (maximum xs) - (minimum xs) 


--------------------- Part 2 -------------------- 

part2 :: [Int] -> Int
part2 = sum . checksum2        

{- | A element is replaced by a zero if it had no divisors, 
     otherwise it will be replaced with the resulting division -}
checksum2 :: [Int]  -- ^ Assume no duplicates and at most one divisor per elem.
          -> [Int]  
checksum2 [] = []
checksum2 (x:xs) = (checksum2' filtered x : checksum2 xs)
  where filtered = filter (isEvenlyDiv x) xs 

checksum2' :: [Int] -> Int -> Int
checksum2' [] _ = 0 
checksum2' (x:_) y = (max x y) `div` (min x y)


-- Filter function 
isEvenlyDiv :: Int -> Int -> Bool
isEvenlyDiv x y = isEvenlyDiv' modulo
  where modulo = (max x y) `mod` (min x y)

isEvenlyDiv' :: Int -> Bool
isEvenlyDiv' 0 = True 
isEvenlyDiv' _ = False


----------------- Shared Functions --------------

-- Transform the intput to an output string
linefun :: String -> String
linefun x = (show ch) ++ "\n"
  where xs = parseInput x
        ch = sum $ map part2 xs -- choose part1 or part2


parseInput :: String -> [[Int]] 
parseInput x = map parseInt xs
  where xs = (map words . lines) x
        parseInt = (mapMaybe (readMaybe :: String -> Maybe Int)) 

