import Data.List (foldl')
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe


--------------------- Part 1 -------------------- 

solve1 = runMaybeT $ run revCaptcha


revCaptcha :: Integer -> Integer
revCaptcha = revCaptcha' . digits

revCaptcha' :: [Integer] -> Integer
revCaptcha' [] = 0
revCaptcha' (x:xs) = if x == lastnum then x+sum else sum
  where 
    (lastnum, sum) = foldl' foldFun (x, 0) xs


foldFun :: (Integer, Integer) -> Integer -> (Integer, Integer)
foldFun (match, sum ) x
  | x == match = (match, sum+x)
  | otherwise  = (x, sum)


--------------------- Part2 --------------------- 

solve2 = runMaybeT $ run revCaptcha2


revCaptcha2 :: Integer -> Integer
revCaptcha2 num = revCaptcha2' 0 xs step (cycle xs)
  where xs   = digits num
        step = (length xs) `div` 2


revCaptcha2' :: Integer -> [Integer] ->  Int -> [Integer] -> Integer
revCaptcha2' sum [] _ _  = sum
revCaptcha2' sum list@(x:xs) step inf@(y:ys)
  | x == (head . drop step ) inf = revCaptcha2' (sum+x) xs step ys
  | otherwise = revCaptcha2' sum xs step ys


----------------- Shared functions -------------- 

run:: (Integer -> Integer) ->  MaybeT IO Integer
run f = do
  content <- lift $ readFile "input_1.txt"
  i <- MaybeT $ return $ readMaybe content
  MaybeT $ return $ Just (f i)


-- |Convert a potentially large integer to a list of digits
digits :: Integer -> [Integer]
digits 0 = []
digits x = hd : digits xs 
  where
    hd = x `mod` 10
    xs = x `div` 10

