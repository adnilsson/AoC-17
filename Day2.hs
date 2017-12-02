import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

main = interact linefun

linefun :: String -> String
linefun x = show ch
  where xs = (map words . lines) x
        ys = map (mapMaybe (readMaybe :: String -> Maybe Int)) xs
        ch = sum $ map checksum ys


checksum :: [Int] -> Int
checksum xs = (maximum xs) - (minimum xs) 

