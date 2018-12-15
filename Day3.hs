import Data.Maybe


input = 361527

--------------------- Part 1 -------------------- 

run1 :: Int -> Int
run1 = l1DistanceToOrigo . numToCoordinate

answer1 = run1 input

--------------------- 

numToCoordinate :: Int -> Maybe Point
numToCoordinate x = borderWalk (nearestSquareBase x) x

borderWalk :: Maybe (Int, Int) -> Int -> Maybe Point
borderWalk Nothing _         = Nothing 
borderWalk (Just (layerBase, layerNum)) stop = 
  Just $ endPoint start layerBase stop
  where start = (layerNum, -layerNum)

endPoint :: Point -> Int -> Int -> Point
endPoint startPoint layerBase stop =
  let cornerValue   = (layerBase)^2 
      circumference = cornerValue - (layerBase-2)^2
      sideLength    = circumference `div` 4
      borderPath    = concat [take sideLength (repeat xs) | xs <- traverseOrder]
      distance      = stop - (cornerValue - circumference)
      steps         = take distance borderPath 
  in walk startPoint $ take distance steps 

walk :: Point -> [Direction] -> Point
walk end [] =  end
walk pos (d:ds) = walk (step pos d) ds

step :: Point -> Direction -> Point
step (x,y) U = (x, y+1) 
step (x,y) D = (x, y-1)
step (x,y) R = (x+1, y) 
step (x,y) L = (x-1, y) 

--------------------- Part 2 -------------------- 

----------------- Shared Functions --------------

type Point = (Int, Int)
data Direction = U | D | R | L deriving (Show)

traverseOrder :: [Direction]
traverseOrder = [U, L, D, R]

layerBases = [3,5..]

layerNumbers = [1..]

baseAndNums = zip layerBases layerNumbers

nearestSquareBase :: Int -> Maybe (Int, Int)
nearestSquareBase x
  | x <= 1     = Nothing
  | otherwise  = Just $ head $ dropWhile pred baseAndNums
    where pred = (\(b, _n) -> b^2 < x)

l1DistanceToOrigo :: Maybe Point -> Int
l1DistanceToOrigo Nothing      = 0
l1DistanceToOrigo (Just (x,y)) = (abs x) + (abs y)
