import Data.IntMap.Strict (IntMap, fromList, fromListWith, insert, member, (!))
import Data.List (sort)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

findVal :: IntMap Int -> Int -> Int -> Int
findVal hashMap val offset = if' (member (val - offset) hashMap) (hashMap ! (val - offset)) 0

buildHistDiff :: [Int] -> IntMap Int
buildHistDiff numList = fromListWith (+) (zipWith (\x y -> (x - y, 1)) (tail numList) (init numList))

run' :: [Int] -> Int -> IntMap Int -> [Int] -> IntMap Int
run' sortedList index hash offsets
    | index == length sortedList = hash
    | otherwise = run' sortedList (index + 1) updateHash offsets
  where
    val = sortedList !! index
    sumValues = sum $ map (findVal hash val) offsets
    updateHash = insert (sortedList !! index) sumValues hash

run :: [Int] -> IntMap Int
run sortedList = run' sortedList 1 (fromList [(0, 1)]) [1, 2, 3]

main :: IO ()
main = do
    contents <- getContents
    let numList = map read $ words contents :: [Int]
    let sorted = 0 : sort numList
    let sortedFull = sorted ++ [maximum sorted + 3]
    let hist = buildHistDiff sortedFull
    let hash = run sortedFull
    print $ (hist ! 3) * (hist ! 1)
    print $ hash ! last sortedFull