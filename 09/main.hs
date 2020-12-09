slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

(!@) :: [a] -> (Int, Int) -> [a]
xs !@ (from, to) = slice from to xs

findNon :: [Int] -> Int -> Int -> Int
findNon numList index sumLength
    | num `elem` [a + b | a <- prevNums, b <- prevNums, a /= b] = findNon numList (index + 1) sumLength
    | otherwise = num
  where
    num = numList !! index
    prevNums = numList !@ (index - sumLength, index)

findContSum' :: [Int] -> Int -> Int -> Int -> Int -> [Int]
findContSum' numList missing start end runningSum
    | runningSum == missing = numList !@ (start, end)
    | runningSum < missing = findContSum' numList missing start (end + 1) (runningSum + numList !! end)
    | runningSum > missing = findContSum' numList missing (start + 1) (start + 1) 0

findContSum :: [Int] -> Int -> [Int]
findContSum numList missing = findContSum' numList missing 0 0 0

main :: IO ()
main = do
    contents <- getContents
    let numList = map read $ words contents :: [Int]
    let missing = findNon numList 25 25
    let contSet = findContSum numList missing
    print missing
    print $ minimum contSet + maximum contSet
