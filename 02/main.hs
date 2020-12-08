isValidPart1 :: (Ord a1, Num a1, Eq a2) => a1 -> a1 -> a2 -> [a2] -> Bool
isValidPart1 minl maxl l word = minl <= a && a <= maxl where a = sum [1 | c <- word, c == l]

isValidPart2 :: Eq a => Int -> Int -> a -> [a] -> Bool
isValidPart2 pos1 pos2 l word = (word !! (pos1 - 1) == l) /= (word !! (pos2 - 1) == l)

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

parseNums :: Foldable t => t Char -> [Int]
parseNums str = map read (splitBy '-' str) :: [Int]

processLine :: [Char] -> (Int -> Int -> Char -> String -> t) -> t
processLine line f = f minl maxl letter word
  where
    nums : letters : rest = words line
    minl : maxl : _ = parseNums nums
    letter = head letters
    word = head rest

processAll :: String -> (Int -> Int -> Char -> String -> Bool) -> Int
processAll d f = sum [1 | x <- lines d, processLine x f]

main :: IO ()
main = do
  content <- getContents
  print $ processAll content isValidPart1
  print $ processAll content isValidPart2