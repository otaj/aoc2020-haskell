processBoard :: String -> Int -> Int -> Int -> Int -> Int
processBoard [] minRow _ minSeat _ = 8 * minRow + minSeat
processBoard (c : rest) minRow maxRow minSeat maxSeat
  | c == 'F' = processBoard rest minRow (splitRow - 1) minSeat maxSeat
  | c == 'B' = processBoard rest splitRow maxRow minSeat maxSeat
  | c == 'L' = processBoard rest minRow maxRow minSeat (splitSeat - 1)
  | c == 'R' = processBoard rest minRow maxRow splitSeat maxSeat
  where
    splitRow = (maxRow + 1 - minRow) `div` 2 + minRow
    splitSeat = (maxSeat + 1 - minSeat) `div` 2 + minSeat

process :: [String] -> [Int]
process = map (\x -> processBoard x 0 127 0 7)

findMissing :: (Num a, Foldable t, Ord a, Enum a) => t a -> a
findMissing boardingIDs = head [passID | passID <- [1 .. maximum boardingIDs], passID `notElem` boardingIDs && (passID + 1) `elem` boardingIDs && (passID - 1) `elem` boardingIDs]

main :: IO ()
main = do
  content <- getContents
  print $ maximum $ process (lines content)
  print $ findMissing $ process (lines content)