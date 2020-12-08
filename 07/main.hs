import qualified Data.Map.Strict as M

parseRest :: [[Char]] -> [([Char], Int)]
parseRest [] = []
parseRest (val : firstBag : secondBag : _ : rest) = (firstBag ++ " " ++ secondBag, intval) : parseRest rest where intval = read val :: Int

parseLine :: [[Char]] -> ([Char], [([Char], Int)])
parseLine (firstBag : secondBag : "bags" : "contain" : rest)
  | rest == ["no", "other", "bags."] = (firstBag ++ " " ++ secondBag, [])
  | otherwise = (firstBag ++ " " ++ secondBag, parseRest rest)

allInsides :: [([Char], Int)] -> [([Char], Int)] -> [([Char], [([Char], Int)])] -> [([Char], Int)]
allInsides insides result bags
  | null insides = result
  | otherwise = allInsides extraInsides transInsides bags
  where
    transInsides = M.toList $ M.fromListWith (+) (insides ++ result)
    extraInsides = M.toList $ M.fromListWith (+) ([(bagR, curAmount * amount) | (bag, curAmount) <- insides, (bagi, bInsides) <- bags, bag == bagi, (bagR, amount) <- bInsides])

processPart1 :: Num a => [Char] -> [([Char], [([Char], Int)])] -> a
processPart1 needle bags = sum [1 | (_, origInsides) <- bags, (posNeedle, _) <- allInsides origInsides [] bags, needle == posNeedle]

processPart2 :: [Char] -> [([Char], [([Char], Int)])] -> Int
processPart2 needle bags = sum [amount | (posNeedle, origInsides) <- bags, posNeedle == needle, (_, amount) <- allInsides origInsides [] bags]

main :: IO ()
main = do
  content <- getContents
  print $ processPart1 "shiny gold" $ map (parseLine . words) (lines content)
  print $ processPart2 "shiny gold" $ map (parseLine . words) (lines content)