parseLines :: [String] -> [String] -> [String]
parseLines allLines passPorts
    | null passPorts = parseLines allLines [[]]
    | null allLines = passPorts
parseLines (line : restLines) passPorts@(curPass : restPass)
    | null line = parseLines restLines [[]] ++ passPorts
    | otherwise = parseLines restLines [line ++ curPass] ++ restPass

unique' :: Eq a => [a] -> [a] -> [a]
unique' xss acc
    | null acc = unique' xs [x]
    | null xss = acc
    | x `elem` acc = unique' xs acc
    | otherwise = unique' xs (x : acc)
  where
    x : xs = xss

unique :: Eq a => [a] -> [a]
unique xs = unique' xs []

process :: Eq a => [[a]] -> Int
process quests = sum [c | c <- map (length . unique) quests]

parseLines2 :: [[Char]] -> [[Char]] -> [[Char]]
parseLines2 allLines passPorts
    | null passPorts = parseLines2 allLines ["abcdefghijklmnopqrstvuwxyz"]
    | null allLines = passPorts
parseLines2 (line : restLines) passPorts@(curPass : restPass)
    | null line = parseLines2 restLines ["abcdefghijklmnopqrstvuwxyz"] ++ passPorts
    | otherwise = parseLines2 restLines ([c | c <- curPass, c `elem` line] : restPass)

main = do
    content <- getContents
    print $ process (parseLines (lines content) [])
    print $ sum $ map length (parseLines2 (lines content) [])