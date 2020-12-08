parseLines :: [[Char]] -> [[String]] -> [[String]]
parseLines allLines passPorts
  | null passPorts = parseLines allLines [[]] ++ passPorts
  | null allLines = passPorts
parseLines (line : restLines) passPorts@(curPass : restPass)
  | null line = parseLines restLines [[]] ++ passPorts
  | otherwise = parseLines restLines [passPart ++ curPass] ++ restPass
  where
    passPart = words line

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y : ys)
  | x == y = removeItem x ys
  | otherwise = y : removeItem x ys

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

isNumeric :: [Char] -> Bool
isNumeric = all (`elem` "0123456789")

inBounds :: Ord a => a -> a -> a -> Bool
inBounds minV maxV val = val >= minV && val <= maxV

isValidYear :: [Char] -> Int -> Int -> Bool
isValidYear value minValue maxValue
  | length value /= 4 = False
  | not $ isNumeric value = False
  | otherwise = inBounds minValue maxValue intValue
  where
    intValue = read value :: Int

isValidHgt :: [Char] -> Bool
isValidHgt value
  | unit `notElem` ["cm", "in"] = False
  | not $ isNumeric amount = False
  | otherwise = isValidInUnit
  where
    (amount, unit) = splitAt (length value - 2) value
    amountInt = read amount :: Int
    isValidInUnit = case unit of
      "cm" -> inBounds 150 193 amountInt
      "in" -> inBounds 59 76 amountInt

isValidHcl :: [Char] -> Bool
isValidHcl value@(start : rest)
  | length value /= 7 = False
  | start /= '#' = False
  | otherwise = all (`elem` "0123456789abcdef") rest

isValidEcl :: [Char] -> Bool
isValidEcl value = value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPid :: [Char] -> Bool
isValidPid value
  | length value /= 9 = False
  | not $ isNumeric value = False
  | otherwise = True

isValidField :: [Char] -> [Char] -> Bool
isValidField field value
  | field == "byr" = isValidYear value 1920 2002
  | field == "iyr" = isValidYear value 2010 2020
  | field == "eyr" = isValidYear value 2020 2030
  | field == "hgt" = isValidHgt value
  | field == "hcl" = isValidHcl value
  | field == "ecl" = isValidEcl value
  | field == "pid" = isValidPid value
  | field == "cid" = True
  | otherwise = False

isValid :: Foldable t => [t Char] -> [[Char]] -> Bool -> Bool
isValid pass fields checkFields
  | null fields = True
  | null pass = False
  | checkFields && not (isValidField field value) = False
  | otherwise = isValid restPass removedfield checkFields
  where
    passPart : restPass = pass
    [field, value] = splitBy ':' passPart
    removedfield = removeItem field fields

process :: (Num a, Foldable t) => [[t Char]] -> [[Char]] -> Bool -> a
process passports fields checkFields = sum [1 | pass <- passports, isValid pass fields checkFields]

main :: IO ()
main = do
  content <- getContents
  print $ process (parseLines (lines content) []) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] False
  print $ process (parseLines (lines content) []) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] True