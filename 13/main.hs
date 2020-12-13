if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

earliest num bID
    | m == 0 = 0
    | otherwise = bID - m
  where
    m = num `mod` bID

process' :: Int -> [[Char]] -> Int -> Int
process' _ [] best = best
process' num (mNum : rest) best
    | mNum == "x" = process' num rest best
    | otherwise = process' num rest newBest
  where
    bID = read mNum :: Int
    newBest = if' (earliest num bID < earliest num best) bID best

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
        | c == delimiter = [] : l
        | otherwise = (c : x) : xs

process :: Int -> [Char] -> Int
process num rest = process' num (splitBy ',' rest) (10 * num)

main :: IO ()
main = do
    content <- getContents
    let (numS : rest : _) = lines content
    let num = read numS :: Int
    let closest = process num rest
    print (closest * earliest num closest)