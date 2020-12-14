if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

earliest :: Integer -> Integer -> Integer
earliest num bID
    | m == 0 = 0
    | otherwise = bID - m
  where
    m = num `mod` bID

bezout' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
bezout' u1 _ v1 _ r1 0
    | r1 < 0 = (- u1, v1, - r1)
    | otherwise = (u1, v1, r1)
bezout' u1 u2 v1 v2 r1 r2 = bezout' u2 (u1 - (q * u2)) v2 (v1 - (q * v2)) r2 (r1 - (q * r2)) where q = r1 `quot` r2

bezout :: Integer -> Integer -> (Integer, Integer, Integer)
bezout = bezout' 1 0 0 1

crt :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
crt r1 m1 r2 m2
    | (r1 - r2) `mod` g /= 0 = error ""
    | otherwise = (r `mod` m, m)
  where
    (c1, c2, g) = bezout m1 m2
    m = (m1 * m2) `quot` g
    r = ((r1 * c2 * m2) + (r2 * c1 * m1)) `quot` g

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
        | c == delimiter = [] : l
        | otherwise = (c : x) : xs

process' :: Integer -> [[Char]] -> Integer -> Integer
process' _ [] best = best
process' num (mNum : rest) best
    | mNum == "x" = process' num rest best
    | otherwise = process' num rest newBest
  where
    bID = read mNum :: Integer
    newBest = if' (earliest num bID < earliest num best) bID best

process :: Integer -> [Char] -> Integer
process num rest = process' num (splitBy ',' rest) (10 * num)

process2' :: [([Char], Integer)] -> Integer -> Integer -> Integer
process2' [] n _ = n
process2' ((mNum, i) : rest) n m
    | mNum == "x" = process2' rest n m
    | otherwise = process2' rest newN newM
  where
    num = read mNum :: Integer
    busN = (num - i) `mod` num
    (newN, newM) = crt n m busN num

process2 :: [Char] -> Integer
process2 rest = process2' (zip (splitBy ',' rest) [0 ..]) 0 1

main :: IO ()
main = do
    content <- getContents
    let (numS : buses : _) = lines content
    let num = read numS :: Integer
    let closest = process num buses
    print (closest * earliest num closest)
    print $ process2 buses