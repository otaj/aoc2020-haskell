import Data.Char (digitToInt, intToDigit, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import Numeric

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
        | c == delimiter = [] : l
        | otherwise = (c : x) : xs

isBinary :: Char -> Bool
isBinary x = x `elem` ['0', '1']

binaryToInt :: [Char] -> Word64
binaryToInt str = val where ((val, _) : _) = Numeric.readInt 2 isBinary digitToInt str

intToBinary :: Word64 -> Int -> [Char]
intToBinary x pad = ['0' | _ <- [1 .. (pad - length res)]] ++ res
  where
    res = showIntAtBase 2 intToDigit x ""

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace

generateAdders' :: [Char] -> [[Char]]
generateAdders' [] = [[]]
generateAdders' (c : r)
    | isBinary c = map ('0' :) rest
    | otherwise = map ('0' :) rest ++ map ('1' :) rest
  where
    rest = generateAdders' r

generateAdders :: [Char] -> [Word64]
generateAdders chars = map binaryToInt $ generateAdders' chars

process1' :: [[Char]] -> M.Map Word64 [Char] -> [Char] -> M.Map Word64 [Char]
process1' [] result _ = result
process1' (line : rest) result mask
    | command == "mask" = process1' rest result newMask
    | otherwise = process1' rest newResult mask
  where
    (command, lineRest) = splitAt 4 line
    (a : newMask : _) = map trim $ splitBy '=' lineRest
    newVal = intToBinary (read newMask :: Word64) 36
    addr = read (init a) :: Word64
    newResult = M.insert addr (zipWith (\x y -> if' (isBinary x) x y) mask newVal) result

process1 :: [[Char]] -> M.Map Word64 [Char]
process1 content = process1' content M.empty []

process2' :: [[Char]] -> M.Map Word64 Word64 -> [Word64] -> [Char] -> M.Map Word64 Word64
process2' [] result _ _ = result
process2' (line : rest) result maskAdders mask
    | command == "mask" = process2' rest result newMaskAdders newMask
    | otherwise = process2' rest newResult maskAdders mask
  where
    (command, lineRest) = splitAt 4 line
    (a : newMask : _) = map trim $ splitBy '=' lineRest
    newMaskAdders = generateAdders newMask
    newVal = read newMask :: Word64
    addrBase = binaryToInt $ zipWith (\x y -> if' (x == 'X') '0' (if' (x == '1') '1' y)) mask (intToBinary (read (init a) :: Word64) 36)
    newResult = M.union (M.fromList [(addrBase + add, newVal) | add <- maskAdders]) result

process2 :: [[Char]] -> M.Map Word64 Word64
process2 content = process2' content M.empty [] []

main :: IO ()
main = do
    content <- getContents
    let result1 = process1 $ lines content
    let result2 = process2 $ lines content
    print $ M.foldl (\acc val -> acc + binaryToInt val) 0 result1
    print $ M.foldl (+) 0 result2