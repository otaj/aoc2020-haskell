if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

process1' :: [[Char]] -> Int -> Int -> Int -> (Int, Int)
process1' [] east north _ = (east, north)
process1' ((c : numS) : instr) east north dir
    | c == 'N' = process1' instr east (north + num) dir
    | c == 'E' = process1' instr (east + num) north dir
    | c == 'S' = process1' instr east (north - num) dir
    | c == 'W' = process1' instr (east - num) north dir
    | c == 'L' = process1' instr east north newDir
    | c == 'R' = process1' instr east north newDir
    | c == 'F' = process1' instr newEast newNorth dir
  where
    num = read numS :: Int
    angle = num `div` 90
    angleDir = if' (c == 'L') (-1) 1
    newDir = (dir + (angleDir * angle)) `mod` 4
    (newEast, newNorth) = case dir of
        0 -> (east + num, north)
        1 -> (east, north - num)
        2 -> (east - num, north)
        3 -> (east, north + num)

process1 :: [[Char]] -> (Int, Int)
process1 instr = process1' instr 0 0 0

process2' :: [[Char]] -> Int -> Int -> Int -> Int -> (Int, Int)
process2' [] east north _ _ = (east, north)
process2' ((c : numS) : instr) east north wEast wNorth
    | c == 'N' = process2' instr east north wEast (wNorth + num)
    | c == 'E' = process2' instr east north (wEast + num) wNorth
    | c == 'S' = process2' instr east north wEast (wNorth - num)
    | c == 'W' = process2' instr east north (wEast - num) wNorth
    | c == 'R' = process2' instr east north newEast newNorth
    | c == 'L' = process2' instr east north newEast newNorth
    | c == 'F' = process2' instr (east + num * wEast) (north + num * wNorth) wEast wNorth
  where
    num = read numS :: Int
    angle = num `div` 90
    angleDir = if' (c == 'L') (-1) 1
    change = (angle * angleDir) `mod` 4
    (newEast, newNorth) = case change of
        0 -> (wEast, wNorth)
        1 -> (wNorth, - wEast)
        2 -> (- wEast, - wNorth)
        3 -> (- wNorth, wEast)

process2 :: [[Char]] -> (Int, Int)
process2 instr = process2' instr 0 0 10 1

main :: IO ()
main = do
    content <- getContents
    let (east1, north1) = process1 $ lines content
    let (east2, north2) = process2 $ lines content
    print $ abs east1 + abs north1
    print $ abs east2 + abs north2