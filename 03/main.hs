process :: Num p => Int -> Int -> Int -> Int -> [[Char]] -> p
process right down startx starty content 
    | starty >= length content = 0
    | otherwise = process right down (startx + right) (starty + down) content + a
        where v = content !! starty !! mod startx (length (content !! starty))
              a = if v == '#' then 1 else 0


main :: IO ()
main = do
    content <- getContents
    print $ process 3 1 0 0 (lines content)
    print $ process 1 1 0 0 (lines content) * process 3 1 0 0 (lines content) * process 5 1 0 0 (lines content) * process 7 1 0 0 (lines content) * process 1 2 0 0 (lines content)