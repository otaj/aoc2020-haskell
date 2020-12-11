slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

(!@) :: [a] -> (Int, Int) -> [a]
xs !@ (from, to) = slice from to xs

replace2D :: (Int, Int) -> a -> [[a]] -> [[a]]
replace2D _ _ [] = []
replace2D (n1, n2) newVal (x : xs)
    | n1 == 0 = replaceNth n2 newVal x : xs
    | otherwise = x : replace2D (n1 - 1, n2) newVal xs

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
    | n == 0 = newVal : xs
    | otherwise = x : replaceNth (n - 1) newVal xs

neigh1 :: [[Char]] -> Int -> Int -> [Char]
neigh1 seats x y = concat $ map (!@ ylim) seats !@ xlim
  where
    xlim = (max (x -1) 0, min (x + 1) (length seats - 1))
    ylim = (max (y -1) 0, min (y + 1) (length (seats !! x) - 1))

getDir :: [[Char]] -> Int -> Int -> Int -> Int -> Char
getDir seats x y d1 d2
    | x < 0 || (x >= length seats) || y < 0 || (y >= length (seats !! x)) = '.'
    | v == '.' = getDir seats (x + d1) (y + d2) d1 d2
    | otherwise = v
  where
    v = seats !! x !! y

neigh2 :: [[Char]] -> Int -> Int -> [Char]
neigh2 seats x y = map (\(d1, d2) -> getDir seats (x + d1) (y + d2) d1 d2) [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

evolveStep :: [[Char]] -> [[Char]] -> Int -> Int -> ([[Char]] -> Int -> Int -> [Char]) -> [[Char]]
evolveStep newSeats seats x y neighFn
    | x == length seats = newSeats
    | y == length (seats !! x) = evolveStep newSeats seats (x + 1) 0 neighFn
    | v == 'L' && numOccupied == 0 = evolveStep occSeats seats x (y + 1) neighFn
    | v == '#' && numOccupied > 4 = evolveStep emptiedSeats seats x (y + 1) neighFn
    | otherwise = evolveStep newSeats seats x (y + 1) neighFn
  where
    v = seats !! x !! y
    neigh = neighFn seats x y
    numOccupied = sum [1 | c <- neigh, c == '#']
    occSeats = replace2D (x, y) '#' newSeats
    emptiedSeats = replace2D (x, y) 'L' newSeats

evolve :: [[Char]] -> [[Char]] -> ([[Char]] -> Int -> Int -> [Char]) -> [[Char]]
evolve newSeats seats f
    | resultSeats == seats = seats
    | otherwise = evolve resultSeats resultSeats f
  where
    resultSeats = evolveStep newSeats seats 0 0 f

run :: [[Char]] -> ([[Char]] -> Int -> Int -> [Char]) -> [[Char]]
run seats = evolve seats seats

main :: IO ()
main = do
    content <- getContents
    let seats = lines content
    let stableSeats1 = run seats neigh1
    let stableSeats2 = run seats neigh2
    print $ sum [sum [1 | c <- row, c == '#'] | row <- stableSeats1]
    print $ sum [sum [1 | c <- row, c == '#'] | row <- stableSeats2]