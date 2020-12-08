subsetsOfSize :: (Eq t, Num t) => t -> [a] -> [[a]]
subsetsOfSize 0 _ = [[]]
subsetsOfSize _ [] = []
subsetsOfSize n (x : xs) =
  [x : subs | subs <- subsetsOfSize (n -1) xs] -- ones starting with x,
    ++ subsetsOfSize n xs

numCombos :: (Eq t, Eq a, Num t, Num a) => t -> a -> [a] -> [a]
numCombos n summed xs = [product xx | xx <- subsetsOfSize n xs, sum xx == summed]

main :: IO ()
main = do
  contents <- getContents
  let numbers = map read (words contents) :: [Int]
  print (head $ numCombos 2 2020 numbers)
  print (head $ numCombos 3 2020 numbers)