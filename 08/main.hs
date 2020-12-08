if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
    | n == 0 = newVal : xs
    | otherwise = x : replaceNth (n - 1) newVal xs

parseContent' :: [Char] -> (String, Int, Bool)
parseContent' line = (instr, intAmount, False)
  where
    [instr, amount] = words line
    sign = head amount
    intAmount = read $ if' (sign == '+') (tail amount) amount :: Int

parseContent :: String -> [(String, Int, Bool)]
parseContent content = map parseContent' (lines content)

runProgram' :: [([Char], Int, Bool)] -> Int -> Int -> (Int, Bool)
runProgram' program acc index
    | index == length program = (acc, True)
    | instrMet = (acc, False)
    | instr == "jmp" = runProgram' newProgram acc (index + amount)
    | instr == "acc" = runProgram' newProgram (acc + amount) (index + 1)
    | instr == "nop" = runProgram' newProgram acc (index + 1)
  where
    (instr, amount, instrMet) = program !! index
    newProgram = replaceNth index (instr, amount, True) program

runProgram :: [([Char], Int, Bool)] -> (Int, Bool)
runProgram program = runProgram' program 0 0

changingProgram' :: [([Char], Int, Bool)] -> Int -> Int
changingProgram' program index
    | instr == "acc" = changingProgram' program (index + 1)
    | metFinish = result
    | otherwise = changingProgram' program (index + 1)
  where
    (instr, amount, _) = program !! index
    newInstr = if' (instr == "nop") "jmp" "nop"
    newProgram = replaceNth index (newInstr, amount, False) program
    (result, metFinish) = runProgram newProgram

changingProgram :: [([Char], Int, Bool)] -> Int
changingProgram program = changingProgram' program 0

main :: IO ()
main = do
    content <- getContents
    print $ runProgram $ parseContent content
    print $ changingProgram $ parseContent content
