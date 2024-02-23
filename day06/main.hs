import Data.Char (isDigit)
import Data.Int (Int64)
import System.IO

data ActType = TurnOn | TurnOff | Toggle | NoAct deriving (Enum, Show)

data Act = Act
  { actType :: !ActType,
    from :: !(Int, Int),
    to :: !(Int, Int)
  }
  deriving (Show)

actRow :: Act -> [ActType]
actRow act = left ++ mid ++ right
  where
    left = replicate (fst (from act)) NoAct
    mid = replicate (fst (to act) - fst (from act) + 1) (actType act)
    right = replicate (1000 - fst (to act) - 1) NoAct

actGrid :: Act -> [[ActType]]
actGrid act = left ++ mid ++ right
  where
    left = replicate (snd (from act)) (replicate 1000 NoAct)
    mid = replicate (snd (to act) - snd (from act) + 1) (actRow act)
    right = replicate (1000 - snd (to act) - 1) (replicate 1000 NoAct)

parseActType :: String -> (ActType, String)
parseActType ('t' : 'u' : 'r' : 'n' : ' ' : 'o' : 'n' : ' ' : rest) = (TurnOn, rest)
parseActType ('t' : 'u' : 'r' : 'n' : ' ' : 'o' : 'f' : 'f' : ' ' : rest) = (TurnOff, rest)
parseActType ('t' : 'o' : 'g' : 'g' : 'l' : 'e' : ' ' : rest) = (Toggle, rest)
parseActType _ = error "Act parsing error"

parseChar :: Char -> String -> (Char, String)
parseChar c = f
  where
    f (i : rest) | i == c = (i, rest)
    f _ = error "Char parsing error"

parseNumber :: String -> (Int, String)
parseNumber input = (result, rest)
  where
    result = read $ takeWhile isDigit input
    rest = dropWhile isDigit input

parseRange :: String -> ((Int, Int), String)
parseRange input = ((from, to), rest)
  where
    (from, input') = parseNumber input
    (_, input'') = parseChar ',' input'
    (to, rest) = parseNumber input''

skipThrough :: String -> String
skipThrough (' ' : 't' : 'h' : 'r' : 'o' : 'u' : 'g' : 'h' : ' ' : rest) = rest
skipThrough _ = error "Skip through error"

parseAct :: String -> Act
parseAct input = Act actType from to
  where
    (actType, input') = parseActType input
    (from, input'') = parseRange input'
    input''' = skipThrough input''
    (to, _) = parseRange input'''

startingGridPartOne :: [[Bool]]
startingGridPartOne = replicate 1000 $ replicate 1000 False

applyActPartOne :: ActType -> Bool -> Bool
applyActPartOne TurnOn b = True
applyActPartOne TurnOff b = False
applyActPartOne Toggle b = not b
applyActPartOne NoAct b = b

startingGridPartTwo :: [[Int64]]
startingGridPartTwo = replicate 1000 $ replicate 1000 0

applyActPartTwo :: ActType -> Int64 -> Int64
applyActPartTwo TurnOn i = i + 1
applyActPartTwo TurnOff i = if i > 0 then i - 1 else 0
applyActPartTwo Toggle i = i + 2
applyActPartTwo NoAct i = i

applyActs ::
  (ActType -> a -> a) ->
  [[[ActType]]] ->
  [[a]] ->
  [[a]]
applyActs f acts grid = foldl (flip (zipWith (zipWith f))) grid acts

countLightsPartOne :: [[Bool]] -> Int
countLightsPartOne grid = sum $ map (length . filter id) grid

countLightsPartTwo :: [[Int64]] -> Int64
countLightsPartTwo grid = sum $ map sum grid

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file
  let input = lines contents

  let acts = map (actGrid . parseAct) input

  putStr "Count (part 1): "
  print $
    countLightsPartOne $
      applyActs applyActPartOne acts startingGridPartOne
  putStr "Brightness (part 2): "
  print $
    countLightsPartTwo $
      applyActs applyActPartTwo acts startingGridPartTwo

  hClose file
