import System.IO

splitBy :: (Char -> Bool) -> String -> [String]
splitBy p s = case dropWhile p s of
  "" -> []
  s' -> w : splitBy p s''
    where
      (w, s'') = break p s'

parseBox :: String -> (Int, Int, Int)
parseBox str = case map read $ splitBy (== 'x') str of
  [a, b, c] -> (a, b, c)
  _badInput -> error "Bad input"

evalBoxPaper (l, w, h) =
  sum
    [ 2 * sideA,
      2 * sideB,
      2 * sideC,
      minimum [sideA, sideB, sideC]
    ]
  where
    sideA = l * w
    sideB = w * h
    sideC = h * l

evalBoxRibbon (l, w, h) = minimum [p l w, p l h, p w h] + l * w * h
  where
    p a b = a + a + b + b

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file

  let boxes = lines contents
  let eval f boxes = sum $ map (f . parseBox) boxes

  putStr "Wrapping paper: "
  print $ eval evalBoxPaper boxes
  putStr "Ribbon: "
  print $ eval evalBoxRibbon boxes

  hClose file
