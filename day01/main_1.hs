import System.IO

traverseBraces :: (Num a1) => (a1, [Char]) -> (a1, [Char])
traverseBraces (cur, []) = (cur, [])
traverseBraces (cur, '(' : bs) = traverseBraces (cur + 1, bs)
traverseBraces (cur, ')' : bs) = traverseBraces (cur - 1, bs)
traverseBraces (cur, _ : bs) = traverseBraces (cur, bs)

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file
  let cur = fst $ traverseBraces (0, contents)
  print cur
  hClose file
