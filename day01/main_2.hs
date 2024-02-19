import System.IO

traverseBraces :: (Integral a) => (a, a, [Char]) -> (a, a, [Char])
traverseBraces (pos, -1, _ : bs) = (pos, -1, bs)
traverseBraces (pos, cur, '(' : bs) = traverseBraces (pos + 1, cur + 1, bs)
traverseBraces (pos, cur, ')' : bs) = traverseBraces (pos + 1, cur - 1, bs)
traverseBraces (pos, cur, _ : bs) = traverseBraces (pos + 1, cur, bs)

tupFirst (a, b, c) = a

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file
  let result = traverseBraces (0, 0, contents)
  let pos = tupFirst result
  print pos
  hClose file
