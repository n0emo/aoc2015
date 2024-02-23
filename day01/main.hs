import System.IO

traverseBracesPartOne :: (Num a1) => (a1, [Char]) -> a1
traverseBracesPartOne (cur, []) = cur
traverseBracesPartOne (cur, '(' : bs) = traverseBracesPartOne (cur + 1, bs)
traverseBracesPartOne (cur, ')' : bs) = traverseBracesPartOne (cur - 1, bs)
traverseBracesPartOne (cur, _ : bs) = traverseBracesPartOne (cur, bs)

traverseBracesPartTwo :: (Integral a) => (a, a, [Char]) -> a
traverseBracesPartTwo (pos, -1, _ : bs) = pos
traverseBracesPartTwo (pos, cur, '(' : bs) = traverseBracesPartTwo (pos + 1, cur + 1, bs)
traverseBracesPartTwo (pos, cur, ')' : bs) = traverseBracesPartTwo (pos + 1, cur - 1, bs)
traverseBracesPartTwo (pos, cur, _ : bs) = traverseBracesPartTwo (pos + 1, cur, bs)

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file

  putStr "Final floor (part 1): "
  print $ traverseBracesPartOne (0, contents)
  putStr "Steps to -1'th floor (part 2): "
  print $ traverseBracesPartTwo (0, 0, contents)

  hClose file
