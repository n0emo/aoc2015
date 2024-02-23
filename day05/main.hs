import Data.List
import System.IO

count :: (t -> Bool) -> [t] -> Int
count p = foldr (\x -> (+) (if p x then 1 else 0)) 0

isNiceStringPartOne :: String -> Bool
isNiceStringPartOne =
  and <$> sequence [has3Vowels, hasNoDisallowed, hasCharTwiceInARow]
  where
    has3Vowels s = count (`elem` "aeiou") s >= 3
    hasNoDisallowed s = count (`elem` disallowed) (pairs s) == 0
    disallowed = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]
    hasCharTwiceInARow s = count (uncurry (==)) (pairs s) > 0
    pairs s = zip s (tail s)

isNiceStringPartTwo :: String -> Bool
isNiceStringPartTwo = and <$> sequence [hasNicePair, hasNiceTriple]
  where
    hasNicePair s =
      count
        (\i -> slice i (i + 2) s `isInfixOf` drop (i + 2) s)
        [0 .. length s - 2]
        > 0
    hasNiceTriple s =
      count
        (\(a, _, c) -> a == c)
        (zip3 s (tail s) (tail $ tail s))
        > 0
    slice a b = take (b - a) . drop a

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  contents <- hGetContents file
  let input = lines contents

  putStr "Nice string (part one): "
  print $ count isNiceStringPartOne input
  putStr "Nice string (part two): "
  print $ count isNiceStringPartTwo input

  hClose file
