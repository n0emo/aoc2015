import Data.List
import System.IO

count :: (t -> Bool) -> [t] -> Int
count p = foldr (\x -> (+) (if p x then 1 else 0)) 0

-- Part 1
vowels = "aeiou"

disallowed = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]

has3Vowels :: String -> Bool
has3Vowels s = count (`elem` vowels) s >= 3

pairs :: [a] -> [(a, a)]
pairs s = zip s (tail s)

hasNoDisallowed :: String -> Bool
hasNoDisallowed s = count (`elem` disallowed) (pairs s) == 0

hasCharTwiceInARow :: String -> Bool
hasCharTwiceInARow s = count (uncurry (==)) (pairs s) > 0

isNiceStringPartOne :: String -> Bool
isNiceStringPartOne =
  and <$> sequence [has3Vowels, hasNoDisallowed, hasCharTwiceInARow]

-- Part 2

-- squarePairs :: Int -> [([Int], [Int])]
-- squarePairs n =
--   [ (a, b)
--     | a <- pairs' n,
--       fst a == snd a - 1,
--       b <- pairs' n,
--       fst b == snd b - 1,
--       snd a < fst b
--   ]
--   where
--     pairs' n = [(a, b) | a <- [0 .. n - 1], b <- [0 .. n - 1]]
--
-- hasNicePair :: String -> Bool
-- hasNicePair s =
--   count
--     ( \([a, b], [c, d]) ->
--         (a == c && b == d) || (a == d && b == c)
--     )
--     pairs
--     > 0
--   where
--     sLen = length s
--     sArr = listArray (0, sLen - 1) s
--     indexPairs = squarePairs sLen
--     pairs = map (\((a, b), (c, d)) -> ([sArr ! a, sArr ! b], [sArr ! c, sArr ! d])) indexPairs

slice :: Int -> Int -> String -> String
slice a b = (take (b - a)) . (drop a)

hasNicePair :: String -> Bool
hasNicePair s =
  count
    (\i -> (slice i (i + 2) s) `isInfixOf` (drop (i + 2) s))
    [0 .. sLen - 2]
    > 0
  where
    sLen = length s

hasNiceTriple :: String -> Bool
hasNiceTriple s =
  count
    (\(a, _, c) -> a == c)
    (zip3 s (tail s) (tail $ tail s))
    > 0

isNiceStringPartTwo :: String -> Bool
isNiceStringPartTwo = and <$> sequence [hasNicePair, hasNiceTriple]

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
