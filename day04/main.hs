import Data.ByteString.Char8 qualified as BS
import Distribution.Utils.MD5 as MD5
import System.IO

hash :: String -> String
hash = showMD5 . MD5.md5 . BS.pack

validSN :: Int -> String -> Int -> Bool
validSN zeros s n = (all (== '0') . take zeros) . hash $ s ++ show n

mineFirstNum :: Int -> String -> Int
mineFirstNum zeros s = 1 + length (takeWhile (not . validSN zeros s) [1 ..])

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  input <- hGetLine file

  putStr "Mined number with 5 zeros: "
  print $ mineFirstNum 5 input
  putStr "Mined number with 6 zeros: "
  print $ mineFirstNum 6 input

  hClose file
