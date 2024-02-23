import Data.Int
import Data.Set qualified as Set
import System.IO

data Position = Position
  { x :: !Int32,
    y :: !Int32
  }
  deriving (Show, Eq, Ord)

type Visited = Set.Set Position

type Route = [Char]

data Santa = Santa
  { position :: !Position,
    visited :: !Visited,
    route :: !Route
  }
  deriving (Show)

startingSanta :: Route -> Santa
startingSanta = Santa (Position 0 0) Set.empty

nextPos :: Int32 -> Int32 -> Char -> Position
nextPos x y r = case r of
  '^' -> Position x (y + 1)
  'v' -> Position x (y - 1)
  '>' -> Position (x + 1) y
  '<' -> Position (x - 1) y
  _ -> Position x y

nextHouse :: Santa -> Santa
nextHouse (Santa (Position x y) visited []) =
  Santa (Position x y) (Set.insert (Position x y) visited) []
nextHouse (Santa (Position x y) visited (r : rs)) =
  nextHouse $ Santa (nextPos x y r) (Set.insert (Position x y) visited) rs

oddEvenPartition :: [a] -> ([a], [a])
oddEvenPartition = foldr (\x ~(ys, zs) -> (x : zs, ys)) ([], [])

countHousesSanta :: Route -> Int
countHousesSanta route =
  Set.size $ visited $ nextHouse $ startingSanta route

countHousesSantaAndRobo :: Route -> Int
countHousesSantaAndRobo route =
  Set.size $ Set.union visitedRobo visitedSanta
  where
    routes = oddEvenPartition route
    visitedSanta = visited $ nextHouse $ startingSanta $ fst routes
    visitedRobo = visited $ nextHouse $ startingSanta $ snd routes

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  route <- hGetContents file

  putStr "One Santa: "
  print $ countHousesSanta route
  putStr "Santa and Robo-Santa: "
  print $ countHousesSantaAndRobo route

  hClose file
