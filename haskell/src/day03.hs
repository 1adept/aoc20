module Day03 where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import Text.Printf (printf)

newtype Map a = Map [a]
newtype Coord a = Coord (a, a)

main :: IO ()
main = do
  example <- T.readFile "../data/03_example.in"
  input <- T.readFile "../data/03_input.in"

  let treeE = Map $ T.lines example
  let treeI = Map $ T.lines input

  let allInstructions = [
        Coord (1, 1), 
        Coord (3, 1), 
        Coord (5, 1), 
        Coord (7, 1), 
        Coord (1, 2)]
  let singleInstruction = [allInstructions !! 1]

  putStrLn "Example:"
  printf "Part1: %d\n" (solve treeE singleInstruction)
  printf "Part2: %d\n" (solve treeE allInstructions)
  
  putStrLn "Input:"
  printf "Part1: %d\n" (solve treeI singleInstruction)
  printf "Part2: %d\n" (solve treeI allInstructions)
  
  putStrLn "\nEnd day 03"

solve :: Map T.Text -> [Coord Int] -> Int
solve = travel

travel :: Map T.Text -> [Coord Int] -> Int
travel treeMap@(Map m) moves = product $ map f moves
  where
    f move = travelSlope treeMap move (Coord (0, 0)) 0

    travelSlope :: Map T.Text -> Coord Int -> Coord Int -> Int -> Int
    travelSlope map@(Map m) move@(Coord (moveX, moveY)) (Coord (x, y)) count
      | y >= length m = count
      | otherwise =
          let nextX = flip mod (T.length . head $ m) $ x + moveX
              coordValue =
                if '#' == T.unpack (m !! y) !! x
                  then 1
                  else 0
           in travelSlope map move (Coord (nextX, y + moveY)) (count + coordValue)
