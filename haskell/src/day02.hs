{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Text.Printf (printf)
import qualified Data.Char as Char

data Password = Password {
  low :: Int,
  high :: Int,
  character :: Char,
  pwd :: String
} deriving Show

main :: IO ()
main = do
    example <- readFile "../data/02_example.in"
    input <- readFile "../data/02_input.in"

    let ePwd = parse example
    let iPwd = parse input

    printf "Example part1: %s\n" $ show (solve1 ePwd)
    printf "Example part2: %s\n" $ show (solve2 ePwd)

    printf "Input part1 %s\n" $ show (solve1 iPwd)  
    printf "Input part2 %s\n" $ show (solve2 iPwd)

    putStrLn "End of day 02"

solve1, solve2 :: [Password] -> Int
solve1 =
  let 
    isValid p =
      let
        c = character p
        charCount = length . filter (== c) $ pwd p
      in (charCount >= low p) && (charCount <= high p)
  in length . filter isValid

solve2 =
  let
    isValid p =
      let
        first = pwd p !! (low p - 1)
        last = pwd p !! (high p - 1)
        c = character p
      in (first == c) /= (last == c)
  in length . filter isValid

parse :: String -> [Password]
parse txt = map parseLine $ lines txt
  where parseLine = 
          let
            num = read . takeWhile Char.isDigit
            char = head . take 1
            pass = tail
          in Password <$> num <* drop 1 <*> num <*> char <* drop 1 <*> pass