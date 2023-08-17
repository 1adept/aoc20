{-# OPTIONS_GHC -Wno-name-shadowing #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf (printf)

data Password = Password {
  low :: Int,
  high :: Int,
  character :: Char,
  pwd :: String
} deriving Show

main :: IO ()
main = do
    example <- T.readFile "../data/02_example.in"
    input <- T.readFile "../data/02_input.in"

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

parse :: T.Text -> [Password]
parse txt =
    let
      parse' :: T.Text -> Password
      parse' t =
        let
          words = map T.unpack $ T.words t
          minmax = head words
          -- "Cannot find Data.List.Split (splitOn)"
          -- lh = splitOn "-" minmax
          low = read . takeWhile (/= '-') $ minmax
          high = read . drop 1 . dropWhile (/= '-') $ minmax
          c   = head $ words !! 1
          pwd = words !! 2
        in Password low high c pwd
    in map parse' $ T.lines txt
