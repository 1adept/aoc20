import Text.Printf
import System.Environment (getArgs)
import qualified Control.Monad

main :: IO ()
main = do
  args <- getArgs
  Control.Monad.when (null args) $ error "Please provide a file path"

  content <- readFile $ head args
  let values :: [Int] = map read $ lines content

  solve values 2
  solve values 3

  putStrLn "End of the day 01"

solve :: [Int] -> Int -> IO ()
solve list number = do
  let result = find2020In list number
  printf "%d values to make 2020 are %s with a product of %d\n" number (show result) (product result)

find2020In :: [Int] -> Int -> [Int]
find2020In list x =
    let
        find2020In' list indices =
          let
            values = map (list !!) indices
            lastIndexNext = last indices + 1                              -- last index increased by 1
            simple = take (length indices - 1) indices ++ [lastIndexNext] -- simple case, only last index gets increased

            step1 = takeWhile (\x -> x + 1 < length list) indices                   -- take valid indices
            step2 = take (length step1 - 1) step1 ++ [last step1 + 1]               -- increase the last index by 1
            step3 = step2 ++ replicate (length indices - length step2) (last step2) -- fill by last index to length of `indices`
          in if sum values == 2020
            then values
            else if lastIndexNext + 1 < length list
              then find2020In' list simple
              else find2020In' list step3
    in find2020In' list $ replicate x 0


