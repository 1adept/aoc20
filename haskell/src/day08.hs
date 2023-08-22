{-# LANGUAGE LambdaCase #-}

import qualified Data.Set as S
import Text.Parsec (Parsec, char, digit, letter, many, parse, space, (<|>))
import Text.Printf (printf)

data Instruction = Instruction Action Int
  deriving (Show)

data Action
  = NOP
  | ACC
  | JMP
  deriving (Show)

data State = State
  { pointer :: Int,
    accumulator :: Int,
    finished :: Bool
  }
  deriving (Show)

main :: IO ()
main = do
  example <- readFile "../data/08_example.in"
  input <- readFile "../data/08_input.in"

  let e = doParse example
  printf "EXAMPLE Part1=%s, Part2=%s\n" (show $ run e) (show $ run2 e)
  let i = doParse input
  printf "INPUT: Part1=%s, Part2=%s\n" (show $ run i) (show $ run2 i)

  putStrLn "Day 08 end"

run2 :: [Instruction] -> State
run2 is = findCorruption 0
  where
    findCorruption x =
      let i@(Instruction newAction _) = changeAction (is !! x)
          newIs = take x is ++ [i] ++ drop (x + 1) is
          newState = runUntil newIs (\s -> pointer s >= length is)
       in case newAction of
            ACC -> findCorruption (x + 1)
            _ ->
              if finished newState
                then newState
                else findCorruption (x + 1)

    changeAction i@(Instruction action number) = case action of
      NOP -> Instruction JMP number
      JMP -> Instruction NOP number
      _ -> i

run :: [Instruction] -> State
run ins = runUntil ins (const False)

runUntil :: [Instruction] -> (State -> Bool) -> State
runUntil inst p = runUntil' (State 0 0 False) (S.fromList [])
  where
    runUntil' st seen
      | p st = st {finished = True}
      | S.member x seen = st
      | otherwise = runUntil' st' seen'
      where
        x = pointer st
        st' = runInstruction (inst !! x) st
        seen' = S.insert x seen

runInstruction :: Instruction -> State -> State
runInstruction (Instruction act num) s =
  case act of
    NOP -> s {pointer = pointer s + 1}
    ACC -> s {pointer = pointer s + 1, accumulator = accumulator s + num}
    JMP -> s {pointer = pointer s + num}

type Parser = Parsec String ()

doParse :: String -> [Instruction]
doParse = map f . filter (/= "") . lines
  where
    f line = case parse instructionParser "" line of
      Left err -> error $ printf "Error in %s with %s" line (show err)
      Right is -> is

instructionParser :: Parser Instruction
instructionParser = do
  act <-
    do
      \case
        "nop" -> NOP
        "acc" -> ACC
        "jmp" -> JMP
        _ -> error "No operation"
      <$> many letter
  _ <- space
  num <- do
    sign <- char '-' <|> char '+'
    nums <- many digit
    return (read $ if sign == '-' then sign : nums else nums)
  return (Instruction act num)