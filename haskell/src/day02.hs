import Data.Text (Text)
import Data.Maybe

data Password = Password {
    min :: Int,
    max :: Int,
    char :: Char,
    pwd :: String
}

main :: IO ()
main = do
    putStrLn "End of day 02"

-- 1-3 a: abcdef
parse :: Text -> [Password]
parse txt = 
  let 
    parseNum
  in undefined
