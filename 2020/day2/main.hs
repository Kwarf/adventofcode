import           Data.Maybe
import qualified Text.Parsec                   as Parsec

data Policy = Policy
  { allowedOccurrences :: (Int, Int)
  , letter             :: Char
  }
  deriving Show

parseInt = read <$> Parsec.many1 Parsec.digit

parser = do
  minOcc <- parseInt
  Parsec.char '-'
  maxOcc <- parseInt
  Parsec.space
  letter <- Parsec.letter
  Parsec.char ':'
  Parsec.space
  password <- Parsec.many1 Parsec.letter
  return (Policy (minOcc, maxOcc) letter, password)

isValidPassword a = cnt >= fst ao && cnt <= snd ao
 where
  cnt = length . filter ((letter $ fst a) ==) $ snd a
  ao  = allowedOccurrences $ fst a

parsedToMaybe :: Either Parsec.ParseError a -> Maybe a
parsedToMaybe (Left  a) = error $ show a
parsedToMaybe (Right b) = Just b

main = do
  input <-
    mapMaybe (parsedToMaybe . Parsec.parse parser "") . lines <$> readFile
      "input.txt"

  putStr "The answer to the first part is: "
  print . length . filter isValidPassword $ input
