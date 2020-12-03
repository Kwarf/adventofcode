import qualified Data.Algebra.Boolean          as Algebra
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

isValidFirstPartPassword a = cnt >= fst ao && cnt <= snd ao
 where
  cnt = length . filter ((letter $ fst a) ==) $ snd a
  ao  = allowedOccurrences $ fst a

isValidSeondPartPassword a = Algebra.xor
  (snd a !! (fst ao - 1) == ltr)
  (length (snd a) >= snd ao && snd a !! (snd ao - 1) == ltr)
 where
  ltr = letter $ fst a
  ao  = allowedOccurrences $ fst a

parsedToMaybe :: Either Parsec.ParseError a -> Maybe a
parsedToMaybe (Left  a) = error $ show a
parsedToMaybe (Right b) = Just b

main = do
  input <-
    mapMaybe (parsedToMaybe . Parsec.parse parser "") . lines <$> readFile
      "input.txt"

  putStr "The answer to the first part is: "
  print . length . filter isValidFirstPartPassword $ input

  putStr "The answer to the second part is: "
  print . length . filter isValidSeondPartPassword $ input
