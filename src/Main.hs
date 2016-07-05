module Main where

import Data.MultiSet (fromList, intersection, size)

import System.Random (StdGen, newStdGen, randomRs)

data Color =
  Red |
  Green |
  Blue |
  Orange |
  Magenta |
  Yellow
    deriving (Show, Eq, Ord, Enum)

data Guess = Guess Color Color Color Color
  deriving (Show, Eq, Ord)

guessToList :: Guess -> [Color]
guessToList (Guess g1 g2 g3 g4) = [g1, g2, g3, g4]

type Secret = Guess

data Feedback = Feedback Int Int
  deriving (Show, Eq, Ord)

data AnswerPin =
  Black |
  White |
  Nope
    deriving (Show, Eq, Ord)

type Answer = [AnswerPin]

randomSecret :: StdGen -> Secret
randomSecret g = Guess c1 c2 c3 c4 where
  [c1, c2, c3, c4] = map randomColor (take 4 (randomRs (0, 5) g))

randomColor :: Int -> Color
randomColor = toEnum

secret :: Secret
secret = Guess Red Orange Green Blue

inputGuess :: String -> Guess
inputGuess input = Guess c1 c2 c3 c4 where
  [c1, c2, c3, c4] = map characterColor input

characterColor :: Char -> Color
characterColor 'r' = Red
characterColor 'g' = Green
characterColor 'b' = Blue
characterColor 'o' = Orange
characterColor 'm' = Magenta
characterColor 'y' = Yellow

secretGuessFeedback :: Secret -> Guess -> Feedback
secretGuessFeedback secret guess =
  Feedback correctPosition correctColor where
    correctPosition = length (filter id (zipWith (==) secrets guesses))
    correctColor = size (intersection (fromList secrets) (fromList guesses))
    secrets = guessToList secret
    guesses = guessToList guess

feedbackAnswer :: Feedback -> Answer
feedbackAnswer (Feedback correctPosition correctColor) =
  replicate correctPosition Black ++
  replicate (correctColor - correctPosition) White ++
  replicate (4 - correctPosition - (correctColor - correctPosition)) Nope

answerString :: Answer -> String
answerString = unwords . map colorString

colorString :: AnswerPin -> String
colorString Black = "black"
colorString White = "white"
colorString Nope = "nope"

main :: IO ()
main = do
  generator <- newStdGen
  let secret = randomSecret generator
  mainLoop secret

mainLoop :: Secret -> IO ()
mainLoop secret = do
  putStrLn "Your guess: "
  input <- getLine
  let guess = inputGuess input
      feedback = secretGuessFeedback secret guess
      answer = feedbackAnswer feedback
  putStrLn (answerString answer)
  case guess == secret of
    False -> mainLoop secret
    True -> putStrLn "You win!"


