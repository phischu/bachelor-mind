module Main where

import Graphics.Gloss (
  play, Display(InWindow),
  Picture, pictures, translate, scale,
  circleSolid, circle, blank, text,
  white, black, red, green, blue, orange, magenta, yellow, makeColor)
import qualified Graphics.Gloss as Gloss (
  Color, color)
import Graphics.Gloss.Interface.Pure.Game (
  Event(EventKey), Key(Char), KeyState(Down))

import Data.MultiSet (
  fromList, intersection, size)

import System.Random (
  StdGen, newStdGen, randomRs)

import Data.Maybe (
  fromJust)


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
  [c1, c2, c3, c4] = map (fromJust . characterColor) input

characterColor :: Char -> Maybe Color
characterColor 'r' = Just Red
characterColor 'g' = Just Green
characterColor 'b' = Just Blue
characterColor 'o' = Just Orange
characterColor 'm' = Just Magenta
characterColor 'y' = Just Yellow
characterColor _ = Nothing

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

data GameState =
  Running [Color] [(Guess, Answer)] |
  Over [(Guess, Answer)]

window :: Display
window = InWindow "Bachelor Mind" (600, 600) (0, 0)

initialState :: GameState
initialState = Running [] []

render :: GameState -> Picture
render gameState =
  translate (negate 200) (negate 200) (renderGameState gameState)

renderGameState :: GameState -> Picture
renderGameState (Running current rounds) =
  vertical 50 (renderCurrent current : map renderRound rounds)
renderGameState (Over rounds) =
  vertical 50 (scale 0.2 0.2 (text "you win") : map renderRound rounds)

renderCurrent :: [Color] -> Picture
renderCurrent current =
  horizontal 50 (take 4 (map renderColor current ++ repeat (circle 20)))

renderRound :: (Guess, Answer) -> Picture
renderRound (guess, answer) =
  horizontal 200 [renderGuess guess, renderAnswer answer]

renderGuess :: Guess -> Picture
renderGuess guess = horizontal 50 (map renderColor (guessToList guess))

renderAnswer :: Answer -> Picture
renderAnswer [a1, a2, a3, a4] = pictures [
  translate (negate d) d (renderAnswerPin a1),
  translate d d (renderAnswerPin a2),
  translate (negate d) (negate d) (renderAnswerPin a3),
  translate d (negate d) (renderAnswerPin a4)] where
    d = 12

renderColor :: Color -> Picture
renderColor color = Gloss.color (glossColor color) (circleSolid 20)

renderAnswerPin :: AnswerPin -> Picture
renderAnswerPin Black = Gloss.color black (circleSolid 10)
renderAnswerPin White = circle 10
renderAnswerPin Nope = blank

horizontal :: Float -> [Picture] -> Picture
horizontal offset pics =
  pictures (zipWith (uncurry translate) positions pics) where
    positions = zip [0, offset ..] [0, 0 ..]

vertical :: Float -> [Picture] -> Picture
vertical offset pics =
  pictures (zipWith (uncurry translate) positions pics) where
    positions = zip [0, 0 ..] [0, offset ..]

glossColor :: Color -> Gloss.Color
glossColor color = case color of
  Red -> red
  Green -> green
  Blue -> blue
  Orange -> makeColor 1 0.5 0 1
  Magenta -> magenta
  Yellow -> yellow

handle :: Event -> GameState -> GameState
handle (EventKey (Char characterKey) Down _ _) (Running current rounds) =
  case characterColor characterKey of
    Nothing -> Running current rounds
    Just color -> Running (current ++ [color]) rounds
handle _ gameState =
  gameState

update :: Float -> GameState -> GameState
update _ (Running [c1, c2, c3, c4] rounds) =
  if guess == secret
    then Over ((guess, answer) : rounds)
    else Running [] ((guess, answer) : rounds) where
      guess = Guess c1 c2 c3 c4
      answer = feedbackAnswer (secretGuessFeedback secret guess)
update _ gameState = gameState

main :: IO ()
main = play window white 25 initialState render handle update

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


