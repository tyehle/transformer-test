module Game where

import System.Random
import Control.Monad.State
import Text.Read (readMaybe)

run :: IO ()
run = do
  putStrLn "Secret number is between 0 and 100"
  secret <- getStdRandom (randomR (0, 100))
  guesses <- execStateT (play secret) 0
  putStrLn $ "Got it in " ++ show guesses

play :: Integer -> StateT Integer IO ()
play secret = do
  guess <- lift getGuess
  modify (+1)
  case compare guess secret of
    LT -> lift (putStrLn "Too low") >> play secret
    GT -> lift (putStrLn "Too high") >> play secret
    EQ -> return ()
  where
    getGuess = do
      input <- readMaybe <$> getLine
      maybe (putStrLn "Enter a number" >> getGuess) return input
