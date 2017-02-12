module Unique where

import Control.Monad.State

run :: IO ()
run = evalStateT printTwo [42..]

printTwo :: StateT [Integer] IO ()
printTwo = do
  x <- pop
  liftIO $ print x
  y <- pop
  liftIO $ print y

pop :: StateT [a] IO a
pop = do
  (x:xs) <- get
  put xs
  return x
