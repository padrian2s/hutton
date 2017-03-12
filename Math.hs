

import           Control.Applicative
import           Control.Monad                   (liftM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Writer.Lazy
import           System.Environment

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got num: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a + b)

tick :: State Int Int
tick = do n <- get
          put (n+1)
          return n

h :: Maybe Integer
h = (+) <$> Just 4 <*> Just 9

plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x

printEnv e = case e of
              Nothing -> ""
              Just r  -> r

main :: IO ()
main = lookupEnv "PATH" >>= print




---
