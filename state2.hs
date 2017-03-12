import           Control.Monad.Trans.State

tick :: State Int Int
tick = do n <- get
          put (n+1)
          modify (+1)
          modify (+1)
          modify (+1)

          return n


plusOne :: Int -> Int
plusOne = execState tick


main :: IO ()
main = do
  print $ plusOne 1
