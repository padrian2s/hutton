
import           Control.Monad.Trans.State

type Stack = [Integer]

pop :: Stack -> (Integer,Stack)
pop (x:xs) = (x,xs)

push :: Integer -> Stack -> ((),Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Integer, Stack)
stackManip stack = let
    ((),newStack1) = push 3 stack
    (a ,newStack2) = pop newStack1
    in pop newStack2

s :: State Stack ()
s = do
  a <- get
  put a
  put [3,3]



main = do
      -- print $ stackManip [5,8,3,2,1]
      -- print $ push 3 [5,8,3,2]
      -- print $ pop [5,8,3,2]
      print $ runState s [1]
