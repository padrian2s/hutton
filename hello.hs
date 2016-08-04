


import           Control.Monad.Trans.Writer.Lazy

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)


main = do
        -- putStrLn "D"
        -- putStrLn $ show $ f 1 2
        -- putStrLn $ show $ p "DDDD"
        putStrLn $ show $ runWriter multWithLog
