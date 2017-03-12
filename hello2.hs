
type CustomerID = Int
type ReviewBody = String

data BookInfo = Book Int String
                deriving (Show)

data BetterReview = BetterReview BookInfo CustomerID ReviewBody
                    deriving (Show)
f :: BookInfo -> IO ()
f a = putStrLn $ show a

f2 :: BetterReview -> IO ()
f2 a = putStrLn $ show a

bookID  (Book id title) = id

p :: BetterReview -> BookInfo
p (BetterReview bi cID rB) = bi

main = do
      a <- return $ Book 324 "####"
      b <- return $ BetterReview a 3 "D"
      f2 b
