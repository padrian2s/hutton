
f :: () -> String
f () = let x = (Just Nothing)
       in
        case x of
         (Just _) -> "something"
         Nothing  -> "nothing"



check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x==c
          then c
          else y | (x,y) <- zip word display])

-- putStrLn $ show $ check "DDD" "D" 'D'
main = do
    putStrLn $ show $ f ()
