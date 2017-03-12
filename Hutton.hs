

data Expr = Val Int | Div Expr Expr
--
-- eval :: Expr -> Int
-- eval (Val n)   =  n
-- eval (Div x y) =  eval x `div` eval y

-- data Maybe a = Nothing | Just a

-- eval'::Expr -> Maybe Int
-- eval' (Val n)   =  Just n
-- eval' (Div x y) =  case eval' x of
--                     Nothing -> Nothing
--                     Just n  -> case eval' y of
--                                   Nothing -> Nothing
--                                   Just m  -> safediv n m

safediv::Int -> Int -> Maybe Int
safediv n m =  if m == 0 then Nothing else Just (n `div` m)

seqn :: Maybe a -> Maybe b -> Maybe (a,b)
seqn Nothing   _        =  Nothing
seqn _         Nothing  =  Nothing
seqn (Just x)  (Just y) =  Just (x,y)

apply :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing  =  Nothing
apply f (Just x) =  f x


eval (Val n)   = Just n
eval (Div x y) = apply f (eval x `seqn` eval y)
                   where f (n,m) = safediv n m



main = do
    print $ eval $ Div (Val 3) (Val 1)
