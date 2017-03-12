{-# LANGUAGE TemplateHaskell #-}

import           Control.Applicative
import Control.Lens hiding (element)


data MaybeT a = JustT a | NothingT

instance  Functor MaybeT  where
    fmap _ NothingT       = NothingT
    fmap f (JustT a)      = JustT (f a)



instance Applicative MaybeT where
    pure = JustT

    JustT f  <*> m       = fmap f m
    NothingT <*> _m      = NothingT

    JustT _m1 *> m2      = m2
    NothingT  *> _m2     = NothingT

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
--
-- _2 :: Lens (c, a) (c, b) a b
-- _2 f (c, a) = (,) c <$> f a
--
-- view :: ((a -> Const a b) -> s -> Const a t) -> s -> a
-- view l s = getConst (l Const s)
data Atom = Atom { _element :: String, _point :: Point, _ax::(String, String) } deriving (Show)
data Point = Point { _x :: Double, _y :: Double } deriving (Show)

makeLenses ''Atom
makeLenses ''Point

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 }, _ax=("S", "F") }

main = do
  putStrLn $ show $ atom^.ax^._2
  -- putStrLn $ show $ shiftAtomX atom
