module Data.Natural.Class where

import Data.Functor.Const
import Data.Peano
import Data.Proxy

class Natural n where
    natural :: f Zero -> (∀ m . Natural m => f (Succ m)) -> f n

instance Natural Zero where
    natural zf _ = zf

instance Natural n => Natural (Succ n) where
    natural _ sf = sf

reify :: Natural n => Const Peano n
reify = natural (Const Zero) (succ' reify)
  where succ' = Const . Succ . getConst :: Const Peano n -> Const Peano (Succ n)
