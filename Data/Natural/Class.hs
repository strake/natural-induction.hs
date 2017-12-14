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

reify :: Natural n => Proxy n -> Peano
reify (Proxy :: Proxy n) = getConst $ natural @n (Const Zero) (φ $ Const . Succ . reify)
  where φ = ($ Proxy) :: ∀ f a . (Proxy a -> f a) -> f a
