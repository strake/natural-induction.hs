module Data.Natural.Class where

import Data.Peano

class Natural n where
    natural :: f Zero -> (∀ m . Natural m => f (Succ m)) -> f n

instance Natural Zero where
    natural zf _ = zf

instance Natural n => Natural (Succ n) where
    natural _ sf = sf
