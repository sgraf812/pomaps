{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.POMap.Divisibility where

import           Algebra.PartialOrd
import           Control.Arrow      ((&&&))

newtype Divisibility
  = Div { unDiv :: Integer }
  deriving (Eq, Num, Show, Read)

instance PartialOrd Divisibility where
  leq (Div a) (Div b) = b `mod` a == 0

divisibility :: Int -> [(Divisibility, Integer)]
divisibility n = map ((Div &&& id) . fromIntegral) [1..n]
