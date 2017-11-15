{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.POMap.Divisibility where

import           Algebra.PartialOrd
import           Control.Arrow         ((&&&))
import           Test.Tasty.QuickCheck

newtype Divisibility
  = Div { unDiv :: Integer }
  deriving (Eq, Num, Show, Read)

instance PartialOrd Divisibility where
  leq (Div a) (Div b) = b `mod` a == 0

instance Arbitrary Divisibility where
  arbitrary = Div . getPositive <$> arbitrary
  shrink = fmap (Div . getPositive) . shrink . Positive . unDiv

divisibility :: Int -> [(Divisibility, Integer)]
divisibility n = map ((Div &&& id) . fromIntegral) [1..n]
