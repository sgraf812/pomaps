{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.POMap.Arbitrary where

import           Algebra.PartialOrd
import           Data.POMap.Strict
import           Test.Tasty.QuickCheck

instance (PartialOrd k, Arbitrary k, Arbitrary v) => Arbitrary (POMap k v) where
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . shrink . toList
