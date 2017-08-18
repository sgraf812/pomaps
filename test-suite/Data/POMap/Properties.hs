module Data.POMap.Properties where

import           Algebra.PartialOrd
import           Control.Monad           (guard)
import           Data.POMap.Divisibility
import           Data.POMap.Lazy
import           Prelude                 hiding (lookup, max, null)
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

default (Integer)

div' :: Int -> POMap Divisibility Integer
div' = fromList . divisibility

div100 :: POMap Divisibility Integer
div100 = div' 100

div1000 :: POMap Divisibility Integer
div1000 = div' 1000

spec :: Spec
spec =
  describe "POMap" $ do
    describe "empty" $ do
      it "fromList []" $ fromList (divisibility 0) `shouldBe` empty
      it "is null" $ null empty `shouldBe` True
      it "has size 0" $ size empty `shouldBe` 0
    describe "singleton" $ do
      let m = singleton 1 1
      it "fromList [(k, v)]" $ fromList (divisibility 1) `shouldBe` m
      it "is not null" $ null m `shouldBe` False
      it "has size 1" $ size m `shouldBe` 1
    describe "width" $ do
      it "width empty == 0" $ width empty `shouldBe` 0
      it "width singleton == 1" $ width (singleton 1 1) `shouldBe` 1
      it "width div100 == 50" $ width div100 `shouldBe` 50
      it "width div1000 == 500" $ width div1000 `shouldBe` 500

    let prop100and1000 prop = do
          it "100 divs" $ property (prop div100 100)
          it "1000 divs" $ property (prop div1000 1000)

    describe "member" $
      prop100and1000 $ \m max (Positive n) ->
        member (Div n) m == (n <= max)
    describe "lookup" $
      prop100and1000 $ \m max (Positive n) ->
        lookup (Div n) m == (guard (n <= max) >> Just n)

    let lookupXProps what lu p =
          describe ("is " ++ what) $
            prop100and1000 $ \m _ (Positive n) ->
              all (p (Div n) . fst) (lu (Div n) m)

    describe "lookupLT" $ do
      it "nothing less than 1" $
        lookupLT 1 div100 `shouldBe` []
      it "1 is less than 2" $
        lookupLT 2 div100 `shouldBe` [(1, 1)]
      it "64 is less than 128" $
        lookupLT 128 div100 `shouldBe` [(64, 64)]
      lookupXProps "less than" lookupLT $ \a b ->
        not (a `leq` b) && b `leq` a
    describe "lookupLE" $ do
      it "50 leq 50" $
        lookupLE 50 div100 `shouldBe` [(50, 50)]
      it "64 is less equal 128" $
        lookupLE 128 div100 `shouldBe` [(64, 64)]
      lookupXProps "less equal" lookupLE (flip leq)
    describe "lookupGE" $ do
      lookupXProps "greater equal" lookupGE leq
    describe "lookupGT" $ do
      lookupXProps "greater than" lookupGT $ \a b ->
        a `leq` b && not (b `leq` a)



