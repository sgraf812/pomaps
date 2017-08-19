module Data.POMap.Properties where

import           Algebra.PartialOrd
import           Control.Arrow           ((&&&))
import           Control.Monad           (guard)
import           Data.Function           (on)
import           Data.List               (sortBy)
import           Data.Ord                (comparing)
import           Data.POMap.Arbitrary    ()
import           Data.POMap.Divisibility
import           Data.POMap.Lazy
import           Prelude                 hiding (lookup, max, null)
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

div' :: Int -> POMap Divisibility Integer
div' = fromList . divisibility

div100 :: POMap Divisibility Integer
div100 = div' 100

div1000 :: POMap Divisibility Integer
div1000 = div' 1000

primes :: [Integer]
primes = 2 : [ p | p <- [3..], not . any (divides p) . takeWhile (\n -> n*n <= p) $ primes]
  where
    divides p n = p `mod` n == 0

primesUntil :: Integer -> [Integer]
primesUntil n = takeWhile (<= n) primes

makeEntries :: [Integer] -> [(Divisibility, Integer)]
makeEntries = fmap (Div &&& id)

shouldBeSameEntries :: [(Divisibility, Integer)] -> [(Divisibility, Integer)] -> Expectation
shouldBeSameEntries = shouldBe `on` sortBy (comparing (unDiv . fst))

shouldBePOMap :: POMap Divisibility Integer -> POMap Divisibility Integer -> Expectation
shouldBePOMap a b = toList a `shouldBeSameEntries` toList b

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
      it "width singleton == 1" $ width (singleton () ()) `shouldBe` 1
      it "width div100 == 50" $ width div100 `shouldBe` 50
      it "width div1000 == 500" $ width div1000 `shouldBe` 500

    let prop100and1000 prop = do
          it "100 divs" $ property (prop div100 (100 :: Integer))
          it "1000 divs" $ property (prop div1000 (1000 :: Integer))

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
        lookupLT 2 div100 `shouldBe` makeEntries [1]
      it "64 is less than 128" $
        lookupLT 128 div100 `shouldBe` makeEntries [64]
      it "[6, 10, 15] less than 30" $
        lookupLT 30 div100 `shouldBeSameEntries` makeEntries [6, 10, 15]
      lookupXProps "less than" lookupLT $ \a b ->
        not (a `leq` b) && b `leq` a
    describe "lookupLE" $ do
      it "50 leq 50" $
        lookupLE 50 div100 `shouldBe` makeEntries [50]
      it "64 is less equal 128" $
        lookupLE 128 div100 `shouldBe` makeEntries [64]
      it "[30, 42, 70] leq 210" $
        lookupLE 210 div100 `shouldBeSameEntries` makeEntries [30, 42, 70]
      lookupXProps "less equal" lookupLE (flip leq)
    describe "lookupGE" $ do
      it "50 geq 50" $
        lookupGE 50 div100 `shouldBe` makeEntries [50]
      it "Nothing is geq 101" $
        lookupGE 101 div100 `shouldBe` makeEntries []
    describe "lookupGT" $ do
      it "primes are gt 1" $
        lookupGT 1 div100 `shouldBeSameEntries` makeEntries (primesUntil 100)
      it "Nothing is gt 101" $
        lookupGT 101 div100 `shouldBe` makeEntries []
      it "[66, 99] gt 33" $
        lookupGT 33 div100 `shouldBeSameEntries` makeEntries [66, 99]
      lookupXProps "greater than" lookupGT $ \a b ->
        a `leq` b && not (b `leq` a)

    -- `insert` should already be tested thoroughly because of `fromList`
    describe "insert" $
      it "overwrites an entry" $
        property $ \m k v ->
          lookup (k :: Divisibility) (insert k v m) `shouldBe` Just (v :: Int)
    describe "insertWithKey" $ do
      it "can access old value" $
        insertWithKey (\_ _ old -> old) 1 2 div100 `shouldBePOMap` div100
      it "can access new value" $
        lookup 1 (insertWithKey (\_ new _ -> new) 1 2 div100) `shouldBe` Just 2
      it "can access key" $
        lookup 1 (insertWithKey (\k _ _ -> unDiv k + 2) 1 2 div100) `shouldBe` Just 3
      it "adds new values without consulting the function" $
        lookup 1 (insertWithKey (\_ _ _ -> 3) (Div 1) 2 empty) `shouldBe` Just (2 :: Integer)
    describe "insertLookupWithKey" $ do
      let f k new old = unDiv k + new + old
      it "lookup &&& insertWithKey" $
        property $ \m k v ->
          insertLookupWithKey f k v m `shouldBe` (lookup k m, insertWithKey f k v m)
