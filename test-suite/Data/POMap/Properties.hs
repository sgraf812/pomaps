{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.POMap.Properties where

import           Algebra.PartialOrd
import           Control.Arrow           ((&&&))
import           Control.Monad           (guard)
import           Data.Foldable           hiding (toList)
import           Data.Function           (on)
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.List               (find, sortBy)
import           Data.Maybe              (listToMaybe)
import           Data.Monoid             (Dual (..), Endo (..), Sum (..))
import           Data.Ord                (comparing)
import           Data.POMap.Arbitrary    ()
import           Data.POMap.Divisibility
import           Data.POMap.Lazy
import           GHC.Exts                (coerce)
import           Prelude                 hiding (lookup, map, max, null)
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

type DivMap v = POMap Divisibility v

instance Eq v => Eq (DivMap v) where
  (==) = (==) `on` sortBy (comparing (unDiv . fst)) . toList

div' :: Int -> DivMap Integer
div' = fromList . divisibility

div100 :: DivMap Integer
div100 = div' 100

div1000 :: DivMap Integer
div1000 = div' 1000

primes :: [Integer]
primes = 2 : [ p | p <- [3..], not . any (divides p) . takeWhile (\n -> n*n <= p) $ primes]
  where
    divides p n = p `mod` n == 0

primesUntil :: Integer -> [Integer]
primesUntil n = takeWhile (<= n) primes

makeEntries :: [Integer] -> [(Divisibility, Integer)]
makeEntries = fmap (Div &&& id)

shouldBeSameEntries :: (Eq v, Show v) => [(Divisibility, v)] -> [(Divisibility, v)] -> Expectation
shouldBeSameEntries = shouldBe `on` sortBy (comparing (unDiv . fst))

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

    describe "insert" $
      it "overwrites an entry" $
        property $ \(m :: DivMap Int) k v ->
          lookup k (insert k v m) `shouldBe` Just v
    describe "insertWithKey" $ do
      it "can access old value" $
        insertWithKey (\_ _ old -> old) 1 2 div100 `shouldBe` div100
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

    describe "delete" $
      it "deletes" $ property $ \(m :: DivMap Int) k ->
        lookup k (delete k m) `shouldBe` Nothing
    describe "deleteLookup" $
      it "lookup &&& delete" $ property $ \(m :: DivMap Int) k ->
        deleteLookup k m `shouldBe` (lookup k m, delete k m)

    describe "adjust" $ do
      let f old = old + 1
      it "adjusts" $ property $ \(m :: DivMap Int) k ->
        lookup k (adjust f k m) `shouldBe` (+1) <$> lookup k m
    describe "adjustWithKey" $ do
      let f k old = unDiv k + old + 1
      it "passes the key" $ property $ \(m :: DivMap Integer) k ->
        lookup k (adjustWithKey f k m) `shouldBe` (unDiv k + 1 +) <$> lookup k m
    describe "adjustLookupWithKey" $ do
      let f k old = unDiv k + old + 1
      it "lookup &&& adjustWithKey" $ property $ \(m :: DivMap Integer) k ->
        adjustLookupWithKey f k m `shouldBe` (lookup k m, adjustWithKey f k m)

    describe "update" $ do
      it "Nothing deletes" $ property $ \(m :: DivMap Int) k ->
        lookup k (update (const Nothing) k m) `shouldBe` Nothing
      let f old = old + 1
      it "Just adjusts" $ property $ \(m :: DivMap Int) k ->
        lookup k (update (Just . f) k m) `shouldBe` lookup k (adjust f k m)
    describe "updateWithKey" $ do
      let f k old = Just (unDiv k + old + 1)
      it "passes the key" $ property $ \(m :: DivMap Integer) k ->
        lookup k (updateWithKey f k m) `shouldBe` (unDiv k + 1 +) <$> lookup k m
    describe "updateLookupWithKey" $ do
      let f k old = Just (unDiv k + old + 1)
      it "lookup &&& updateWithKey" $ property $ \(m :: DivMap Integer) k ->
        updateLookupWithKey f k m `shouldBe` (lookup k m, updateWithKey f k m)

    describe "alter" $ do
      let fJust _ = Just 4
      it "const Just inserts" $ property $ \(m :: DivMap Int) k ->
        lookup k (alter fJust k m) `shouldBe` lookup k (insert k 4 m)
      let f old = Just (old + 1)
      it "(>>=) updates" $ property $ \(m :: DivMap Int) k ->
        lookup k (alter (>>= f) k m) `shouldBe` lookup k (update f k m)
    describe "alterWithKey" $ do
      let f old = (+1) <$> old
      it "const f alters" $ property $ \(m :: DivMap Int) k ->
        lookup k (alterWithKey (const f) k m) `shouldBe` lookup k (alter f k m)
      let g k old = Just (unDiv k + old + 1)
      let g' k old = old >>= g k
      it "(>>=) updates" $ property $ \(m :: DivMap Integer) k ->
        lookup k (alterWithKey g' k m) `shouldBe` lookup k (updateWithKey g k m)
    describe "alterLookupWithKey" $ do
      let f k Nothing  = Just (unDiv k + 1)
          f _ (Just _) = Nothing
      it "lookup &&& alterWithKey" $ property $ \(m :: DivMap Integer) k ->
        alterLookupWithKey f k m `shouldBe` (lookup k m, alterWithKey f k m)
    describe "alterF" $ do
      it "Const looks up" $ property $ \(m :: DivMap Integer) k ->
        getConst (alterF Const k m) `shouldBe` lookup k m
      let f _ = Identity (Just 4)
      it "Identity inserts" $ property $ \(m :: DivMap Integer) k ->
        lookup k (runIdentity (alterF f k m)) `shouldBe` lookup k (insert k 4 m)

    describe "union" $ do
      it "domain" $ property $ \(m1 :: DivMap Integer) m2 k ->
        (member k m1 || member k m2) === member k (union m1 m2)
      it "left bias" $ property $ \(m1 :: DivMap Integer) m2 k ->
        (member k m1 && member k m2) ==> lookup k (union m1 m2) === lookup k m1
    describe "unionWith" $ do
      let left l _ = l
      it "union == unionWith left" $ property $ \(m1 :: DivMap Integer) m2 k ->
        lookup k (union m1 m2) === lookup k (unionWith left m1 m2)
      let right _ r = r
      it "can have right bias" $ property $ \(m1 :: DivMap Integer) m2 k ->
        (member k m1 && member k m2) ==> lookup k (unionWith right m1 m2) === lookup k m2
    describe "unionWithKey" $ do
      let left l _ = l
      it "unionWith f == unionWithKey (const f)" $ property $ \(m1 :: DivMap Integer) m2 k ->
        lookup k (unionWith left m1 m2) === lookup k (unionWithKey (const left) m1 m2)
      let merge k l r = unDiv k + l + r
      it "can access key" $ property $ \(m1 :: DivMap Integer) m2 k ->
        (member k m1 && member k m2) ==>
          lookup k (unionWithKey merge m1 m2) === (merge k <$> lookup k m1 <*> lookup k m2)
    describe "unions" $ do
      it "domain" $
        forAll (vectorOf 10 arbitrary) $ \(ms :: [DivMap Integer]) k ->
          any (member k) ms === member k (unions ms)
      it "left bias" $
        forAll (vectorOf 10 arbitrary) $ \(ms :: [DivMap Integer]) k ->
          lookup k (unions ms) === (find (member k) ms >>= lookup k)
    describe "unionsWith" $ do
      let left l _ = l
      it "unions = unionsWith left" $
        forAll (vectorOf 5 arbitrary) $ \(ms :: [DivMap Integer]) k ->
          any (member k) ms === member k (unionsWith left ms)
      let right _ r = r
      it "can have right bias" $
        forAll (vectorOf 5 arbitrary) $ \(ms :: [DivMap Integer]) k ->
          lookup k (unionsWith right ms) === (find (member k) (reverse ms) >>= lookup k)

    describe "difference" $
      it "domain" $ property $ \(m1 :: DivMap Integer) (m2 :: DivMap ()) k ->
        (member k m1 && member k (difference m1 m2)) ==> not (member k m2)
    describe "differenceWith" $ do
      it "difference = differenceWith (\\_ _ -> Nothing)" $ property $ \(m1 :: DivMap Integer) (m2 :: DivMap ()) k ->
        lookup k (difference m1 m2) === lookup k (differenceWith (\_ _ -> Nothing) m1 m2)
      it "m = differenceWith (\\l _ -> Just l) m _" $ property $ \(m1 :: DivMap Integer) (m2 :: DivMap ()) k ->
        lookup k m1 === lookup k (differenceWith (\l _ -> Just l) m1 m2)
    describe "differenceWithKey" $ do
      let f l r = Just (l + r)
      it "differenceWith f = differenceWithKey (const f)" $ property $ \(m1 :: DivMap Int) (m2 :: DivMap Int) k ->
        lookup k (differenceWith f m1 m2) === lookup k (differenceWithKey (const f) m1 m2)

    describe "intersection" $
      it "domain" $ property $ \(m1 :: DivMap Integer) (m2 :: DivMap ()) k ->
        (member k m1 && member k m2) === member k (intersection m1 m2)
    describe "intersectionWith" $ do
      let left l _ = l
      it "intersection = intersectionWith left" $ property $ \(m1 :: DivMap Integer) (m2 :: DivMap ()) k ->
        lookup k (intersection m1 m2) === lookup k (intersectionWith left m1 m2)
    describe "intersectionWithKey" $ do
      let f = (+)
      it "intersectionWith f = intersectionWithKey f" $ property $ \(m1 :: DivMap Int) (m2 :: DivMap Int) k ->
        lookup k (intersectionWith f m1 m2) === lookup k (intersectionWithKey (const f) m1 m2)
      let merge k l r = unDiv k + l + r
      it "can access key" $ property $ \(m1 :: DivMap Integer) m2 k ->
        (member k m1 && member k m2) ==>
          lookup k (intersectionWithKey merge m1 m2) === (merge k <$> lookup k m1 <*> lookup k m2)


    describe "map" $ do
      let f = (+1)
      it "map = fmap" $ property $ \(m :: DivMap Int) ->
        map f m `shouldBe` fmap f m
    describe "mapWithKey" $ do
      let f = (+1)
      it "mapWithKey (const f) = map f" $ property $ \(m :: DivMap Int) ->
        mapWithKey (const f) m `shouldBe` map f m
      let g k v = unDiv k + v
      it "can access keys" $ property $ \(m :: DivMap Integer) k ->
        lookup k (mapWithKey g m) `shouldBe` (unDiv k +) <$> lookup k m
    describe "traverseWithKey" $ do
      let f old = Identity (old + 1)
      it "traverseWithKey (const f) = traverse f" $ property $ \(m :: DivMap Int) ->
        runIdentity (traverseWithKey (const f) m) `shouldBe` runIdentity (traverse f m)
    describe "traverseMaybeWithKey" $ do
      let f k old = Identity (unDiv k + old + 1)
      it "traverseMaybeWithKey (\\k v -> Just <$> f k v) = traverseWithKey f" $ property $ \(m :: DivMap Integer) ->
        runIdentity (traverseMaybeWithKey (\k v -> Just <$> f k v) m)
          `shouldBe` runIdentity (traverseWithKey f m)
    describe "mapAccum" $ do
      let f a b = a + b
      let g b = b + 1
      it "fst . mapAccum (\\a b -> (f a b, b)) acc = foldr f acc" $ property $ \(m :: DivMap Integer) ->
        fst (mapAccum (\a b -> (f a b, b)) 0 m) `shouldBe` foldr f 0 m
      it "snd . mapAccum (\\a b -> (a, g b)) acc = map g" $ property $ \(m :: DivMap Integer) ->
        snd (mapAccum (\_ b -> (b, g b)) 0 m) `shouldBe` map g m
    describe "mapAccumWithKey" $ do
      let f a b = (a + b, b + 1)
      it "mapAccumWithKey (\\a _ b -> f a b) acc =  mapAccum f acc" $ property $ \(m :: DivMap Integer) ->
        mapAccumWithKey (\a _ b -> f a b) 0 m `shouldBe` mapAccum f 0 m

    describe "type class instances" $ do
      describe "Functor" $
        describe "fmap" $ do
          it "fmap id = id" $ property $ \(m :: DivMap Int) ->
            fmap id m `shouldBe` m
          let f = (+1)
          let g = (*2)
          it "fmap f . fmap g = fmap (f . g)" $ property $ \(m :: DivMap Int) ->
            fmap f (fmap g m) `shouldBe` fmap (f . g) m
          it "fmaps over all entries" $ property $ \(m :: DivMap Int) k ->
            lookup k (fmap (+1) m) `shouldBe` (+1) <$> lookup k m

      describe "Foldable" $ do
        describe "foldMap" $ do
          it "getSum (foldMap (const (Sum 1))) = size" $ property $ \(m :: DivMap Int) ->
            getSum (foldMap (const (Sum 1)) m) `shouldBe` size m
          it "foldMap f = fold . fmap f" $ property $ \(m :: DivMap Int) ->
            foldMap Sum m `shouldBe` fold (fmap Sum m)
        describe "foldr" $ do
          let f = (-)
          let z = 9000
          it "foldr f z m = appEndo (foldMap (Endo . f) m ) z" $ property $ \(m :: DivMap Int) ->
            foldr f z m `shouldBe` appEndo (foldMap (Endo . f) m ) z
        describe "foldl" $ do
          let f = (-)
          let z = 9000
          it "foldl f z m = appEndo (getDual (foldMap (Dual . Endo . flip f) m)) z" $ property $ \(m :: DivMap Int) ->
            foldl f z m `shouldBe` appEndo (getDual (foldMap (Dual . Endo . flip f) m)) z
        describe "fold" $
          it "fold = foldMap id" $ property $ \(m :: DivMap (Sum Int)) ->
            fold m `shouldBe` foldMap id m

      describe "Traversable" $ do
        describe "traverse" $ do
          it "traverse (const (Const (Sum 1))) = size" $ property $ \(m :: DivMap Int) ->
            getSum (getConst (traverse (const (Const (Sum 1))) m)) `shouldBe` size m
          let f n = replicate (min 2 n) n
          let g n = if odd n then Just n else Nothing
          let t = listToMaybe
          it "naturality" $ property $ \(m :: DivMap Int) ->
            t (traverse f m) `shouldBe` traverse (t . f) m
          it "identity" $ property $ \(m :: DivMap Int) ->
            traverse Identity m `shouldBe` Identity m
          it "composition" $ property $ \(m :: DivMap Int) ->
            traverse (Compose . fmap g . f) m `shouldBe` (Compose . fmap (traverse g) . traverse f) m
        describe "sequenceA" $ do
          let t = listToMaybe
          it "naturality" $ property $ \(m :: DivMap [Int]) ->
            t (sequenceA m) `shouldBe` sequenceA (fmap t m)
          it "identity" $ property $ \(m :: DivMap Int) ->
            sequenceA (fmap Identity m) `shouldBe` Identity m
          it "composition" $ property $ \(m :: DivMap (Maybe (Maybe Int))) ->
            sequenceA (fmap Compose m) `shouldBe` (Compose . fmap sequenceA . sequenceA) m
