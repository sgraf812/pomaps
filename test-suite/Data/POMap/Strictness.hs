{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
module Data.POMap.Strictness where

import           Data.Function                (on)
import           Data.Functor.Identity
import qualified Data.List                    as List
import qualified Data.Map.Strict              as SMap
import           Data.Ord                     (comparing)
import           Data.POMap.Arbitrary         ()
import           Data.POMap.Divisibility
import qualified Data.POMap.Lazy              as L
import qualified Data.POMap.Strict            as S
import           GHC.Exts                     (toList)
import           Test.ChasingBottoms.IsBottom
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

type DivMap v = L.POMap Divisibility v

instance {-# OVERLAPPING #-} Eq v => Eq (DivMap v) where
  (==) = (==) `on` List.sortBy (comparing (unDiv . fst)) . toList

shouldBeBottom :: a -> Expectation
shouldBeBottom x = isBottom x `shouldBe` True

shouldNotBeBottom :: a -> Expectation
shouldNotBeBottom x = isBottom x `shouldBe` False

spec :: Spec
spec =
  describe "POMap" $ do
    describe "singleton" $ do
      it "strict" $ shouldBeBottom (S.singleton (Div 1) bottom)
      it "lazy" $ shouldNotBeBottom (L.singleton (Div 1) bottom)

    describe "member" $
      it "strict in the key" $ shouldBeBottom (L.member (Div bottom) L.empty)
    describe "lookup" $
      it "strict in the key" $ shouldBeBottom (L.lookup (Div bottom) L.empty)
    describe "lookupLT" $
      it "strict in the key" $ shouldBeBottom (L.lookupLT (Div bottom) L.empty)
    describe "lookupLE" $
      it "strict in the key" $ shouldBeBottom (L.lookupLE (Div bottom) L.empty)
    describe "lookupGT" $
      it "strict in the key" $ shouldBeBottom (L.lookupGT (Div bottom) L.empty)
    describe "lookupGE" $
      it "strict in the key" $ shouldBeBottom (L.lookupGE (Div bottom) L.empty)

    let insertTemplate l s = do
          it "strict in the key" $ property $ \(m :: DivMap Int) ->
            shouldBeBottom (l (Div bottom) 0 m)
          it "strict" $ property $ \(m :: DivMap Int) ->
            shouldBeBottom (s (Div 1) bottom m)
          it "lazy" $ property $ \(m :: DivMap Int) ->
            shouldNotBeBottom (l (Div 1) bottom m)

    describe "insert" $
      insertTemplate L.insert S.insert
    describe "insertWithKey" $
      insertTemplate (L.insertWithKey (\_ new _ -> new)) (S.insertWithKey (\_ new _ -> new))
    describe "insertLookupWithKey" $ do
      let templ impl k v m = snd (impl (\_ new _ -> new) k v m)
      insertTemplate (templ L.insertLookupWithKey) (templ S.insertLookupWithKey)

    describe "delete" $
      it "strict in the key" $ property $ \(m :: DivMap Int) ->
        shouldBeBottom (L.delete (Div bottom) m)
    describe "deleteLookup" $
      it "strict in the key" $ property $ \(m :: DivMap Int) ->
        shouldBeBottom (L.deleteLookup (Div bottom) m)

    let adjustTemplate l s = do
          it "strict in the key" $ property $ \(m :: DivMap Int) ->
            shouldBeBottom (l (const 0) (Div bottom) m)
          it "strict" $
            shouldBeBottom (s (const bottom) (Div 1) (L.singleton (Div 1) 1))
          it "lazy" $ property $ \(m :: DivMap Int) ->
            shouldNotBeBottom (l (const bottom) (Div 1) m)
    let ignoreKey impl f = impl (const f)

    describe "adjust" $
      adjustTemplate L.adjust S.adjust
    describe "adjustWithKey" $
      adjustTemplate (ignoreKey L.adjustWithKey) (ignoreKey S.adjustWithKey)
    describe "adjustLookupWithKey" $ do
      let templ impl f k m = snd (ignoreKey impl f k m)
      adjustTemplate (templ L.adjustLookupWithKey) (templ S.adjustLookupWithKey)

    let updateTemplate l s = adjustTemplate (\f -> l (Just . f)) (\f -> s (Just . f))

    describe "update" $
      updateTemplate L.update S.update
    describe "updateWithKey" $
      updateTemplate (ignoreKey L.updateWithKey) (ignoreKey S.updateWithKey)
    describe "updateLookupWithKey" $ do
      let templ impl f k m = snd (ignoreKey impl f k m)
      updateTemplate (templ L.updateLookupWithKey) (templ S.updateLookupWithKey)

    describe "alter" $
      updateTemplate L.alter S.alter
    describe "alterWithKey" $
      updateTemplate (ignoreKey L.alterWithKey) (ignoreKey S.alterWithKey)
    describe "alterLookupWithKey" $ do
      let templ impl f k m = snd (ignoreKey impl f k m)
      updateTemplate (templ L.alterLookupWithKey) (templ S.alterLookupWithKey)
    describe "alterF" $ do
      let insertAt impl k v m = runIdentity (impl (const (Identity (Just v))) k m)
      insertTemplate (insertAt L.alterF) (insertAt S.alterF)

    let mapTemplate l s = do
          it "strict" $ property $ \(m :: DivMap Int) ->
            not (null m) ==> shouldBeBottom (s (const bottom) m)
          it "lazy" $ property $ \(m :: DivMap Int) ->
            shouldNotBeBottom (l (const bottom) m)

    describe "map" $
      mapTemplate L.map S.map
    describe "mapWithKey" $
      mapTemplate (ignoreKey L.mapWithKey) (ignoreKey S.mapWithKey)
    describe "mapAccum" $ do
      let templ impl f m = snd (impl (const f) undefined m)
      mapTemplate (templ L.mapAccum) (templ S.mapAccum)
    describe "mapAccumWithKey" $ do
      let templ impl f m = snd (impl (\_ _ -> f) undefined m)
      mapTemplate (templ L.mapAccumWithKey) (templ S.mapAccumWithKey)
    describe "mapKeysWith" $ do
      it "strict" $ property $ \(m :: DivMap Int) ->
        length m > 1 ==> shouldBeBottom (S.mapKeysWith (\_ _ -> bottom) (const (Div 1)) m)
      it "lazy" $ property $ \(m :: DivMap Int) ->
        shouldNotBeBottom (L.mapKeysWith (\_ _ -> bottom) (const (Div 1)) m)

    describe "Functor" $ do
      describe "fmap" $
        it "always lazy" $ property $ \(m :: DivMap Int) ->
          shouldNotBeBottom (const bottom <$> m)
      describe "<$" $
        it "always lazy" $ property $ \(m :: DivMap Int) ->
          shouldNotBeBottom (bottom <$ m)

    describe "traverseWithKey" $ do
      let templ impl f m = runIdentity (impl (\_ v -> Identity (f v)) m)
      mapTemplate (templ L.traverseWithKey) (templ S.traverseWithKey)
    describe "traverseMaybeWithKey" $ do
      let templ impl f m = runIdentity (impl (\_ v -> Identity (Just (f v))) m)
      mapTemplate (templ L.traverseMaybeWithKey) (templ S.traverseMaybeWithKey)
{-
    describe "foldrWithKey" $ do
      it "foldrWithKey (const f) = foldr f" $ property $ \(m :: DivMap Int) ->
        foldrWithKey (const (-)) 0 m `shouldBe` foldr (-) 0 m
      let f k a b = unDiv k + a + b
      it "foldrWithKey f z = foldr (uncurry f) z . mapWithKey (,)" $ property $ \(m :: DivMap Integer) ->
        foldrWithKey f 0 m `shouldBe` foldr (uncurry f) 0 (mapWithKey (,) m)
    describe "foldlWithKey" $ do
      it "foldlWithKey (\a _ b -> f a b) = foldl f" $ property $ \(m :: DivMap Int) ->
        foldlWithKey (\a _ b -> a - b) 0 m `shouldBe` foldl (-) 0 m
      let f a k b = unDiv k + a + b
      it "foldlWithKey f z = foldl (\a (k, b) -> f a k b) z . mapWithKey (,)" $ property $ \(m :: DivMap Integer) ->
        foldlWithKey f 0 m `shouldBe` foldl (\a (k, b) -> f a k b) 0 (mapWithKey (,) m)
    describe "foldMapWithKey" $ do
      it "foldMapWithKey (const f) = foldMap f" $ property $ \(m :: DivMap Int) ->
        foldMapWithKey (const Sum) m `shouldBe` foldMap Sum m

    describe "foldr'" $
      it "foldr' = foldr" $ property $ \(m :: DivMap Int) ->
        foldr' (-) 0 m `shouldBe` foldr (-) 0 m
    describe "foldrWithKey'" $ do
      let f k a b = unDiv k + a + b
      it "foldrWithKey' = foldrWithKey" $ property $ \(m :: DivMap Integer) ->
        foldrWithKey' f 0 m `shouldBe` foldrWithKey f 0 m
    describe "foldl'" $
      it "foldl' = foldl" $ property $ \(m :: DivMap Int) ->
        foldl' (-) 0 m `shouldBe` foldl (-) 0 m
    describe "foldlWithKey'" $ do
      let f a k b = unDiv k + a + b
      it "foldlWithKey' = foldlWithKey" $ property $ \(m :: DivMap Integer) ->
        foldlWithKey' f 0 m `shouldBe` foldlWithKey f 0 m

    describe "keys" $ do
      it "length . keys = size" $ property $ \(m :: DivMap Int) ->
        length (keys m) `shouldBe` size m
      it "all (\\k -> member k m) (keys m)" $ property $ \(m :: DivMap Int) ->
        all (`member` m) (keys m) `shouldBe` True
    describe "elems" $
      it "foldMap Sum . elems = foldMap Sum" $ property $ \(m :: DivMap Int) ->
        foldMap Sum (elems m) `shouldBe` foldMap Sum m
    describe "assocs" $ do
      it "length . assocs = size" $ property $ \(m :: DivMap Int) ->
        length (assocs m) `shouldBe` size m
      it "List.lookup k (assocs m) = lookup k m" $ property $ \(m :: DivMap Int) k ->
        List.lookup k (assocs m) `shouldBe` lookup k m

    describe "toList" $ do
      it "length . toList = size" $ property $ \(m :: DivMap Int) ->
        length (toList m) `shouldBe` size m
      it "List.lookup k (toList m) = lookup k m" $ property $ \(m :: DivMap Int) k ->
        List.lookup k (toList m) `shouldBe` lookup k m
    describe "fromList" $
      it "fromList = foldr (uncurry insert) empty" $ property $ \(xs :: [(Divisibility, Int)]) ->
        fromList xs `shouldBe` foldr (uncurry insert) empty xs
    describe "fromListWith" $ do
      it "fromListWith const = fromList" $ property $ \(xs :: [(Divisibility, Int)]) ->
        fromListWith const xs `shouldBe` fromList xs
      let f old new = old + new
      it "fromListWith f = fromListWithKey (const f)" $ property $ \(xs :: [(Divisibility, Int)]) ->
        fromListWith f xs `shouldBe` fromListWithKey (const f) xs
      it "fromListWith f = foldr (uncurry (insertWith f)) empty" $ property $ \(xs :: [(Divisibility, Int)]) ->
        fromListWith f xs `shouldBe` foldr (uncurry (insertWith f)) empty xs
    describe "fromListWithKey" $ do
      let f k old new = unDiv k + old + new
      it "fromListWithKey f = foldr (uncurry (insertWithKey f)) empty" $ property $ \(xs :: [(Divisibility, Integer)]) ->
        fromListWithKey f xs `shouldBe` foldr (uncurry (insertWithKey f)) empty xs

    describe "filter" $
      it "filter p = fromList . filter (p . snd) . toList" $ property $ \(m :: DivMap Int) ->
        filter odd m `shouldBe` fromList (List.filter (odd . snd) (toList m))
    describe "filterWithKey" $ do
      let p k v = odd (unDiv k + v)
      it "filterWithKey p = fromList . filter (uncurry p) . toList" $ property $ \(m :: DivMap Integer) ->
        filterWithKey p m `shouldBe` fromList (List.filter (uncurry p) (toList m))
    describe "partition" $
      it "partition p = filter p &&& filter even" $ property $ \(m :: DivMap Int) ->
        partition odd m `shouldBe` (filter odd &&& filter even) m
    describe "partitionWithKey" $ do
      let p k v = odd (unDiv k + v)
      it "partitionWithKey p = filterWithKey p &&& filterWithKey ((not .) . p)" $ property $ \(m :: DivMap Integer) ->
        partitionWithKey p m `shouldBe` (filterWithKey p &&& filterWithKey ((not .) . p)) m
    describe "mapMaybe" $ do
      let f v = if odd v then Just (v + 1) else Nothing
      it "mapMaybe f = fromList . Maybe.mapMaybe (traverse f) . toList" $ property $ \(m :: DivMap Int) ->
        mapMaybe f m `shouldBe` fromList (Maybe.mapMaybe (traverse f) (toList m))
    describe "mapMaybeWithKey" $ do
      let f k v = if odd (unDiv k + v) then Just (v + 1) else Nothing
      it "mapMaybeWithKey f = fromList . Maybe.mapMaybe (sequenceA . (fst &&& uncurry f)) . toList" $ property $ \(m :: DivMap Integer) ->
        mapMaybeWithKey f m `shouldBe` fromList (Maybe.mapMaybe (sequenceA . (fst &&& uncurry f)) (toList m))
    describe "mapEither" $ do
      let f v
            | odd v = Left (v + 1)
            | otherwise = Right (v - 1)
      it "mapEither f = (fromList &&& fromList) . Either.partitionEithers . fmap (... f ...) . toList" $
        property $ \(m :: DivMap Int) ->
          mapEither f m `shouldBe`
            ((fromList *** fromList)
            . Either.partitionEithers
            . fmap (\(k, v) -> bimap ((,) k) ((,) k) (f v))
            . toList)
            m
    describe "mapEitherWithKey" $ do
      let f k v
            | odd (unDiv k + v) = Left (v + 1)
            | otherwise = Right (v - 1)
      it "mapEitherWithKey f = (fromList &&& fromList) . Either.partitionEithers . fmap (... f ...) . toList" $
        property $ \(m :: DivMap Integer) ->
          mapEitherWithKey f m `shouldBe`
            ((fromList *** fromList)
            . Either.partitionEithers
            . fmap (\(k, v) -> bimap ((,) k) ((,) k) (f k v))
            . toList)
            m

    describe "isSubmapOf" $ do
      it "div100 is submap of div1000" $
        div100 `isSubmapOf` div1000
      it "div1000 is not submap of div100" $
        not (div1000 `isSubmapOf` div100)
    describe "isSubmapOfBy" $ do
      it "isSubmapOfBy (<) not refl" $ property $ \(m :: DivMap Int) ->
        size m > 0 ==> not (isSubmapOfBy (<) m m)
      it "isSubmapOfBy (<) m (map (+1) m)" $ property $ \(m :: DivMap Int) ->
        isSubmapOfBy (<) m (map (+1) m)
    describe "isProperSubmapOf" $ do
      it "submap with less size" $ property $ \(m1 :: DivMap Int) m2 ->
        (m1 `isProperSubmapOf` m2) `shouldBe` (size m1 < size m2 && m1 `isSubmapOf` m2)
      it "div100 is proper submap of div1000" $
        div100 `isProperSubmapOf` div1000
      it "div1000 is not proper submap of div100" $
        not (div1000 `isSubmapOf` div100)
    describe "isProperSubmapOfBy" $
      it "not (isProperSubmapOfBy (<) m (map (+1) m))" $ property $ \(m :: DivMap Int) ->
        not (isProperSubmapOfBy (<) m (map (+1) m))

    describe "lookupMin" $ do
      it "antichain" $ property $ \(m :: DivMap Int) ->
        isAntichain (fmap fst (lookupMin m))
      let less a b = a `leq` b && not (b `leq` a)
      it "no element less" $ property $ \(m :: DivMap Int) ->
        shouldSatisfy (fmap fst (lookupMin m)) $ \mins ->
          all (\k -> not (any (`less` k) (keys m))) mins
    describe "lookupMax" $ do
      let greater a b = b `leq` a && not (a `leq` b)
      it "antichain" $ property $ \(m :: DivMap Int) ->
        isAntichain (fmap fst (lookupMax m))
      it "no element greater" $ property $ \(m :: DivMap Int) ->
        shouldSatisfy (fmap fst (lookupMax m)) $ \mins ->
          all (\k -> not (any (`greater` k) (keys m))) mins


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
          let t = Maybe.listToMaybe
          it "naturality" $ property $ \(m :: DivMap Int) ->
            t (traverse f m) `shouldBe` traverse (t . f) m
          it "identity" $ property $ \(m :: DivMap Int) ->
            traverse Identity m `shouldBe` Identity m
          it "composition" $ property $ \(m :: DivMap Int) ->
            traverse (Compose . fmap g . f) m `shouldBe` (Compose . fmap (traverse g) . traverse f) m
        describe "sequenceA" $ do
          let t = Maybe.listToMaybe
          it "naturality" $ property $ \(m :: DivMap [Int]) ->
            t (sequenceA m) `shouldBe` sequenceA (fmap t m)
          it "identity" $ property $ \(m :: DivMap Int) ->
            sequenceA (fmap Identity m) `shouldBe` Identity m
          it "composition" $ property $ \(m :: DivMap (Maybe (Maybe Int))) ->
            sequenceA (fmap Compose m) `shouldBe` (Compose . fmap sequenceA . sequenceA) m
        it "fmap = fmapDefault" $ property $ \(m :: DivMap Int) ->
          fmap (+1) m `shouldBe` fmapDefault (+1) m
        it "foldMap = foldMapDefault" $ property $ \(m :: DivMap Int) ->
          foldMap Sum m `shouldBe` foldMapDefault Sum m
-}
