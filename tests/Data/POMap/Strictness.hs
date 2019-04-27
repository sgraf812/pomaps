{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
module Data.POMap.Strictness where

import           Data.Function                (on)
import           Data.Functor.Identity
import qualified Data.List                    as List
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
  (==) = (==) `on` List.sortOn (unDiv . fst) . toList

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
      let insertAt impl k v = impl (const (Identity (Just v))) k
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
    describe "mapMaybe" $ do
      let templ impl f = impl (Just . f)
      mapTemplate (templ L.mapMaybe) (templ S.mapMaybe)
    describe "mapMaybeWithKey" $ do
      let templ impl f = impl (\_ v -> Just (f v))
      mapTemplate (templ L.mapMaybeWithKey) (templ S.mapMaybeWithKey)
    describe "mapEither" $ do
      let templ impl f = fst . impl (Left . f)
      mapTemplate (templ L.mapEither) (templ S.mapEither)

    describe "traverseWithKey" $ do
      let templ impl f = impl (\ _ v -> Identity (f v))
      mapTemplate (templ L.traverseWithKey) (templ S.traverseWithKey)
    describe "traverseMaybeWithKey" $ do
      let templ impl f = impl (\ _ v -> Identity (Just (f v)))
      mapTemplate (templ L.traverseMaybeWithKey) (templ S.traverseMaybeWithKey)

    let fromListTemplate l s = do
          it "strict" $ property $ \(xs :: [(Divisibility, Int)]) ->
            not (null xs) ==> shouldBeBottom (s (fmap (\ (k, _) -> (k, bottom)) xs))
          it "lazy" $ property $ \(xs :: [(Divisibility, Int)]) ->
            shouldNotBeBottom (l (fmap (\(k, _) -> (k, bottom)) xs))

    describe "fromList" $
      fromListTemplate L.fromList S.fromList
    describe "fromListWith" $
      fromListTemplate (L.fromListWith const) (S.fromListWith const)
    describe "fromListWithKey" $
      fromListTemplate (L.fromListWithKey (\_ _ v -> v)) (S.fromListWithKey (\_ _ v -> v))

    describe "type class instances" $ do
      describe "Functor" $ do
        describe "<$>" $
          it "always lazy" $ property $ \(m :: DivMap Int) ->
            shouldNotBeBottom (const bottom <$> m)
        describe "<$" $
          it "always lazy" $ property $ \(m :: DivMap Int) ->
            shouldNotBeBottom (bottom <$ m)
      describe "Traversable" $
        describe "traverse" $
          it "always lazy" $ property $ \(m :: DivMap Int) ->
            shouldNotBeBottom (traverse (\_ -> Identity bottom) m)
