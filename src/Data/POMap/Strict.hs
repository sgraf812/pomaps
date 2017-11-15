{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

-- |
-- Module      :  Data.POMap.Strict
-- Copyright   :  (c) Sebastian Graf 2017
-- License     :  MIT
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- A reasonably efficient implementation of partially ordered maps from keys to values
-- (dictionaries).
--
-- The API of this module is strict in both the keys and the values.
-- If you need value-lazy maps, use "Data.POMap.Lazy" instead.
-- The 'POMap' type is shared between the lazy and strict modules,
-- meaning that the same 'POMap' value can be passed to functions in
-- both modules (although that is rarely needed).
--
-- A consequence of this is that the 'Functor', 'Traversable' and 'Data' instances
-- are the same as for the "Data.POMap.Lazy" module, so if they are used
-- on strict maps, the resulting maps will be lazy.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- > import qualified Data.POMap.Strict as POMap
--
-- The implementation of 'POMap' is based on a decomposition of
-- chains (totally ordered submaps), inspired by
-- [\"Sorting and Selection in Posets\"](https://arxiv.org/abs/0707.1532).
--
-- Operation comments contain the operation time complexity in
-- [Big-O notation](http://en.wikipedia.org/wiki/Big_O_notation) and
-- commonly refer to two characteristics of the poset from which keys are drawn:
-- The number of elements in the map \(n\) and the /width/ \(w\) of the poset,
-- referring to the size of the biggest anti-chain (set of incomparable elements).
--
-- Generally speaking, lookup and mutation operations incur an additional
-- factor of \(\mathcal{O}(w)\) compared to their counter-parts in "Data.Map.Strict".
--
-- Note that for practical applications, the width of the poset should be
-- in the order of \(w\in \mathcal{O}(\frac{n}{\log n})\), otherwise a simple lookup list
-- is asymptotically superior.
-- Even if that holds, the constants might be too big to be useful for any \(n\) that can
-- can happen in practice.
--
-- The following examples assume the following definitions for a map on the divisibility
-- relation on `Int`egers:
--
-- @
-- {-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
--
-- import           Algebra.PartialOrd
-- import           Data.POMap.Strict (POMap)
-- import qualified Data.POMap.Strict as POMap
--
-- newtype Divisibility
--   = Div Int
--   deriving (Eq, Read, Show)
--
-- -- We want integer literals to be interpreted as 'Divisibility's!
-- default (Divisibility)
--
-- instance 'PartialOrd' Divisibility where
--   Div a \`leq\` Div b = b \`mod\` a == 0
--
-- type DivMap a = POMap Divisibility a
--
-- @
--
-- 'Divisility' is actually an example for a 'PartialOrd' that should not be used as keys of 'POMap'.
-- Its width is \(w=\frac{n}{2}\in\Omega(n)\)!

module Data.POMap.Strict (
  -- * Map type
    Impl.POMap

  -- * Query
  , null
  , Impl.size
  , Impl.width
  , Impl.member
  , Impl.notMember
  , Impl.lookup
  , Impl.findWithDefault
  , Impl.lookupLT
  , Impl.lookupGT
  , Impl.lookupLE
  , Impl.lookupGE

  -- * Construction
  , Impl.empty
  , singleton

  -- ** Insertion
  , insert
  , insertWith
  , insertWithKey
  , insertLookupWithKey

  -- ** Delete\/Update
  , Impl.delete
  , Impl.deleteLookup
  , adjust
  , adjustWithKey
  , adjustLookupWithKey
  , update
  , updateWithKey
  , updateLookupWithKey
  , alter
  , alterWithKey
  , alterLookupWithKey
  , alterF

  -- * Combine

  -- ** Union
  , Impl.union
  , Impl.unionWith
  , Impl.unionWithKey
  , Impl.unions
  , Impl.unionsWith

  -- ** Difference
  , Impl.difference
  , Impl.differenceWith
  , Impl.differenceWithKey

  -- ** Intersection
  , Impl.intersection
  , Impl.intersectionWith
  , Impl.intersectionWithKey

  -- * Traversal
  -- ** Map
  , map
  , mapWithKey
  , traverseWithKey
  , traverseMaybeWithKey
  , mapAccum
  , mapAccumWithKey
  , Impl.mapKeys
  , mapKeysWith
  , Impl.mapKeysMonotonic

  -- * Folds
  , Impl.foldrWithKey
  , Impl.foldlWithKey
  , Impl.foldMapWithKey

  -- ** Strict folds
  , Impl.foldr'
  , Impl.foldl'
  , Impl.foldrWithKey'
  , Impl.foldlWithKey'

  -- * Conversion
  , Impl.elems
  , Impl.keys
  , Impl.assocs

  -- ** Lists
  , Impl.toList
  , fromList
  , fromListWith
  , fromListWithKey

  -- * Filter
  , Impl.filter
  , Impl.filterWithKey

  , Impl.partition
  , Impl.partitionWithKey

  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey

  -- * Submap
  , Impl.isSubmapOf, Impl.isSubmapOfBy
  , Impl.isProperSubmapOf, Impl.isProperSubmapOfBy

  -- * Min\/Max
  , Impl.lookupMin
  , Impl.lookupMax
  ) where

import           Algebra.PartialOrd
import           Data.Map.Internal   (AreWeStrict (..))
import           Data.POMap.Internal (POMap (..))
import qualified Data.POMap.Internal as Impl
import           GHC.Exts            (Proxy#, proxy#)
import           Prelude             hiding (map)

-- $setup
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> import           Algebra.PartialOrd
-- >>> import           Data.POMap.Strict
-- >>> :{
--   newtype Divisibility
--     = Div Int
--     deriving (Eq, Num)
--   instance Show Divisibility where
--     show (Div a) = show a
--   instance PartialOrd Divisibility where
--     Div a `leq` Div b = b `mod` a == 0
--   type DivMap a = POMap Divisibility a
--   default (Divisibility)
-- :}

-- | \(\mathcal{O}(1)\). A map with a single element.
--
-- Examples:
--
-- >>> singleton 1 'a'
-- fromList [(1,'a')]
-- >>> size (singleton 1 'a')
-- 1
singleton :: k -> v -> POMap k v
singleton = Impl.singleton (proxy# :: Proxy# 'Strict)
{-# INLINE singleton #-}

-- | \(\mathcal{O}(w\log n)\). Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- Examples:
--
-- >>> insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3,'b'), (5,'x')]
-- True
-- >>> insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3,'b'), (5,'a'), (7,'x')]
-- True
-- >>> insert 5 'x' empty                         == singleton 5 'x'
-- True
insert :: PartialOrd k => k -> v -> POMap k v -> POMap k v
insert = Impl.insert (proxy# :: Proxy# 'Strict)
{-# INLINE insert #-}

insertWith :: PartialOrd k => (v -> v -> v) -> k -> v -> POMap k v -> POMap k v
insertWith = Impl.insertWith (proxy# :: Proxy# 'Strict)
{-# INLINE insertWith #-}

insertWithKey :: PartialOrd k => (k -> v -> v -> v) -> k -> v -> POMap k v -> POMap k v
insertWithKey = Impl.insertWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE insertWithKey #-}

insertLookupWithKey
  :: PartialOrd k
  => (k -> v -> v -> v)
  -> k
  -> v
  -> POMap k v
  -> (Maybe v, POMap k v)
insertLookupWithKey = Impl.insertLookupWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE insertLookupWithKey #-}

adjust :: PartialOrd k => (v -> v) -> k -> POMap k v -> POMap k v
adjust = Impl.adjust (proxy# :: Proxy# 'Strict)
{-# INLINE adjust #-}

adjustWithKey :: PartialOrd k => (k -> v -> v) -> k -> POMap k v -> POMap k v
adjustWithKey = Impl.adjustWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE adjustWithKey #-}

adjustLookupWithKey :: PartialOrd k => (k -> v -> v) -> k -> POMap k v -> (Maybe v, POMap k v)
adjustLookupWithKey = Impl.adjustLookupWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE adjustLookupWithKey #-}

update :: PartialOrd k => (v -> Maybe v) -> k -> POMap k v -> POMap k v
update = Impl.update (proxy# :: Proxy# 'Strict)
{-# INLINE update #-}

updateWithKey :: PartialOrd k => (k -> v -> Maybe v) -> k -> POMap k v -> POMap k v
updateWithKey = Impl.updateWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE updateWithKey #-}

updateLookupWithKey :: PartialOrd k => (k -> v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v)
updateLookupWithKey = Impl.updateLookupWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE updateLookupWithKey #-}

alter :: PartialOrd k => (Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v
alter = Impl.alter (proxy# :: Proxy# 'Strict)
{-# INLINE alter #-}

alterWithKey :: PartialOrd k => (k -> Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v
alterWithKey = Impl.alterWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE alterWithKey #-}

alterLookupWithKey :: PartialOrd k => (k -> Maybe v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v)
alterLookupWithKey = Impl.alterLookupWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE alterLookupWithKey #-}

alterF
  :: (Functor f, PartialOrd k)
  => (Maybe v -> f (Maybe v))
  -> k
  -> POMap k v
  -> f (POMap k v)
alterF = Impl.alterF (proxy# :: Proxy# 'Strict)
{-# INLINE alterF #-}

fromList :: PartialOrd k => [(k, v)] -> POMap k v
fromList = Impl.fromList (proxy# :: Proxy# 'Strict)
{-# INLINE fromList #-}

fromListWith :: PartialOrd k => (v -> v -> v) -> [(k, v)] -> POMap k v
fromListWith = Impl.fromListWith (proxy# :: Proxy# 'Strict)
{-# INLINE fromListWith #-}

fromListWithKey :: PartialOrd k => (k -> v -> v -> v) -> [(k, v)] -> POMap k v
fromListWithKey = Impl.fromListWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE fromListWithKey #-}

map :: (a -> b) -> POMap k a -> POMap k b
map = Impl.map (proxy# :: Proxy# 'Strict)
{-# INLINE map #-}

mapWithKey :: (k -> a -> b) -> POMap k a -> POMap k b
mapWithKey = Impl.mapWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE mapWithKey #-}

traverseWithKey :: Applicative t => (k -> a -> t b) -> POMap k a -> t (POMap k b)
traverseWithKey = Impl.traverseWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE traverseWithKey #-}

mapAccum :: (a -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c)
mapAccum = Impl.mapAccum (proxy# :: Proxy# 'Strict)
{-# INLINE mapAccum #-}

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c)
mapAccumWithKey = Impl.mapAccumWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE mapAccumWithKey #-}

mapKeysWith :: PartialOrd k2 => (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeysWith = Impl.mapKeysWith (proxy# :: Proxy# 'Strict)
{-# INLINE mapKeysWith #-}

traverseMaybeWithKey :: Applicative t => (k -> a -> t (Maybe b)) -> POMap k a -> t (POMap k b)
traverseMaybeWithKey = Impl.traverseMaybeWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE traverseMaybeWithKey #-}

mapMaybe :: (a -> Maybe b) -> POMap k a -> POMap k b
mapMaybe = Impl.mapMaybe (proxy# :: Proxy# 'Strict)
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: (k -> a -> Maybe b) -> POMap k a -> POMap k b
mapMaybeWithKey = Impl.mapMaybeWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE mapMaybeWithKey #-}

mapEither :: (a -> Either b c) -> POMap k a -> (POMap k b, POMap k c)
mapEither = Impl.mapEither (proxy# :: Proxy# 'Strict)
{-# INLINE mapEither #-}

mapEitherWithKey :: (k -> a -> Either b c) -> POMap k a -> (POMap k b, POMap k c)
mapEitherWithKey = Impl.mapEitherWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE mapEitherWithKey #-}
