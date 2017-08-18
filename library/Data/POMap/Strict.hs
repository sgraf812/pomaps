{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

module Data.POMap.Strict (
  -- * Map type
    Impl.POMap

  -- * Query
  , Impl.null
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
  , Impl.map
  , Impl.mapWithKey
  , Impl.traverseWithKey
  , Impl.traverseMaybeWithKey
  , Impl.mapAccum
  , Impl.mapAccumWithKey
  , Impl.mapKeys
  , Impl.mapKeysWith
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

  , Impl.mapMaybe
  , Impl.mapMaybeWithKey
  , Impl.mapEither
  , Impl.mapEitherWithKey

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

singleton :: k -> v -> POMap k v
singleton = Impl.singleton (proxy# :: Proxy# 'Strict)
{-# INLINE singleton #-}

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
