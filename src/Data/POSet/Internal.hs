{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Data.POSet.Internal where

import           Algebra.PartialOrd
import qualified Data.List          as List
import           Data.POMap.Strict  (POMap)
import qualified Data.POMap.Strict  as POMap
import           GHC.Exts           (coerce)
import qualified GHC.Exts

newtype POSet k
  = POSet (POMap k ())
  deriving (Show, Read)

--
-- * Instances
--

instance Foldable POSet where
  foldr f = coerce (POMap.foldrWithKey @_ @() (\k _ acc -> f k acc))
  {-# INLINE foldr #-}
  foldl f = coerce (POMap.foldlWithKey @_ @_ @() (\k acc _ -> f k acc))
  {-# INLINE foldl #-}
  null m = size m == 0
  {-# INLINE null #-}
  length = size
  {-# INLINE length #-}

instance PartialOrd k => GHC.Exts.IsList (POSet k) where
  type Item (POSet k) = k
  fromList = fromList
  toList = toList

--
-- * Query
--

size :: POSet k -> Int
size = coerce (POMap.size @_ @())
{-# INLINE size #-}

width :: POSet k -> Int
width = coerce (POMap.width @_ @())
{-# INLINE width #-}

member :: PartialOrd k => k -> POSet k -> Bool
member = coerce (POMap.member @_ @())
{-# INLINE member #-}

notMember :: PartialOrd k => k -> POSet k -> Bool
notMember = coerce (POMap.notMember @_ @())
{-# INLINE notMember #-}

lookupLT :: PartialOrd k => k -> POSet k -> [k]
lookupLT k = List.map @(_,()) fst . coerce (POMap.lookupLT @_ @() k)
{-# INLINE lookupLT #-}

lookupLE :: PartialOrd k => k -> POSet k -> [k]
lookupLE k = List.map @(_,()) fst . coerce (POMap.lookupLE @_ @() k)
{-# INLINE lookupLE #-}

lookupGE :: PartialOrd k => k -> POSet k -> [k]
lookupGE k = List.map @(_,()) fst . coerce (POMap.lookupGE @_ @() k)
{-# INLINE lookupGE #-}

lookupGT :: PartialOrd k => k -> POSet k -> [k]
lookupGT k = List.map @(_,()) fst . coerce (POMap.lookupGT @_ @() k)
{-# INLINE lookupGT #-}

isSubsetOf :: PartialOrd k => POSet k -> POSet k -> Bool
isSubsetOf = coerce (POMap.isSubmapOf @_ @())
{-# INLINE isSubsetOf #-}

isProperSubsetOf :: PartialOrd k => POSet k -> POSet k -> Bool
isProperSubsetOf = coerce (POMap.isProperSubmapOf @_ @())
{-# INLINE isProperSubsetOf #-}

--
-- * Construction
--

empty :: POSet k
empty = POSet POMap.empty
{-# INLINE empty #-}

singleton :: k -> POSet k
singleton k = POSet (POMap.singleton k ())
{-# INLINE singleton #-}
-- INLINE means we don't need to SPECIALIZE

insert :: (PartialOrd k) => k -> POSet k -> POSet k
insert k = coerce (POMap.insert k ())
{-# INLINE insert #-}

delete :: (PartialOrd k) => k -> POSet k -> POSet k
delete = coerce (POMap.delete @_ @())
{-# INLINE delete #-}

--
-- * Combine
--

-- ** Union

union :: PartialOrd k => POSet k -> POSet k -> POSet k
union = coerce (POMap.union @_ @())
{-# INLINE union #-}

unions :: PartialOrd k => [POSet k] -> POSet k
unions = coerce (POMap.unions @_ @())
{-# INLINE unions #-}

-- ** Difference

difference :: PartialOrd k => POSet k -> POSet k -> POSet k
difference = coerce (POMap.difference @_ @() @())
{-# INLINE difference #-}

-- ** Intersection

intersection :: PartialOrd k => POSet k -> POSet k -> POSet k
intersection = coerce (POMap.intersection @_ @() @())
{-# INLINE intersection #-}

--
-- * Filter
--

filter :: (k -> Bool) -> POSet k -> POSet k
filter f = coerce (POMap.filterWithKey @_ @() (\k _ -> f k))
{-# INLINE filter #-}

partition :: (k -> Bool) -> POSet k -> (POSet k, POSet k)
partition f = coerce (POMap.partitionWithKey @_ @() (\k _ -> f k))
{-# INLINE partition #-}

--
-- * Map
--

map :: PartialOrd k2 => (k1 -> k2) -> POSet k1 -> POSet k2
map = coerce (POMap.mapKeys @_ @_ @())
{-# INLINE map #-}

mapMonotone :: (k1 -> k2) -> POSet k1 -> POSet k2
mapMonotone = coerce (POMap.mapKeysMonotonic @_ @_ @())
{-# INLINE mapMonotone #-}

--
-- * Folds
--

foldr' :: (a -> b -> b) -> b -> POSet a -> b
foldr' f = coerce (POMap.foldrWithKey' @_ @()  (\k _ acc -> f k acc))
{-# INLINE foldr' #-}

foldl' :: (b -> a -> b) -> b -> POSet a -> b
foldl' f = coerce (POMap.foldlWithKey' @_ @_ @()  (\k acc _ -> f k acc))
{-# INLINE foldl' #-}

--
-- * Min/Max
--

lookupMin :: PartialOrd k => POSet k -> [k]
lookupMin = List.map @(_,()) fst . coerce (POMap.lookupMin @_ @())
{-# INLINE lookupMin #-}

lookupMax :: PartialOrd k => POSet k -> [k]
lookupMax = List.map @(_,()) fst . coerce (POMap.lookupMax @_ @())
{-# INLINE lookupMax #-}

--
-- * Conversion
--

elems :: POSet k -> [k]
elems = coerce (POMap.keys @_ @())
{-# INLINE elems #-}

toList :: POSet k -> [k]
toList = coerce (POMap.keys @_ @())
{-# INLINE toList #-}

fromList :: (PartialOrd k) => [k] -> POSet k
fromList = coerce (POMap.fromList @_ @()) . List.map (\k -> (k, ()))
{-# INLINE fromList #-}
