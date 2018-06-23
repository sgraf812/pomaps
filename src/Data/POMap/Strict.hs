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
--   deriving (Eq, Read, Show, Num)
--
-- default (Divisibility)
--
-- instance 'PartialOrd' Divisibility where
--   Div a \`leq\` Div b = b \`mod\` a == 0
--
-- type DivMap a = POMap Divisibility a
--
-- -- We want integer literals to be interpreted as 'Divisibility's
-- -- and default 'empty's to DivMap String.
-- default (Divisibility, DivMap String)
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

  , Impl.takeWhileAntitone
  , Impl.dropWhileAntitone
  , Impl.spanAntitone

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
-- This is some setup code for @doctest@.
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
--   default (Divisibility, DivMap String)
-- :}

-- | \(\mathcal{O}(1)\). A map with a single element.
--
-- >>> singleton 1 'a'
-- fromList [(1,'a')]
-- >>> size (singleton 1 'a')
-- 1
singleton :: k -> v -> POMap k v
singleton = Impl.singleton (proxy# :: Proxy# 'Strict)
{-# INLINE singleton #-}

-- | \(\mathcal{O}(w\log n)\).
-- Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
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

-- | \(\mathcal{O}(w\log n)\). Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- >>> insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- True
-- >>> insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- True
-- >>> insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
-- True
insertWith :: PartialOrd k => (v -> v -> v) -> k -> v -> POMap k v -> POMap k v
insertWith = Impl.insertWith (proxy# :: Proxy# 'Strict)
{-# INLINE insertWith #-}

-- | \(\mathcal{O}(w\log n)\). Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- >>> let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- >>> insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- True
-- >>> insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- True
-- >>> insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"
-- True
insertWithKey :: PartialOrd k => (k -> v -> v -> v) -> k -> v -> POMap k v -> POMap k v
insertWithKey = Impl.insertWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE insertWithKey #-}

-- | \(\mathcal{O}(w\log n)\). Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- >>> let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- >>> insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- True
-- >>> insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- True
-- >>> insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
-- True
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- >>> let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- >>> insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- True
-- >>> insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])
-- True
insertLookupWithKey
  :: PartialOrd k
  => (k -> v -> v -> v)
  -> k
  -> v
  -> POMap k v
  -> (Maybe v, POMap k v)
insertLookupWithKey = Impl.insertLookupWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE insertLookupWithKey #-}

-- | \(\mathcal{O}(w\log n)\). Adjust a value at a specific key with the
-- result of the provided function.
-- When the key is not a member of the map, the original map is returned.
--
-- >>> adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- True
-- >>> adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- True
-- >>> adjust ("new " ++) 7 empty                         == empty
-- True
adjust :: PartialOrd k => (v -> v) -> k -> POMap k v -> POMap k v
adjust = Impl.adjust (proxy# :: Proxy# 'Strict)
{-# INLINE adjust #-}

-- | \(\mathcal{O}(w\log n)\). Adjust a value at a specific key with the
-- result of the provided function.
-- When the key is not a member of the map, the original map is returned.
--
-- >>> let f key x = (show key) ++ ":new " ++ x
-- >>> adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- True
-- >>> adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- True
-- >>> adjustWithKey f 7 empty                         == empty
-- True
adjustWithKey :: PartialOrd k => (k -> v -> v) -> k -> POMap k v -> POMap k v
adjustWithKey = Impl.adjustWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE adjustWithKey #-}

-- | \(\mathcal{O}(w\log n)\). Adjust a value at a specific key with the
-- result of the provided function and simultaneously look up the old value
-- at that key.
-- When the key is not a member of the map, the original map is returned.
--
-- >>> let f key old_value = show key ++ ":" ++ show 42 ++ "|" ++ old_value
-- >>> adjustLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:42|a")])
-- True
-- >>> adjustLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- True
-- >>> adjustLookupWithKey f 5 empty                         == (Nothing,  empty)
-- True
adjustLookupWithKey :: PartialOrd k => (k -> v -> v) -> k -> POMap k v -> (Maybe v, POMap k v)
adjustLookupWithKey = Impl.adjustLookupWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE adjustLookupWithKey #-}

-- | \(\mathcal{O}(w\log n)\). The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- >>> let f x = if x == "a" then Just "new a" else Nothing
-- >>> update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- True
-- >>> update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- True
-- >>> update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- True
update :: PartialOrd k => (v -> Maybe v) -> k -> POMap k v -> POMap k v
update = Impl.update (proxy# :: Proxy# 'Strict)
{-# INLINE update #-}

-- | \(\mathcal{O}(w\log n)\). The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- >>> let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- >>> updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- True
-- >>> updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- True
-- >>> updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- True
updateWithKey :: PartialOrd k => (k -> v -> Maybe v) -> k -> POMap k v -> POMap k v
updateWithKey = Impl.updateWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE updateWithKey #-}

-- | \(\mathcal{O}(w\log n)\). Lookup and update. See also 'updateWithKey'.
-- __Warning__: Contrary to "Data.Map.Strict", the lookup does /not/ return
-- the updated value, but the old value. This is consistent with 'insertLookupWithKey'
-- and also @Data.IntMap.Strict.'Data.IntMap.Strict.updateLookupWithKey'@.
--
-- Re-apply the updating function to the looked-up value once more to get the
-- value in the map, like in the last example:
--
-- >>> let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- >>> updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- True
-- >>> updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- True
-- >>> updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
-- True
-- >>> fst (updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")])) >>= f 5
-- Just "5:new a"
updateLookupWithKey :: PartialOrd k => (k -> v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v)
updateLookupWithKey = Impl.updateLookupWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE updateLookupWithKey #-}

-- | \(\mathcal{O}(w\log n)\). The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
--
-- >>> let f _ = Nothing
-- >>> alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- True
-- >>> alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- True
-- >>> let f _ = Just "c"
-- >>> alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- True
-- >>> alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]
-- True
alter :: PartialOrd k => (Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v
alter = Impl.alter (proxy# :: Proxy# 'Strict)
{-# INLINE alter #-}

-- | \(\mathcal{O}(w\log n)\). The expression (@'alterWithKey' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alterWithKey' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f k ('lookup' k m)@.
--
-- >>> let f _ _ = Nothing
-- >>> alterWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- True
-- >>> alterWithKey f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- True
-- >>> let f k _ = Just (show k ++ ":c")
-- >>> alterWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "7:c")]
-- True
-- >>> alterWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:c")]
-- True
alterWithKey :: PartialOrd k => (k -> Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v
alterWithKey = Impl.alterWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE alterWithKey #-}

-- | \(\mathcal{O}(w\log n)\). Lookup and alteration. See also 'alterWithKey'.
--
-- >>> let f k x = if x == Nothing then Just ((show k) ++ ":new a") else Nothing
-- >>> alterLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b")])
-- True
-- >>> alterLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "7:new a")])
-- True
-- >>> alterLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")
-- True
alterLookupWithKey :: PartialOrd k => (k -> Maybe v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v)
alterLookupWithKey = Impl.alterLookupWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE alterLookupWithKey #-}

-- | \(\mathcal{O}(w\log n)\).
-- The expression (@'alterF' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alterF' can be used to inspect, insert, delete, or update a value in a 'Map'.
-- In short: @'lookup' k \<$\> 'alterF' f k m = f ('lookup' k m)@.
--
-- Example:
--
-- @
-- interactiveAlter :: Divibility -> DivMap String -> IO (DivMap String)
-- interactiveAlter k m = alterF f k m where
--   f Nothing -> do
--      putStrLn $ show k ++
--          " was not found in the map. Would you like to add it?"
--      getUserResponse1 :: IO (Maybe String)
--   f (Just old) -> do
--      putStrLn "The key is currently bound to " ++ show old ++
--          ". Would you like to change or delete it?"
--      getUserresponse2 :: IO (Maybe String)
-- @
--
-- 'alterF' is the most general operation for working with an individual
-- key that may or may not be in a given map. When used with trivial
-- functors like 'Identity' and 'Const', it is often slightly slower than
-- more specialized combinators like 'lookup' and 'insert'. However, when
-- the functor is non-trivial and key comparison is not particularly cheap,
-- it is the fastest way.
alterF
  :: (Functor f, PartialOrd k)
  => (Maybe v -> f (Maybe v))
  -> k
  -> POMap k v
  -> f (POMap k v)
alterF = Impl.alterF (proxy# :: Proxy# 'Strict)
{-# INLINE alterF #-}

-- | \(\mathcal{O}(wn\log n)\).
-- Build a map from a list of key\/value pairs.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- This version is strict in its values, as opposed to the 'IsList' instance
-- for 'POMap'.
--
-- >>> fromList [] == (empty :: DivMap String)
-- True
-- >>> fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- True
-- >>> fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]
-- True
fromList :: PartialOrd k => [(k, v)] -> POMap k v
fromList = Impl.fromListImpl (proxy# :: Proxy# 'Strict)
{-# INLINE fromList #-}

-- | \(\mathcal{O}(wn\log n)\).
-- Build a map from a list of key\/value pairs with a combining function.
--
-- This version is strict in its values, as opposed to the 'IsList' instance
-- for 'POMap'.
--
-- >>> fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- True
-- >>> fromListWith (++) [] == (empty :: DivMap String)
-- True
fromListWith :: PartialOrd k => (v -> v -> v) -> [(k, v)] -> POMap k v
fromListWith = Impl.fromListWith (proxy# :: Proxy# 'Strict)
{-# INLINE fromListWith #-}

-- | \(\mathcal{O}(wn\log n)\).
-- Build a map from a list of key\/value pairs with a combining function.
--
-- >>> let f k a1 a2 = (show k) ++ a1 ++ a2
-- >>> fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "3ab"), (5, "5a5ba")]
-- True
-- >>> fromListWithKey f [] == (empty :: DivMap String)
-- True
fromListWithKey :: PartialOrd k => (k -> v -> v -> v) -> [(k, v)] -> POMap k v
fromListWithKey = Impl.fromListWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE fromListWithKey #-}

-- | \(\mathcal{O}(n)\). Map a function over all values in the map.
--
-- >>> map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
-- True
map :: (a -> b) -> POMap k a -> POMap k b
map = Impl.map (proxy# :: Proxy# 'Strict)
{-# INLINE map #-}

-- | \(\mathcal{O}(n)\). Map a function over all values in the map.
--
-- >>> let f key x = (show key) ++ ":" ++ x
-- >>> mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
-- True
mapWithKey :: (k -> a -> b) -> POMap k a -> POMap k b
mapWithKey = Impl.mapWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE mapWithKey #-}

-- | \(\mathcal{O}(n)\).
-- @'traverseWithKey' f m == 'fromList' <$> 'traverse' (\(k, v) -> (\v' -> v' `seq` (k,v')) <$> f k v) ('toList' m)@
-- That is, it behaves much like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value and the values are
-- forced before they are installed in the result map.
--
-- >>> traverseWithKey (\(Div k) v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- True
-- >>> traverseWithKey (\(Div k) v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
-- True
traverseWithKey :: Applicative t => (k -> a -> t b) -> POMap k a -> t (POMap k b)
traverseWithKey = Impl.traverseWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE traverseWithKey #-}

-- | \(\mathcal{O}(n)\).
-- The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- >>> let f a b = (a ++ b, b ++ "X")
-- >>> mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])
-- True
mapAccum :: (a -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c)
mapAccum = Impl.mapAccum (proxy# :: Proxy# 'Strict)
{-# INLINE mapAccum #-}

-- | \(\mathcal{O}(n)\). The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- >>> let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- >>> mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])
-- True
mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c)
mapAccumWithKey = Impl.mapAccumWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE mapAccumWithKey #-}

-- | \(\mathcal{O}(wn\log n)\).
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- >>> mapKeysWith (+) (\ _ -> 1) (fromList [(1,1), (2,2), (3,3), (4,4)]) == singleton 1 10
-- True
-- >>> mapKeysWith (+) (\ _ -> 3) (fromList [(1,1), (2,1), (3,1), (4,1)]) == singleton 3 4
-- True
mapKeysWith :: PartialOrd k2 => (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeysWith = Impl.mapKeysWith (proxy# :: Proxy# 'Strict)
{-# INLINE mapKeysWith #-}

-- | \(\mathcal{O}(n)\).
-- Traverse keys\/values and collect the 'Just' results.
--
-- Contrary to 'traverse', this is value-strict.
traverseMaybeWithKey :: Applicative t => (k -> a -> t (Maybe b)) -> POMap k a -> t (POMap k b)
traverseMaybeWithKey = Impl.traverseMaybeWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE traverseMaybeWithKey #-}

-- | \(\mathcal{O}(n)\).
-- Map values and collect the 'Just' results.
--
-- >>> let f x = if x == "a" then Just "new a" else Nothing
-- >>> mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"
-- True
mapMaybe :: (a -> Maybe b) -> POMap k a -> POMap k b
mapMaybe = Impl.mapMaybe (proxy# :: Proxy# 'Strict)
{-# INLINE mapMaybe #-}

-- | \(\mathcal{O}(n)\).
-- Map keys\/values and collect the 'Just' results.
--
-- >>> let f k _ = if k == 3 then Just ("key : " ++ (show k)) else Nothing
-- >>> mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"
-- True
mapMaybeWithKey :: (k -> a -> Maybe b) -> POMap k a -> POMap k b
mapMaybeWithKey = Impl.mapMaybeWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE mapMaybeWithKey #-}

-- | \(\mathcal{O}(n)\).
-- Map values and separate the 'Left' and 'Right' results.
--
-- >>> let f a = if a < "c" then Left a else Right a
--
-- >>> :{
--   mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
--     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- :}
-- True
--
-- >>> :{
--   mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
--     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- :}
-- True
mapEither :: (a -> Either b c) -> POMap k a -> (POMap k b, POMap k c)
mapEither = Impl.mapEither (proxy# :: Proxy# 'Strict)
{-# INLINE mapEither #-}

-- | \(\mathcal{O}(n)\).
-- Map keys\/values and separate the 'Left' and 'Right' results.
--
-- >>> let f (Div k) a = if k < 5 then Left (k * 2) else Right (a ++ a)
--
-- >>> :{
--   mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
--     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- :}
-- True
--
-- >>> :{
--   mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
--     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])
-- :}
-- True
mapEitherWithKey :: (k -> a -> Either b c) -> POMap k a -> (POMap k b, POMap k c)
mapEitherWithKey = Impl.mapEitherWithKey (proxy# :: Proxy# 'Strict)
{-# INLINE mapEitherWithKey #-}
