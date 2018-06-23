{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

-- | This module doesn't respect the PVP!
-- Breaking changes may happen at any minor version (>= *.*.m.*)

module Data.POSet.Internal where

import           Algebra.PartialOrd
import           Control.DeepSeq    (NFData (rnf))
import qualified Data.List          as List
import           Data.POMap.Lazy    (POMap)
import qualified Data.POMap.Lazy    as POMap
import           GHC.Exts           (coerce)
import qualified GHC.Exts
import           Text.Read          (Lexeme (Ident), Read (..), lexP, parens,
                                     prec, readListPrecDefault)

-- $setup
-- This is some setup code for @doctest@.
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> import           Algebra.PartialOrd
-- >>> import           Data.POSet
-- >>> :{
--   newtype Divisibility
--     = Div Int
--     deriving (Eq, Num)
--   instance Show Divisibility where
--     show (Div a) = show a
--   instance PartialOrd Divisibility where
--     Div a `leq` Div b = b `mod` a == 0
--   type DivSet = POSet Divisibility
--   default (Divisibility, DivSet)
-- :}

-- | A set of partially ordered values @k@.
newtype POSet k
  = POSet (POMap k ())

--
-- * Instances
--

instance PartialOrd k => Eq (POSet k) where
  POSet a == POSet b = a == b

instance PartialOrd k => PartialOrd (POSet k) where
  POSet a `leq` POSet b = a `leq` b

instance Show a => Show (POSet a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

instance (Read a, PartialOrd a) => Read (POSet a) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault

instance NFData a => NFData (POSet a) where
  rnf (POSet m) = rnf m

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

-- | \(\mathcal{O}(1)\). The number of elements in this set.
size :: POSet k -> Int
size = coerce (POMap.size @_ @())
{-# INLINE size #-}

-- | \(\mathcal{O}(w)\).
-- The width \(w\) of the chain decomposition in the internal
-- data structure.
-- This is always at least as big as the size of the biggest possible
-- anti-chain.
width :: POSet k -> Int
width = coerce (POMap.width @_ @())
{-# INLINE width #-}

-- | \(\mathcal{O}(w\log n)\).
-- Is the key a member of the map? See also 'notMember'.
member :: PartialOrd k => k -> POSet k -> Bool
member = coerce (POMap.member @_ @())
{-# INLINE member #-}

-- | \(\mathcal{O}(w\log n)\).
-- Is the key not a member of the map? See also 'member'.
notMember :: PartialOrd k => k -> POSet k -> Bool
notMember = coerce (POMap.notMember @_ @())
{-# INLINE notMember #-}

-- | \(\mathcal{O}(w\log n)\).
-- Find the largest set of keys smaller than the given one and
-- return the corresponding list of (key, value) pairs.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupLT 3 (fromList [3, 5])
-- []
-- >>> lookupLT 6 (fromList [3, 5])
-- [3]
lookupLT :: PartialOrd k => k -> POSet k -> [k]
lookupLT k = List.map @(_,()) fst . coerce (POMap.lookupLT @_ @() k)
{-# INLINE lookupLT #-}

-- | \(\mathcal{O}(w\log n)\).
-- Find the largest key smaller or equal to the given one and return
-- the corresponding list of (key, value) pairs.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupLE 2  (fromList [3, 5])
-- []
-- >>> lookupLE 3  (fromList [3, 5])
-- [3]
-- >>> lookupLE 10 (fromList [3, 5])
-- [5]
lookupLE :: PartialOrd k => k -> POSet k -> [k]
lookupLE k = List.map @(_,()) fst . coerce (POMap.lookupLE @_ @() k)
{-# INLINE lookupLE #-}

-- | \(\mathcal{O}(w\log n)\).
-- Find the smallest key greater or equal to the given one and return
-- the corresponding list of (key, value) pairs.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupGE 3 (fromList [3, 5])
-- [3]
-- >>> lookupGE 5 (fromList [3, 10])
-- [10]
-- >>> lookupGE 6 (fromList [3, 5])
-- []
lookupGE :: PartialOrd k => k -> POSet k -> [k]
lookupGE k = List.map @(_,()) fst . coerce (POMap.lookupGE @_ @() k)
{-# INLINE lookupGE #-}

-- | \(\mathcal{O}(w\log n)\).
-- Find the smallest key greater than the given one and return the
-- corresponding list of (key, value) pairs.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupGT 3 (fromList [6, 5])
-- [6]
-- >>> lookupGT 5 (fromList [3, 5])
-- []
lookupGT :: PartialOrd k => k -> POSet k -> [k]
lookupGT k = List.map @(_,()) fst . coerce (POMap.lookupGT @_ @() k)
{-# INLINE lookupGT #-}

-- | \(\mathcal{O}(n_2 w_1 n_1 \log n_1)\).
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: PartialOrd k => POSet k -> POSet k -> Bool
isSubsetOf = coerce (POMap.isSubmapOf @_ @())
{-# INLINE isSubsetOf #-}

-- | \(\mathcal{O}(n_2 w_1 n_1 \log n_1)\).
-- Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: PartialOrd k => POSet k -> POSet k -> Bool
isProperSubsetOf = coerce (POMap.isProperSubmapOf @_ @())
{-# INLINE isProperSubsetOf #-}

--
-- * Construction
--

-- | \(\mathcal{O}(1)\). The empty set.
empty :: POSet k
empty = POSet POMap.empty
{-# INLINE empty #-}

-- | \(\mathcal{O}(1)\). A set with a single element.
singleton :: k -> POSet k
singleton k = POSet (POMap.singleton k ())
{-# INLINE singleton #-}
-- INLINE means we don't need to SPECIALIZE

-- | \(\mathcal{O}(w\log n)\).
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: (PartialOrd k) => k -> POSet k -> POSet k
insert k = coerce (POMap.insert k ())
{-# INLINE insert #-}

-- | \(\mathcal{O}(w\log n)\).
-- Delete an element from a set.
delete :: (PartialOrd k) => k -> POSet k -> POSet k
delete = coerce (POMap.delete @_ @())
{-# INLINE delete #-}

--
-- * Combine
--

-- ** Union

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- The union of two sets, preferring the first set when
-- equal elements are encountered.
union :: PartialOrd k => POSet k -> POSet k -> POSet k
union = coerce (POMap.union @_ @())
{-# INLINE union #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max_i n_i\) and \(w=\max_i w_i\).
-- The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: PartialOrd k => [POSet k] -> POSet k
unions = coerce (POMap.unions @_ @())
{-# INLINE unions #-}

-- ** Difference

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Difference of two sets.
difference :: PartialOrd k => POSet k -> POSet k -> POSet k
difference = coerce (POMap.difference @_ @() @())
{-# INLINE difference #-}

-- ** Intersection

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- The intersection of two sets.
-- Elements of the result come from the first set, so for example
--
-- >>> data AB = A | B deriving Show
-- >>> instance Eq AB where _ == _ = True
-- >>> instance PartialOrd AB where _ `leq` _ = True
-- >>> singleton A `intersection` singleton B
-- fromList [A]
-- >>> singleton B `intersection` singleton A
-- fromList [B]
intersection :: PartialOrd k => POSet k -> POSet k -> POSet k
intersection = coerce (POMap.intersection @_ @() @())
{-# INLINE intersection #-}

--
-- * Filter
--

-- | \(\mathcal{O}(n)\).
-- Filter all elements that satisfy the predicate.
filter :: (k -> Bool) -> POSet k -> POSet k
filter f = coerce (POMap.filterWithKey @_ @() (\k _ -> f k))
{-# INLINE filter #-}

-- | \(\mathcal{O}(n)\).
-- Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
partition :: (k -> Bool) -> POSet k -> (POSet k, POSet k)
partition f = coerce (POMap.partitionWithKey @_ @() (\k _ -> f k))
{-# INLINE partition #-}

-- | \(\mathcal{O}(log n)\). Take while a predicate on the keys holds.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'filter' p
-- @
--
-- @since 0.0.1.0
takeWhileAntitone :: (k -> Bool) -> POSet k -> POSet k
takeWhileAntitone = coerce (POMap.takeWhileAntitone @_ @())

-- | \(\mathcal{O}(log n)\). Drop while a predicate on the keys holds.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'filter' (not . p)
-- @
--
-- @since 0.0.1.0
dropWhileAntitone :: (k -> Bool) -> POSet k -> POSet k
dropWhileAntitone = coerce (POMap.dropWhileAntitone @_ @())

-- | \(\mathcal{O}(log n)\). Divide a set at the point where a predicate on the keys stops holding.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k ==\> p j \>= p k@.
--
-- @
-- spanAntitone p xs = 'partition' p xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the set
-- at some /unspecified/ point where the predicate switches from holding to not
-- holding (where the predicate is seen to hold before the first element and to fail
-- after the last element).
--
-- @since 0.0.1.0
spanAntitone :: (k -> Bool) -> POSet k -> (POSet k, POSet k)
spanAntitone = coerce (POMap.spanAntitone @_ @())

--
-- * Map
--

-- | \(\mathcal{O}(wn\log n)\).
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: PartialOrd k2 => (k1 -> k2) -> POSet k1 -> POSet k2
map = coerce (POMap.mapKeys @_ @_ @())
{-# INLINE map #-}

-- | \(\mathcal{O}(n)\).
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is strictly increasing.
-- /The precondition is not checked./
-- Semi-formally, for every chain @ls@ in @s@ we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
mapMonotonic :: (k1 -> k2) -> POSet k1 -> POSet k2
mapMonotonic = coerce (POMap.mapKeysMonotonic @_ @_ @())
{-# INLINE mapMonotonic #-}

--
-- * Folds
--

-- | \(\mathcal{O}(n)\).
-- A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> POSet a -> b
foldr' f = coerce (POMap.foldrWithKey' @_ @()  (\k _ acc -> f k acc))
{-# INLINE foldr' #-}

-- | \(\mathcal{O}(n)\).
-- A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (b -> a -> b) -> b -> POSet a -> b
foldl' f = coerce (POMap.foldlWithKey' @_ @_ @()  (\k acc _ -> f k acc))
{-# INLINE foldl' #-}

--
-- * Min/Max
--

-- | \(\mathcal{O}(w\log n)\).
-- The minimal keys of the set.
lookupMin :: PartialOrd k => POSet k -> [k]
lookupMin = List.map @(_,()) fst . coerce (POMap.lookupMin @_ @())
{-# INLINE lookupMin #-}

-- | \(\mathcal{O}(w\log n)\).
-- The maximal keys of the set.
lookupMax :: PartialOrd k => POSet k -> [k]
lookupMax = List.map @(_,()) fst . coerce (POMap.lookupMax @_ @())
{-# INLINE lookupMax #-}

--
-- * Conversion
--

-- | \(\mathcal{O}(n)\).
-- The elements of a set in unspecified order.
elems :: POSet k -> [k]
elems = coerce (POMap.keys @_ @())
{-# INLINE elems #-}

-- | \(\mathcal{O}(n)\).
-- The elements of a set in unspecified order.
toList :: POSet k -> [k]
toList = coerce (POMap.keys @_ @())
{-# INLINE toList #-}

-- | \(\mathcal{O}(wn\log n)\).
-- Build a set from a list of keys.
fromList :: (PartialOrd k) => [k] -> POSet k
fromList = coerce (POMap.fromList @_ @()) . List.map (\k -> (k, ()))
{-# INLINE fromList #-}
