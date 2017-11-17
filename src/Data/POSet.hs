-- |
-- Module      :  Data.POSet
-- Copyright   :  (c) Sebastian Graf 2017
-- License     :  MIT
-- Maintainer  :  sgraf1337@gmail.com
-- Portability :  portable
--
-- A reasonably efficient implementation of partially ordered sets.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- > import qualified Data.POSet as POSet
--
-- The implementation of 'POSet' is based on a decomposition of
-- chains (totally ordered submaps), inspired by
-- [\"Sorting and Selection in Posets\"](https://arxiv.org/abs/0707.1532).
--
-- Operation comments contain the operation time complexity in
-- [Big-O notation](http://en.wikipedia.org/wiki/Big_O_notation) and
-- commonly refer to two characteristics of the poset from which keys are drawn:
-- The number of elements in the set \(n\) and the /width/ \(w\) of the poset,
-- referring to the size of the biggest anti-chain (set of incomparable elements).
--
-- Generally speaking, lookup and mutation operations incur an additional
-- factor of \(\mathcal{O}(w)\) compared to their counter-parts in "Data.Set".
--
-- Note that for practical applications, the width of the poset should be
-- in the order of \(w\in \mathcal{O}(\frac{n}{\log n})\), otherwise a simple lookup list
-- is asymptotically superior.
-- Even if that holds, the constants might be too big to be useful for any \(n\) that can
-- can happen in practice.
--
-- The following examples assume the following definitions for a set on the divisibility
-- relation on `Int`egers:
--
-- @
-- {-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
--
-- import           Algebra.PartialOrd
-- import           Data.POSet (POSet)
-- import qualified Data.POSet as POSet
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
-- type DivSet = POSet Divisibility
--
-- -- We want integer literals to be interpreted as 'Divisibility's
-- -- and default 'empty's to DivSet.
-- default (Divisibility, DivSet)
-- @
--
-- 'Divisility' is actually an example for a 'PartialOrd' that should not be used as keys of 'POSet'.
-- Its width is \(w=\frac{n}{2}\in\Omega(n)\)!

module Data.POSet
  (
  -- * Set type
    Impl.POSet
  -- * Query
  , Foldable.null
  , Impl.size
  , Impl.member
  , Impl.notMember
  , Impl.lookupLT
  , Impl.lookupGT
  , Impl.lookupLE
  , Impl.lookupGE
  , Impl.isSubsetOf
  , Impl.isProperSubsetOf

  -- * Construction
  , Impl.empty
  , Impl.singleton
  , Impl.insert
  , Impl.delete

  -- * Combine
  , Impl.union
  , Impl.unions
  , Impl.difference
  , Impl.intersection

  -- * Filter
  , Impl.filter
  , Impl.partition

  -- * Map
  , Impl.map
  , Impl.mapMonotonic

  -- * Folds
  , Foldable.foldr
  , Foldable.foldl
  -- ** Strict folds
  , Impl.foldr'
  , Impl.foldl'

  -- * Min\/Max
  , Impl.lookupMin
  , Impl.lookupMax

  -- * Conversion
  , Impl.elems
  , Impl.toList
  , Impl.fromList
  ) where

import qualified Data.Foldable       as Foldable
import qualified Data.POSet.Internal as Impl
