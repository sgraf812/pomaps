{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module doesn't respect the PVP!
-- Breaking changes may happen at any minor version (>= *.*.m.*)

module Data.POMap.Internal where

import           Algebra.PartialOrd
import           Control.Arrow      (first, second, (***))
import           Control.DeepSeq    (NFData (rnf))
import qualified Data.List          as List
import           Data.Map.Internal  (AreWeStrict (..), Map (..))
import qualified Data.Map.Internal  as Map
import qualified Data.Map.Lazy      as Map.Lazy
import qualified Data.Map.Strict    as Map.Strict
import           Data.Maybe         (fromMaybe)
import qualified Data.Maybe         as Maybe
import           Data.Monoid        (Alt (..), Any (..))
import           GHC.Exts           (Proxy#, inline, proxy#)
import qualified GHC.Exts
import           GHC.Magic          (oneShot)
import           Prelude            hiding (filter, lookup, map)
import           Text.Read          (Lexeme (Ident), Read (..), lexP, parens,
                                     prec, readListPrecDefault)

-- $setup
-- This is some setup code for @doctest@.
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> import           Algebra.PartialOrd
-- >>> import           Data.POMap.Lazy
-- >>> import           Data.POMap.Internal
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

-- | Allows us to abstract over value-strictness in a zero-cost manner.
-- GHC should always be able to specialise the two instances of this and
-- consequently inline 'areWeStrict'.
--
-- It's a little sad we can't just use regular singletons, for reasons
-- outlined [here](https://stackoverflow.com/questions/45734362/specialization-of-singleton-parameters).
class SingIAreWeStrict (s :: AreWeStrict) where
  areWeStrict :: Proxy# s -> AreWeStrict

instance SingIAreWeStrict 'Strict where
  areWeStrict _ = Strict

instance SingIAreWeStrict 'Lazy where
  areWeStrict _ = Lazy

-- | Should be inlined and specialised at all call sites.
seq' :: SingIAreWeStrict s => Proxy# s -> a -> b -> b
seq' p a b
  | Lazy <- areWeStrict p = b
  | otherwise = seq a b
{-# INLINE seq' #-}

seqList :: [a] -> [a]
seqList xs = foldr seq xs xs

-- | A map from partially-ordered keys @k@ to values @v@.
data POMap k v = POMap !Int ![Map k v]

type role POMap nominal representational

-- | Internal smart constructor so that we can be sure that we are always
-- spine-strict, discard empty maps and have appropriate size information.
mkPOMap :: [Map k v] -> POMap k v
mkPOMap decomp = POMap (foldr ((+) . Map.size) 0 decomp') decomp'
  where
    decomp' = seqList (List.filter (not . Map.null) decomp)
{-# INLINE mkPOMap #-}

chainDecomposition :: POMap k v -> [Map k v]
chainDecomposition (POMap _ cd) = cd
{-# INLINE chainDecomposition #-}

--
-- * Instances
--

instance (Show k, Show v) => Show (POMap k v) where
  showsPrec d m = showParen (d > 10) $
    showString "fromList " . shows (toList m)

instance (PartialOrd k, Read k, Read e) => Read (POMap k e) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromListImpl (proxy# :: Proxy# 'Lazy) xs)

  readListPrec = readListPrecDefault

-- | \(\mathcal{O}(wn\log n)\), where \(w=\max(w_1,w_2)), n=\max(n_1,n_2)\).
instance (PartialOrd k, Eq v) => Eq (POMap k v) where
  a == b
    | size a /= size b = False
    | otherwise = isSubmapOf a b && isSubmapOf b a

-- | \(\mathcal{O}(wn\log n)\), where \(w=\max(w_1,w_2)), n=\max(n_1,n_2)\).
instance (PartialOrd k, PartialOrd v) => PartialOrd (POMap k v) where
  a `leq` b = isSubmapOfBy leq a b

instance (NFData k, NFData v) => NFData (POMap k v) where
  rnf (POMap _ d) = rnf d

instance PartialOrd k => GHC.Exts.IsList (POMap k v) where
  type Item (POMap k v) = (k, v)
  fromList = fromListImpl (proxy# :: Proxy# 'Lazy)
  toList = toList

instance Functor (POMap k) where
  fmap = map (proxy# :: Proxy# 'Lazy)
  a <$ (POMap _ d) = mkPOMap (fmap (a <$) d)

instance Foldable (POMap k) where
  foldr f acc = List.foldr (flip (Map.foldr f)) acc . chainDecomposition
  {-# INLINE foldr #-}
  foldl f acc = List.foldl (Map.foldl f) acc . chainDecomposition
  {-# INLINE foldl #-}
  foldMap f (POMap _ d) = foldMap (foldMap f) d
  {-# INLINE foldMap #-}
  null m = size m == 0
  {-# INLINE null #-}
  length = size
  {-# INLINE length #-}

instance Traversable (POMap k) where
  traverse f = traverseWithKey (proxy# :: Proxy# 'Lazy) (const f)
  {-# INLINE traverse #-}

--
-- * Query
--

-- | \(\mathcal{O}(1)\). The number of elements in this map.
size :: POMap k v -> Int
size (POMap s _) = s
{-# INLINE size #-}

-- | \(\mathcal{O}(w)\).
-- The width \(w\) of the chain decomposition in the internal
-- data structure.
-- This is always at least as big as the size of the biggest possible
-- anti-chain.
width :: POMap k v -> Int
width = length . chainDecomposition
{-# INLINE width #-}

foldEntry :: (Monoid m, PartialOrd k) => k -> (v -> m) -> POMap k v -> m
foldEntry !k !f = foldMap find . chainDecomposition
  where
    find Tip = mempty
    find (Bin _ k' v l r) =
      case (k `leq` k', k' `leq` k) of
        (True, True)   -> f v
        (True, False)  -> find l
        (False, True)  -> find r
        (False, False) -> mempty
{-# INLINE foldEntry #-}

-- | \(\mathcal{O}(w\log n)\).
-- Is the key a member of the map?
lookup :: PartialOrd k => k -> POMap k v -> Maybe v
lookup !k = getAlt . foldEntry k pure
{-# INLINABLE lookup #-}

-- | \(\mathcal{O}(w\log n)\).
-- Is the key a member of the map? See also 'notMember'.
--
-- >>> member 5 (fromList [(5,'a'), (3,'b')]) == True
-- True
-- >>> member 1 (fromList [(5,'a'), (3,'b')]) == False
-- True
member :: PartialOrd k => k -> POMap k v -> Bool
member !k = getAny . foldEntry k (const (Any True))
{-# INLINABLE member #-}

-- | \(\mathcal{O}(w\log n)\).
-- Is the key not a member of the map? See also 'member'.
--
-- >>> notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- True
-- >>> notMember 1 (fromList [(5,'a'), (3,'b')]) == True
-- True
notMember :: PartialOrd k => k -> POMap k v -> Bool
notMember k = not . member k
{-# INLINABLE notMember #-}

-- | \(\mathcal{O}(w\log n)\).
-- The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- >>> findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- True
-- >>> findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
-- True
findWithDefault :: PartialOrd k => v -> k -> POMap k v -> v
findWithDefault def k = fromMaybe def . lookup k
{-# INLINABLE findWithDefault #-}

data RelationalOperator
  = LessThan
  | LessEqual
  | Equal
  | GreaterEqual
  | GreaterThan
  deriving (Eq, Ord, Show)

flipRelationalOperator :: RelationalOperator -> RelationalOperator
flipRelationalOperator op =
  case op of
    LessThan     -> GreaterThan
    GreaterThan  -> LessThan
    LessEqual    -> GreaterEqual
    GreaterEqual -> LessEqual
    _            -> op

containsOrdering :: Ordering -> RelationalOperator -> Bool
containsOrdering LT LessThan     = True
containsOrdering LT LessEqual    = True
containsOrdering LT _            = False
containsOrdering GT GreaterThan  = True
containsOrdering GT GreaterEqual = True
containsOrdering GT _            = False
containsOrdering EQ LessThan     = False
containsOrdering EQ GreaterThan  = False
containsOrdering EQ _            = True

comparePartial :: PartialOrd k => k -> k -> Maybe Ordering
comparePartial a b =
  case (a `leq` b, b `leq` a) of
    (True, True)   -> Just EQ
    (True, False)  -> Just LT
    (False, True)  -> Just GT
    (False, False) -> Nothing
{-# INLINE comparePartial #-}

addToAntichain :: PartialOrd k => RelationalOperator -> (k, v) -> [(k, v)] -> [(k, v)]
addToAntichain !op entry@(k, _) chain = maybe chain (entry:) (foldr weedOut (Just []) chain)
  where
    weedOut e'@(k', _) mayChain' =
      case comparePartial k k' of
        Just LT
          | containsOrdering LT op -> mayChain' -- don't need e'
          | containsOrdering GT op -> Nothing
        Just GT
          | containsOrdering LT op -> Nothing
          | containsOrdering GT op -> mayChain' -- don't need e'
        Just EQ -> Nothing -- should never happen
        _ -> (e' :) <$> mayChain' -- still need e'
{-# INLINE addToAntichain #-}

dedupAntichain :: PartialOrd k => RelationalOperator -> [(k, v)] -> [(k, v)]
dedupAntichain !op = foldr (addToAntichain op) []

-- If inlined, this optimizes to the equivalent hand-written variants.
lookupX :: PartialOrd k => RelationalOperator -> k -> POMap k v -> [(k, v)]
lookupX !op !k
  -- we bias comparable elements in the opposite direction
  = dedupAntichain (flipRelationalOperator op)
  . Maybe.mapMaybe findNothing
  . chainDecomposition
  where
    findNothing Tip = Nothing
    findNothing (Bin _ k' v' l r) =
      case comparePartial k k' of
        Just EQ
          | containsOrdering EQ op -> Just (k', v')
          | containsOrdering GT op -> findNothing r
          | containsOrdering LT op -> findNothing l
          | otherwise -> error "lookupX.findNothing: inexhaustive match"
        Just LT
          | containsOrdering GT op -> findJust l k' v'
          | otherwise -> findNothing l
        Just GT
          | containsOrdering LT op -> findJust r k' v'
          | otherwise -> findNothing r
        Nothing -- Incomparable, only the min or max element might not be
          | containsOrdering LT op -> findNothing l
          | containsOrdering GT op -> findNothing r
          | otherwise -> Nothing
    findJust Tip k'' v'' = Just (k'', v'')
    findJust (Bin _ k' v' l r) k'' v'' =
      case comparePartial k k' of
        Just EQ
          | containsOrdering EQ op -> Just (k', v')
          | containsOrdering GT op -> findJust r k'' v''
          | containsOrdering LT op -> findJust l k'' v''
          | otherwise -> error "lookupX.findJust: inexhaustive match"
        Just LT
          | containsOrdering GT op -> findJust l k' v'
          | containsOrdering GT op -> findJust l k' v'
          | otherwise -> findJust l k'' v''
        Just GT
          | containsOrdering LT op -> findJust r k' v'
          | otherwise -> findJust r k'' v''
        Nothing -> Just (k'', v'')
{-# INLINE lookupX #-}

-- | \(\mathcal{O}(w\log n)\).
-- Find the largest set of keys smaller than the given one and
-- return the corresponding list of (key, value) pairs.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupLT 3  (fromList [(3,'a'), (5,'b')])
-- []
-- >>> lookupLT 9 (fromList [(3,'a'), (5,'b')])
-- [(3,'a')]
lookupLT :: PartialOrd k => k -> POMap k v -> [(k, v)]
lookupLT = inline lookupX LessThan
{-# INLINABLE lookupLT #-}

-- | \(\mathcal{O}(w\log n)\).
-- Find the largest key smaller or equal to the given one and return
-- the corresponding list of (key, value) pairs.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupLE 2 (fromList [(3,'a'), (5,'b')])
-- []
-- >>> lookupLE 3 (fromList [(3,'a'), (5,'b')])
-- [(3,'a')]
-- >>> lookupLE 10 (fromList [(3,'a'), (5,'b')])
-- [(5,'b')]
lookupLE :: PartialOrd k => k -> POMap k v -> [(k, v)]
lookupLE = inline lookupX LessEqual
{-# INLINABLE lookupLE #-}

-- | \(\mathcal{O}(w\log n)\).
-- Find the smallest key greater or equal to the given one and return
-- the corresponding list of (key, value) pairs.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupGE 3 (fromList [(3,'a'), (5,'b')])
-- [(3,'a')]
-- >>> lookupGE 5 (fromList [(3,'a'), (10,'b')])
-- [(10,'b')]
-- >>> lookupGE 6 (fromList [(3,'a'), (5,'b')])
-- []
lookupGE :: PartialOrd k => k -> POMap k v -> [(k, v)]
lookupGE = inline lookupX GreaterEqual
{-# INLINABLE lookupGE #-}

-- | \(\mathcal{O}(w\log n)\).
-- Find the smallest key greater than the given one and return the
-- corresponding list of (key, value) pairs.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupGT 5 (fromList [(3,'a'), (10,'b')])
-- [(10,'b')]
-- >>> lookupGT 5 (fromList [(3,'a'), (5,'b')])
-- []
lookupGT :: PartialOrd k => k -> POMap k v -> [(k, v)]
lookupGT = inline lookupX GreaterThan
{-# INLINABLE lookupGT #-}


--
-- * Construction
--

-- | \(\mathcal{O}(1)\). The empty map.
--
-- >>> empty
-- fromList []
-- >>> size empty
-- 0
empty :: POMap k v
empty = POMap 0 []
{-# INLINE empty #-}

singleton :: SingIAreWeStrict s => Proxy# s -> k -> v -> POMap k v
singleton s k v = seq' s v $ POMap 1 [Map.singleton k v]
{-# INLINE singleton #-}
-- INLINE means we don't need to SPECIALIZE

--
-- * Insertion
--

insert :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> k -> v -> POMap k v -> POMap k v
insert s = inline insertWith s const
{-# INLINABLE insert #-}
{-# SPECIALIZE insert :: PartialOrd k => Proxy# 'Strict -> k -> v -> POMap k v -> POMap k v #-}
{-# SPECIALIZE insert :: PartialOrd k => Proxy# 'Lazy -> k -> v -> POMap k v -> POMap k v #-}

insertWith
  :: (PartialOrd k, SingIAreWeStrict s)
  => Proxy# s
  -> (v -> v -> v)
  -> k
  -> v
  -> POMap k v
  -> POMap k v
insertWith s f = inline insertWithKey s (const f)
{-# INLINABLE insertWith #-}
{-# SPECIALIZE insertWith :: PartialOrd k => Proxy# 'Strict -> (v -> v -> v) -> k -> v -> POMap k v -> POMap k v #-}
{-# SPECIALIZE insertWith :: PartialOrd k => Proxy# 'Lazy -> (v -> v -> v) -> k -> v -> POMap k v -> POMap k v #-}

insertWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> v -> v -> v) -> k -> v -> POMap k v -> POMap k v
insertWithKey s f k v = inline alterWithKey s (keyedInsertAsAlter f v) k
{-# INLINABLE insertWithKey #-}
{-# SPECIALIZE insertWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> v -> v -> v) -> k -> v -> POMap k v -> POMap k v #-}
{-# SPECIALIZE insertWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> v -> v -> v) -> k -> v -> POMap k v -> POMap k v #-}

insertLookupWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> v -> v -> v) -> k -> v -> POMap k v -> (Maybe v, POMap k v)
insertLookupWithKey s f k v = inline alterLookupWithKey s (keyedInsertAsAlter f v) k
{-# INLINABLE insertLookupWithKey #-}
{-# SPECIALIZE insertLookupWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> v -> v -> v) -> k -> v -> POMap k v -> (Maybe v, POMap k v) #-}
{-# SPECIALIZE insertLookupWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> v -> v -> v) -> k -> v -> POMap k v -> (Maybe v, POMap k v) #-}

keyedInsertAsAlter :: (k -> v -> v -> v) -> v -> k -> Maybe v -> Maybe v
keyedInsertAsAlter _ v _ Nothing   = Just v
keyedInsertAsAlter f v k (Just v') = Just (f k v v')
{-# INLINE keyedInsertAsAlter #-}

--
-- * Deletion
--

data LookupResult a
  = Incomparable
  | NotFound a
  | Found a
  deriving (Eq, Show, Functor)

instance Ord a => Ord (LookupResult a) where
  compare a b =
    case (a, b) of
      (Incomparable, Incomparable) -> EQ
      (Incomparable, _)            -> GT
      (NotFound n, NotFound m)     -> compare n m
      (NotFound{}, Found{})        -> GT
      (Found n, Found m)           -> compare n m
      _                            -> LT

overChains
  :: (Map k v -> LookupResult a)
  -> (Map k v -> b -> b)
  -> (a -> [Map k v] -> b)
  -> ([Map k v] -> b)
  -> POMap k v
  -> b
overChains handleChain oldWon newWon incomparable pomap
  = unwrapResult
  . fmap snd
  . foldr improve Incomparable
  . zip (List.tails decomp)
  . fmap handleChain
  $ decomp
  where
    decomp = chainDecomposition pomap
    improve ([], _) _ = error "List.tails was empty"
    improve (chain:chains, candidate) winner =
      -- We want to minimize the score: Prefer Found over NotFound and
      -- Incomparability (which means we have to add a new chain to the
      -- composition)
      case compare (Map.size chain <$ candidate) (fst <$> winner) of
        GT -> second (oldWon chain) <$> winner
        _  -> (\chain' -> (Map.size chain, newWon chain' chains)) <$> candidate
    unwrapResult res =
      case res of
        Incomparable    -> incomparable decomp
        NotFound chains -> chains
        Found chains    -> chains
{-# INLINE overChains #-}

-- | \(\mathcal{O}(w\log n)\).
-- Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- >>> delete 5 (fromList [(5,"a"), (3,"b")])
-- fromList [(3,"b")]
-- >>> delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- True
-- >>> delete 5 empty
-- fromList []
delete :: PartialOrd k => k -> POMap k v -> POMap k v
delete = inline update (proxy# :: Proxy# 'Lazy) (const Nothing)
{-# INLINABLE delete #-}

-- | \(\mathcal{O}(w\log n)\). Simultaneous 'delete' and 'lookup'.
deleteLookup :: PartialOrd k => k -> POMap k v -> (Maybe v, POMap k v)
deleteLookup = inline updateLookupWithKey (proxy# :: Proxy# 'Lazy) (\_ _ -> Nothing)
{-# INLINABLE deleteLookup #-}

adjust :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (v -> v) -> k -> POMap k v -> POMap k v
adjust s f = inline update s (Just . f)
{-# INLINABLE adjust #-}
{-# SPECIALIZE adjust :: PartialOrd k => Proxy# 'Strict -> (v -> v) -> k -> POMap k v -> POMap k v #-}
{-# SPECIALIZE adjust :: PartialOrd k => Proxy# 'Lazy -> (v -> v) -> k -> POMap k v -> POMap k v #-}


adjustWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> v -> v) -> k -> POMap k v -> POMap k v
adjustWithKey s f = inline updateWithKey s (\k v -> Just (f k v))
{-# INLINABLE adjustWithKey #-}
{-# SPECIALIZE adjustWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> v -> v) -> k -> POMap k v -> POMap k v #-}
{-# SPECIALIZE adjustWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> v -> v) -> k -> POMap k v -> POMap k v #-}

adjustLookupWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> v -> v) -> k -> POMap k v -> (Maybe v, POMap k v)
adjustLookupWithKey s f = inline updateLookupWithKey s (\k v -> Just (f k v))
{-# INLINABLE adjustLookupWithKey #-}
{-# SPECIALIZE adjustLookupWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> v -> v) -> k -> POMap k v -> (Maybe v, POMap k v) #-}
{-# SPECIALIZE adjustLookupWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> v -> v) -> k -> POMap k v -> (Maybe v, POMap k v) #-}

update :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (v -> Maybe v) -> k -> POMap k v -> POMap k v
update s f = inline alter s (>>= f)
{-# INLINABLE update #-}
{-# SPECIALIZE update :: PartialOrd k => Proxy# 'Strict -> (v -> Maybe v) -> k -> POMap k v -> POMap k v #-}
{-# SPECIALIZE update :: PartialOrd k => Proxy# 'Lazy -> (v -> Maybe v) -> k -> POMap k v -> POMap k v #-}

updateWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> v -> Maybe v) -> k -> POMap k v -> POMap k v
updateWithKey s f = inline alterWithKey s (\k mv -> mv >>= f k)
{-# INLINABLE updateWithKey #-}
{-# SPECIALIZE updateWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> v -> Maybe v) -> k -> POMap k v -> POMap k v #-}
{-# SPECIALIZE updateWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> v -> Maybe v) -> k -> POMap k v -> POMap k v #-}

updateLookupWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v)
updateLookupWithKey s f = inline alterLookupWithKey s (\k mv -> mv >>= f k)
{-# INLINABLE updateLookupWithKey #-}
{-# SPECIALIZE updateLookupWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v) #-}
{-# SPECIALIZE updateLookupWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v) #-}

alter :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v
alter s f = inline alterWithKey s (const f)
{-# INLINABLE alter #-}
{-# SPECIALIZE alter :: PartialOrd k => Proxy# 'Strict -> (Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v #-}
{-# SPECIALIZE alter :: PartialOrd k => Proxy# 'Lazy -> (Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v #-}

alterWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v
alterWithKey s f !k = mkPOMap . overChains handleChain oldWon newWon incomparable
  where
    handleChain = alterChain s f k
    oldWon chain chains' = chain : chains'
    newWon chain' chains = chain' : chains
    incomparable decomp =
      case f k Nothing of
        Nothing -> decomp
        Just v  -> seq' s v (Map.singleton k v : decomp)
{-# INLINABLE alterWithKey #-}
{-# SPECIALIZE alterWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v #-}
{-# SPECIALIZE alterWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v #-}

alterChain :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> Maybe v -> Maybe v) -> k -> Map k v -> LookupResult (Map k v)
alterChain s f k = go
  where
    go Tip = NotFound $ case f k Nothing of
      Just v  -> seq' s v (Map.singleton k v)
      Nothing -> Tip
    go (Bin n k' v' l r) =
      case (k `leq` k', k' `leq` k) of
        (True, True) -> Found $ case f k (Just v') of
          Just v  -> seq' s v (Bin n k' v l r)
          Nothing -> Tip
        (True, False)  -> oneShot (\l' -> Map.balanceL k' v' l' r) <$> go l
        (False, True)  -> oneShot (\r' -> Map.balanceR k' v' l r') <$> go r
        (False, False) -> Incomparable
{-# INLINE alterChain #-}

alterLookupWithKey
  :: (PartialOrd k, SingIAreWeStrict s)
  => Proxy# s
  -> (k -> Maybe v -> Maybe v)
  -> k
  -> POMap k v
  -> (Maybe v, POMap k v)
alterLookupWithKey s f !k
  = second mkPOMap
  . overChains handleChain oldWon newWon incomparable
  where
    handleChain = alterLookupChain s f k
    oldWon chain (v, chains') = (v, chain : chains')
    newWon (v', chain') chains = (v', chain' : chains)
    incomparable decomp =
      (Nothing, case f k Nothing of
        Nothing -> decomp
        Just v  -> seq' s v (Map.singleton k v : decomp))
{-# INLINABLE alterLookupWithKey #-}
{-# SPECIALIZE alterLookupWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v) #-}
{-# SPECIALIZE alterLookupWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v) #-}

alterLookupChain :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> Maybe v -> Maybe v) -> k -> Map k v -> LookupResult (Maybe v, Map k v)
alterLookupChain s f k = go
  where
    go Tip = NotFound (Nothing, case f k Nothing of
      Just v  -> seq' s v (Map.singleton k v)
      Nothing -> Tip)
    go (Bin n k' v' l r) =
      case (k `leq` k', k' `leq` k) of
        (True, True) -> Found (Just v', case f k (Just v') of
          Just v  -> seq' s v (Bin n k' v l r)
          Nothing -> Tip)
        (True, False)  -> second (oneShot (\l' -> Map.balanceL k' v' l' r)) <$> go l
        (False, True)  -> second (oneShot (\r' -> Map.balanceR k' v' l r')) <$> go r
        (False, False) -> Incomparable
{-# INLINE alterLookupChain #-}

alterF
  :: (Functor f, PartialOrd k, SingIAreWeStrict s)
  => Proxy# s
  -> (Maybe v -> f (Maybe v))
  -> k
  -> POMap k v
  -> f (POMap k v)
alterF s f !k = fmap mkPOMap . overChains handleChain oldWon newWon incomparable
  where
    handleChain = alterFChain s k
    -- prepends the unaltered chain to the altered tail
    oldWon chain altered = fmap (chain:) altered
    -- prepends the altered chain to the unaltered tail
    newWon alt chains = fmap (:chains) (alt f)
    (<#>) = flip (<$>)
    -- prepends a new chain in the incomparable case if
    -- the alteration function produces a value
    incomparable decomp = f Nothing <#> \case
      Nothing -> decomp
      Just v  -> seq' s v (Map.singleton k v : decomp)
{-# INLINABLE alterF #-}
{-# SPECIALIZE alterF :: (Functor f, PartialOrd k) => Proxy# 'Strict -> (Maybe v -> f (Maybe v)) -> k -> POMap k v -> f (POMap k v) #-}
{-# SPECIALIZE alterF :: (Functor f, PartialOrd k) => Proxy# 'Lazy -> (Maybe v -> f (Maybe v)) -> k -> POMap k v -> f (POMap k v) #-}

alterFChain
  -- `f` should potentially be pulled into the result type, but not willing
  -- to complicate this right now
  :: (Functor f, PartialOrd k, SingIAreWeStrict s)
  => Proxy# s
  -> k
  -> Map k v
  -> LookupResult ((Maybe v -> f (Maybe v)) -> f (Map k v))
alterFChain s k = go
  where
    -- This is going to be reaaally crazy. Maybe we could use some ContT for
    -- this, I don't know...
    -- So, we always lift the outer functor LookupResult.
    -- That functor contains the logic for actually doing the adjustment,
    -- which takes the function that does the actual adjustment as an argument
    -- and maps into an arbitrary functor `f` which we have to map through.
    ret res val cont = res (oneShot (\f -> cont <$> f val))
    lift sub cont = oneShot (\a f -> cont <$> a f) <$> sub
    go Tip =
      ret NotFound Nothing . oneShot $ \case
        Just v  -> seq' s v (Map.singleton k v)
        Nothing -> Tip
    go (Bin n k' v l r) =
      case (k `leq` k', k' `leq` k) of
        (True, True)   ->
          ret Found (Just v) . oneShot $ \case
            Just v' -> seq' s v' (Bin n k v' l r)
            Nothing -> Tip
        (True, False)  -> lift (go l) . oneShot $ \l' -> Map.balanceL k' v l' r
        (False, True)  -> lift (go r) . oneShot $ \r' -> Map.balanceL k' v l r'
        (False, False) -> Incomparable

--
-- * Combine
--

-- ** Union

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- >>> union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]
-- True
union :: PartialOrd k => POMap k v -> POMap k v -> POMap k v
union = inline unionWith const
{-# INLINABLE union #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Union with a combining function.
--
-- >>> unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
-- True
unionWith :: PartialOrd k => (v -> v -> v) -> POMap k v -> POMap k v -> POMap k v
unionWith f = inline unionWithKey (const f)
{-# INLINABLE unionWith #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Union with a combining function.
--
-- >>> let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- >>> unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]
-- True
unionWithKey :: PartialOrd k => (k -> v -> v -> v) -> POMap k v -> POMap k v -> POMap k v
unionWithKey f l r = List.foldl' (\m (k, v) -> inline insertWithKey (proxy# :: Proxy# 'Lazy) f k v m) r (toList l)
{-# INLINABLE unionWithKey #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max_i n_i\) and \(w=\max_i w_i\).
-- The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
--
-- >>> :{
--   unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
--      == fromList [(3, "b"), (5, "a"), (7, "C")]
-- :}
-- True
--
-- >>> :{
--  unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
--      == fromList [(3, "B3"), (5, "A3"), (7, "C")]
-- :}
-- True
unions :: PartialOrd k => [POMap k v] -> POMap k v
unions = inline unionsWith const
{-# INLINABLE unions #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max_i n_i\) and \(w=\max_i w_i\).
-- The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- >>> :{
--  unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
--      == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]
-- :}
-- True
unionsWith :: PartialOrd k => (v -> v -> v) -> [POMap k v] -> POMap k v
unionsWith f = List.foldl' (unionWith f) empty
{-# INLINABLE unionsWith #-}

-- * Difference

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Difference of two maps.
-- Return elements of the first map not existing in the second map.
--
-- >>> difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")])
-- fromList [(3,"b")]
difference :: PartialOrd k => POMap k a -> POMap k b -> POMap k a
difference = inline differenceWith (\_ _ -> Nothing)
{-# INLINABLE difference #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Difference with a combining function.
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- >>> let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- >>> differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- fromList [(3,"b:B")]
differenceWith :: PartialOrd k => (a -> b -> Maybe a) -> POMap k a -> POMap k b -> POMap k a
differenceWith f = inline differenceWithKey (const f)
{-# INLINABLE differenceWith #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- >>> let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- >>> differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- fromList [(3,"3:b|B")]
differenceWithKey :: PartialOrd k => (k -> a -> b -> Maybe a) -> POMap k a -> POMap k b -> POMap k a
differenceWithKey f l
  = List.foldl' (\m (k, v) -> inline alterWithKey (proxy# :: Proxy# 'Lazy) (f' v) k m) l
  . toList
  where
    f' _ _ Nothing   = Nothing
    f' v k (Just v') = f k v' v
{-# INLINABLE differenceWithKey #-}

-- ** Intersection

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
--
-- >>> intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")])
-- fromList [(5,"a")]
intersection :: PartialOrd k => POMap k a -> POMap k b -> POMap k a
intersection = inline intersectionWith const
{-# INLINABLE intersection #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Intersection with a combining function.
--
-- >>> intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")])
-- fromList [(5,"aA")]
intersectionWith :: PartialOrd k => (a -> b -> c) -> POMap k a -> POMap k b -> POMap k c
intersectionWith f = inline intersectionWithKey (const f)
{-# INLINABLE intersectionWith #-}

-- | \(\mathcal{O}(wn\log n)\), where \(n=\max(n_1,n_2)\) and \(w=\max(w_1,w_2)\).
-- Intersection with a combining function.
--
-- >>> let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- >>> intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")])
-- fromList [(5,"5:a|A")]
intersectionWithKey :: PartialOrd k => (k -> a -> b -> c) -> POMap k a -> POMap k b -> POMap k c
intersectionWithKey f l r
  = fromListImpl (proxy# :: Proxy# 'Lazy)
  . Maybe.mapMaybe (\(k,a) -> [(k, f k a b) | b <- lookup k r])
  . toList
  $ l
{-# INLINABLE intersectionWithKey #-}


-- * Traversals

map :: SingIAreWeStrict s => Proxy# s -> (a -> b) -> POMap k a -> POMap k b
map s f (POMap _ chains)
  | Strict <- areWeStrict s = mkPOMap (fmap (Map.Strict.map f) chains)
  | otherwise = mkPOMap (fmap (Map.Lazy.map f) chains)
{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall s f g xs . map s f (map s g xs) = map s (f . g) xs
 #-}
{-# SPECIALIZE map :: Proxy# 'Strict -> (a -> b) -> POMap k a -> POMap k b #-}
{-# SPECIALIZE map :: Proxy# 'Lazy -> (a -> b) -> POMap k a -> POMap k b #-}

mapWithKey :: SingIAreWeStrict s => Proxy# s -> (k -> a -> b) -> POMap k a -> POMap k b
mapWithKey s f (POMap _ d)
  | Strict <- areWeStrict s = mkPOMap (fmap (Map.Strict.mapWithKey f) d)
  | otherwise = mkPOMap (fmap (Map.Lazy.mapWithKey f) d)
{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall s f g xs . mapWithKey s f (mapWithKey s g xs) =
  mapWithKey s (\k a -> f k (g k a)) xs
"mapWithKey/map" forall s f g xs . mapWithKey s f (map s g xs) =
  mapWithKey s (\k a -> f k (g a)) xs
"map/mapWithKey" forall s f g xs . map s f (mapWithKey s g xs) =
  mapWithKey s (\k a -> f (g k a)) xs
 #-}
{-# SPECIALIZE mapWithKey :: Proxy# 'Strict -> (k -> a -> b) -> POMap k a -> POMap k b #-}
{-# SPECIALIZE mapWithKey :: Proxy# 'Lazy -> (k -> a -> b) -> POMap k a -> POMap k b #-}

traverseWithKey :: (Applicative t, SingIAreWeStrict s) => Proxy# s -> (k -> a -> t b) -> POMap k a -> t (POMap k b)
traverseWithKey s f (POMap _ d)
  | Strict <- areWeStrict s = mkPOMap <$> traverse (Map.Strict.traverseWithKey f) d
  | otherwise = mkPOMap <$> traverse (Map.Lazy.traverseWithKey f) d
{-# INLINABLE traverseWithKey #-}
{-# SPECIALIZE traverseWithKey :: Applicative t => Proxy# 'Strict -> (k -> a -> t b) -> POMap k a -> t (POMap k b) #-}
{-# SPECIALIZE traverseWithKey :: Applicative t => Proxy# 'Lazy -> (k -> a -> t b) -> POMap k a -> t (POMap k b) #-}

mapAccum :: SingIAreWeStrict s => Proxy# s -> (a -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c)
mapAccum s f = inline mapAccumWithKey s (\a _ b -> f a b)
{-# INLINABLE mapAccum #-}
{-# SPECIALIZE mapAccum :: Proxy# 'Strict -> (a -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c) #-}
{-# SPECIALIZE mapAccum :: Proxy# 'Lazy -> (a -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c) #-}

mapAccumWithKey :: SingIAreWeStrict s => Proxy# s -> (a -> k -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c)
mapAccumWithKey s f acc (POMap _ chains) = (acc', mkPOMap chains')
  where
    (acc', chains')
      | Strict <- areWeStrict s = List.mapAccumL (Map.Strict.mapAccumWithKey f) acc chains
      | otherwise = List.mapAccumL (Map.Lazy.mapAccumWithKey f) acc chains
{-# INLINABLE mapAccumWithKey #-}
{-# SPECIALIZE mapAccumWithKey :: Proxy# 'Strict -> (a -> k -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c) #-}
{-# SPECIALIZE mapAccumWithKey :: Proxy# 'Lazy -> (a -> k -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c) #-}

-- | \(\mathcal{O}(wn\log n)\).
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- >>> mapKeys (+ 1) (fromList [(5,"a"), (3,"b")]) == fromList [(4, "b"), (6, "a")]
-- True
-- >>> mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")])
-- fromList [(1,"c")]
-- >>> mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")])
-- fromList [(3,"c")]
mapKeys :: PartialOrd k2 => (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeys f = fromListImpl (proxy# :: Proxy# 'Lazy) . fmap (first f) . toList

mapKeysWith :: (PartialOrd k2, SingIAreWeStrict s) => Proxy# s -> (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeysWith s c f = fromListWith s c . fmap (first f) . toList
{-# INLINABLE mapKeysWith #-}
{-# SPECIALIZE mapKeysWith :: PartialOrd k2 => Proxy# 'Strict -> (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v #-}
{-# SPECIALIZE mapKeysWith :: PartialOrd k2 => Proxy# 'Lazy -> (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v #-}

-- | \(\mathcal{O}(n)\).
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, for every chain @ls@ in @s@ we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- >>> mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
-- True
mapKeysMonotonic :: (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeysMonotonic f (POMap _ d) = mkPOMap (fmap (Map.mapKeysMonotonic f) d)

--
-- * Folds
--

-- | \(\mathcal{O}(n)\).
-- A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> POMap k a -> b
foldr' f acc = List.foldr (flip (Map.foldr' f)) acc . chainDecomposition
{-# INLINE foldr' #-}

-- | \(\mathcal{O}(n)\).
-- Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- >>> keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- >>> let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- >>> foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
-- True
foldrWithKey :: (k -> a -> b -> b) -> b -> POMap k a -> b
foldrWithKey f acc = List.foldr (flip (Map.foldrWithKey f)) acc . chainDecomposition
{-# INLINE foldrWithKey #-}

-- | \(\mathcal{O}(n)\).
-- A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (k -> a -> b -> b) -> b -> POMap k a -> b
foldrWithKey' f acc = List.foldr (flip (Map.foldrWithKey' f)) acc . chainDecomposition
{-# INLINE foldrWithKey' #-}

-- | \(\mathcal{O}(n)\).
-- A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (b -> a -> b) -> b -> POMap k a -> b
foldl' f acc = List.foldl' (Map.foldl' f) acc . chainDecomposition
{-# INLINE foldl' #-}

-- | \(\mathcal{O}(n)\).
-- Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- >>> keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- >>> let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- >>> foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
-- True
foldlWithKey :: (b -> k -> a -> b) -> b -> POMap k a -> b
foldlWithKey f acc = List.foldl (Map.foldlWithKey f) acc . chainDecomposition
{-# INLINE foldlWithKey #-}

-- | \(\mathcal{O}(n)\).
-- A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (b -> k -> a -> b) -> b -> POMap k a -> b
foldlWithKey' f acc = List.foldl' (Map.foldlWithKey' f) acc . chainDecomposition
{-# INLINE foldlWithKey' #-}

-- | \(\mathcal{O}(n)\).
-- Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
foldMapWithKey :: Monoid m => (k -> a -> m) -> POMap k a -> m
foldMapWithKey f = foldMap (Map.foldMapWithKey f ) . chainDecomposition
{-# INLINE foldMapWithKey #-}

-- * Conversion

-- | \(\mathcal{O}(n)\).
-- Return all elements of the map in unspecified order.
--
-- >>> elems (fromList [(5,"a"), (3,"b")])
-- ["b","a"]
-- >>> elems empty
-- []
elems :: POMap k v -> [v]
elems = concatMap Map.elems . chainDecomposition

-- | \(\mathcal{O}(n)\).
-- Return all keys of the map in unspecified order.
--
-- >>> keys (fromList [(5,"a"), (3,"b")])
-- [3,5]
-- >>> keys empty
-- []
keys :: POMap k v -> [k]
keys = concatMap Map.keys . chainDecomposition

-- | \(\mathcal{O}(n)\).
-- Return all key\/value pairs in the map
-- in unspecified order.
--
-- >>> assocs (fromList [(5,"a"), (3,"b")])
-- [(3,"b"),(5,"a")]
-- >>> assocs empty
-- []
assocs :: POMap k v -> [(k, v)]
assocs = concatMap Map.toList . chainDecomposition

-- | \(\mathcal{O}(n)\).
-- Return all key\/value pairs in the map
-- in unspecified order.
--
-- Currently, @toList = 'assocs'@.
toList :: POMap k v -> [(k, v)]
toList = assocs

-- TODO: keysSet, fromSet

-- | Intentionally named this way, to disambiguate it from 'fromList'.
-- This is so that we can doctest this module.
fromListImpl :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> [(k, v)] -> POMap k v
fromListImpl s = List.foldl' (\m (k,v) -> insert s k v m) empty
{-# INLINABLE fromListImpl #-}
{-# SPECIALIZE fromListImpl :: PartialOrd k => Proxy# 'Strict -> [(k, v)] -> POMap k v #-}
{-# SPECIALIZE fromListImpl :: PartialOrd k => Proxy# 'Lazy -> [(k, v)] -> POMap k v #-}

fromListWith :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (v -> v -> v) -> [(k, v)] -> POMap k v
fromListWith s f = List.foldl' (\m (k,v) -> insertWith s f k v m) empty
{-# INLINABLE fromListWith #-}
{-# SPECIALIZE fromListWith :: PartialOrd k => Proxy# 'Strict -> (v -> v -> v) -> [(k, v)] -> POMap k v #-}
{-# SPECIALIZE fromListWith :: PartialOrd k => Proxy# 'Lazy -> (v -> v -> v) -> [(k, v)] -> POMap k v #-}

fromListWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> v -> v -> v) -> [(k, v)] -> POMap k v
fromListWithKey s f = List.foldl' (\m (k,v) -> insertWithKey s f k v m) empty
{-# INLINABLE fromListWithKey #-}
{-# SPECIALIZE fromListWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> v -> v -> v) -> [(k, v)] -> POMap k v #-}
{-# SPECIALIZE fromListWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> v -> v -> v) -> [(k, v)] -> POMap k v #-}

--
-- * Filter
--

-- | \(\mathcal{O}(n)\).
-- Filter all values that satisfy the predicate.
--
-- >>> filter (> "a") (fromList [(5,"a"), (3,"b")])
-- fromList [(3,"b")]
-- >>> filter (> "x") (fromList [(5,"a"), (3,"b")])
-- fromList []
-- >>> filter (< "a") (fromList [(5,"a"), (3,"b")])
-- fromList []
filter :: (v -> Bool) -> POMap k v -> POMap k v
filter p = filterWithKey (const p)

-- | \(\mathcal{O}(n)\).
-- Filter all keys\/values that satisfy the predicate.
--
-- >>> filterWithKey (\(Div k) _ -> k > 4) (fromList [(5,"a"), (3,"b")])
-- fromList [(5,"a")]
filterWithKey :: (k -> v -> Bool) -> POMap k v -> POMap k v
filterWithKey p (POMap _ d) = mkPOMap (Map.filterWithKey p <$> d)

-- TODO: restrictKeys, withoutKeys

-- | \(\mathcal{O}(n)\).
-- Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- >>> partition (> "a") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b")], fromList [(5, "a")])
-- True
-- >>> partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- True
-- >>> partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
-- True
partition :: (v -> Bool) -> POMap k v -> (POMap k v, POMap k v)
partition p = partitionWithKey (const p)

-- | \(\mathcal{O}(n)\).
-- Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- >>> partitionWithKey (\ (Div k) _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (fromList [(5, "a")], fromList [(3, "b")])
-- True
-- >>> partitionWithKey (\ (Div k) _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- True
-- >>> partitionWithKey (\ (Div k) _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
-- True
partitionWithKey :: (k -> v -> Bool) -> POMap k v -> (POMap k v, POMap k v)
partitionWithKey p (POMap _ d)
  = (mkPOMap *** mkPOMap)
  . unzip
  . fmap (Map.partitionWithKey p)
  $ d

mapMaybe :: SingIAreWeStrict s => Proxy# s -> (a -> Maybe b) -> POMap k a -> POMap k b
mapMaybe s f = mapMaybeWithKey s (const f)
{-# INLINABLE mapMaybe #-}
{-# SPECIALIZE mapMaybe :: Proxy# 'Strict -> (a -> Maybe b) -> POMap k a -> POMap k b #-}
{-# SPECIALIZE mapMaybe :: Proxy# 'Lazy -> (a -> Maybe b) -> POMap k a -> POMap k b #-}

mapMaybeWithKey :: SingIAreWeStrict s => Proxy# s -> (k -> a -> Maybe b) -> POMap k a -> POMap k b
mapMaybeWithKey s f (POMap _ d)
  | Strict <- areWeStrict s = mkPOMap (Map.Strict.mapMaybeWithKey f <$> d)
  | otherwise = mkPOMap (Map.Lazy.mapMaybeWithKey f <$> d)
{-# INLINABLE mapMaybeWithKey #-}
{-# SPECIALIZE mapMaybeWithKey :: Proxy# 'Strict -> (k -> a -> Maybe b) -> POMap k a -> POMap k b #-}
{-# SPECIALIZE mapMaybeWithKey :: Proxy# 'Lazy -> (k -> a -> Maybe b) -> POMap k a -> POMap k b #-}

traverseMaybeWithKey :: (Applicative f, SingIAreWeStrict s) => Proxy# s -> (k -> a -> f (Maybe b)) -> POMap k a -> f (POMap k b)
traverseMaybeWithKey s f (POMap _ d)
  | Strict <- areWeStrict s = mkPOMap <$> traverse (Map.Strict.traverseMaybeWithKey f) d
  | otherwise = mkPOMap <$> traverse (Map.Lazy.traverseMaybeWithKey f) d
{-# INLINABLE traverseMaybeWithKey #-}
{-# SPECIALIZE traverseMaybeWithKey :: Applicative f => Proxy# 'Strict -> (k -> a -> f (Maybe b)) -> POMap k a -> f (POMap k b) #-}
{-# SPECIALIZE traverseMaybeWithKey :: Applicative f => Proxy# 'Lazy -> (k -> a -> f (Maybe b)) -> POMap k a -> f (POMap k b) #-}

mapEither :: SingIAreWeStrict s => Proxy# s -> (a -> Either b c) -> POMap k a -> (POMap k b, POMap k c)
mapEither s p = mapEitherWithKey s (const p)
{-# INLINABLE mapEither #-}
{-# SPECIALIZE mapEither :: Proxy# 'Strict -> (a -> Either b c) -> POMap k a -> (POMap k b, POMap k c) #-}
{-# SPECIALIZE mapEither :: Proxy# 'Lazy -> (a -> Either b c) -> POMap k a -> (POMap k b, POMap k c) #-}

mapEitherWithKey :: SingIAreWeStrict s => Proxy# s -> (k -> a -> Either b c) -> POMap k a -> (POMap k b, POMap k c)
mapEitherWithKey s p (POMap _ d)
  = (mkPOMap *** mkPOMap)
  . unzip
  . fmap (mewk p)
  $ d
  where
    mewk
      | Strict <- areWeStrict s = Map.Strict.mapEitherWithKey
      | otherwise = Map.Lazy.mapEitherWithKey
{-# INLINABLE mapEitherWithKey #-}
{-# SPECIALIZE mapEitherWithKey :: Proxy# 'Strict -> (k -> a -> Either b c) -> POMap k a -> (POMap k b, POMap k c) #-}
{-# SPECIALIZE mapEitherWithKey :: Proxy# 'Lazy -> (k -> a -> Either b c) -> POMap k a -> (POMap k b, POMap k c) #-}

-- TODO: Maybe `split*` variants, returning a triple, but that would
-- be rather inefficient anyway.

--
-- * Submap
--

-- | \(\mathcal{O}(n_2 w_1 n_1 \log n_1)\).
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: (PartialOrd k, Eq v) => POMap k v -> POMap k v -> Bool
isSubmapOf = isSubmapOfBy (==)
{-# INLINABLE isSubmapOf #-}

{- | \(\mathcal{O}(n_2 w_1 n_1 \log n_1)\).
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

 >>> isSubmapOfBy (==) (fromList [(1,'a')]) (fromList [(1,'a'),(2,'b')])
 True
 >>> isSubmapOfBy (<=) (fromList [(1,'a')]) (fromList [(1,'b'),(2,'c')])
 True
 >>> isSubmapOfBy (==) (fromList [(1,'a'),(2,'b')]) (fromList [(1,'a'),(2,'b')])
 True

 But the following are all 'False':

 >>> isSubmapOfBy (==) (fromList [(2,'a')]) (fromList [(1,'a'),(2,'b')])
 False
 >>> isSubmapOfBy (<)  (fromList [(1,'a')]) (fromList [(1,'a'),(2,'b')])
 False
 >>> isSubmapOfBy (==) (fromList [(1,'a'),(2,'b')]) (fromList [(1,'a')])
 False
-}
isSubmapOfBy :: (PartialOrd k) => (a -> b -> Bool) -> POMap k a -> POMap k b -> Bool
isSubmapOfBy f s m
  = all (\(k, v) -> fmap (f v) (lookup k m) == Just True)
  . toList
  $ s
{-# INLINABLE isSubmapOfBy #-}

-- | \(\mathcal{O}(n_2 w_1 n_1 \log n_1)\).
-- Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (PartialOrd k, Eq v) => POMap k v -> POMap k v -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)
{-# INLINABLE isProperSubmapOf #-}

{- | \(\mathcal{O}(n_2 w_1 n_1 \log n_1)\).
 Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  >>> isProperSubmapOfBy (==) (fromList [(1,'a')]) (fromList [(1,'a'),(2,'b')])
  True
  >>> isProperSubmapOfBy (<=) (fromList [(1,'a')]) (fromList [(1,'a'),(2,'b')])
  True

 But the following are all 'False':

  >>> isProperSubmapOfBy (==) (fromList [(1,'a'),(2,'b')]) (fromList [(1,'a'),(2,'b')])
  False
  >>> isProperSubmapOfBy (==) (fromList [(1,'a'),(2,'b')]) (fromList [(1,'a')])
  False
  >>> isProperSubmapOfBy (<)  (fromList [(1,'a')])         (fromList [(1,'a'),(2,'b')])
  False
-}
isProperSubmapOfBy :: (PartialOrd k) => (a -> b -> Bool) -> POMap k a -> POMap k b -> Bool
isProperSubmapOfBy f s m = size s < size m && isSubmapOfBy f s m
{-# INLINABLE isProperSubmapOfBy #-}

--
-- * Min/Max
--

-- | \(\mathcal{O}(w\log n)\).
-- The minimal keys of the map.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupMin (fromList [(6,"a"), (3,"b")])
-- [(3,"b")]
-- >>> lookupMin empty
-- []
lookupMin :: PartialOrd k => POMap k v -> [(k, v)]
lookupMin = dedupAntichain LessThan . Maybe.mapMaybe Map.lookupMin . chainDecomposition
{-# INLINABLE lookupMin #-}

-- | \(\mathcal{O}(w\log n)\).
-- The maximal keys of the map.
--
-- Note that the following examples assume the @Divisibility@
-- partial order defined at the top.
--
-- >>> lookupMax (fromList [(6,"a"), (3,"b")])
-- [(6,"a")]
-- >>> lookupMax empty
-- []
lookupMax :: PartialOrd k => POMap k v -> [(k, v)]
lookupMax = dedupAntichain GreaterThan . Maybe.mapMaybe Map.lookupMax . chainDecomposition
{-# INLINABLE lookupMax #-}
