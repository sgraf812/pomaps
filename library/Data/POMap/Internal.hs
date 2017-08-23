{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.POMap.Internal where

import           Algebra.PartialOrd
import           Control.Arrow      (first, second, (&&&), (***))
import qualified Data.List          as List
import           Data.Map.Internal  (AreWeStrict (..), Map (..))
import qualified Data.Map.Internal  as Map
import           Data.Maybe         (fromMaybe)
import qualified Data.Maybe         as Maybe
import           Data.Monoid        (Alt (..), Any (..))
import           Debug.Trace
import           GHC.Exts           (Proxy#, inline, proxy#)
import qualified GHC.Exts
import           GHC.Magic          (oneShot)
import           GHC.TypeLits
import           Prelude            hiding (lookup, map)

class SingIAreWeStrict (s :: AreWeStrict) where
  areWeStrict :: Proxy# s -> AreWeStrict

instance SingIAreWeStrict 'Strict where
  areWeStrict _ = Strict

instance SingIAreWeStrict 'Lazy where
  areWeStrict _ = Lazy

seq' :: SingIAreWeStrict s => Proxy# s -> a -> b -> b
seq' p a b
  | Lazy <- areWeStrict p = b
  | otherwise = seq a b
{-# INLINE seq' #-}

data POMap k v
  = POMap !Int ![Map k v]
  deriving (Show, Read) -- TODO: Implement these by hand


mkPOMap :: [Map k v] -> POMap k v
mkPOMap decomp = POMap (List.foldr ((+) . Map.size) 0 decomp) decomp
{-# INLINE mkPOMap #-}


chainDecomposition :: POMap k v -> [Map k v]
chainDecomposition (POMap _ cd) = cd

--
-- * Instances
--

{-# INLINE chainDecomposition #-}
instance Functor (POMap k) where
  fmap = map
  a <$ (POMap n d) = POMap n (fmap (a <$) d)

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
  traverse f = traverseWithKey (const f)
  {-# INLINE traverse #-}

instance PartialOrd k => GHC.Exts.IsList (POMap k v) where
  type Item (POMap k v) = (k, v)
  fromList = fromList (proxy# :: Proxy# 'Lazy)
  toList = toList

--
-- * Query
--

size :: POMap k v -> Int
size (POMap s _) = s
{-# INLINE size #-}

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

lookup :: PartialOrd k => k -> POMap k v -> Maybe v
lookup !k = getAlt . foldEntry k pure
{-# INLINABLE lookup #-}

member :: PartialOrd k => k -> POMap k v -> Bool
member !k = getAny . foldEntry k (const (Any True))
{-# INLINABLE member #-}

notMember :: PartialOrd k => k -> POMap k v -> Bool
notMember k = not . member k
{-# INLINABLE notMember #-}

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
          | containsOrdering GT op -> mayChain' -- don't need e'
          | containsOrdering LT op -> Nothing
        Just GT
          | containsOrdering GT op -> Nothing
          | containsOrdering LT op -> mayChain' -- don't need e'
        Just EQ -> Nothing -- should never happen
        _ -> (e' :) <$> mayChain' -- still need e'
{-# INLINE addToAntichain #-}

-- If inlined, this optimizes to the equivalent hand-written variants.
lookupX :: PartialOrd k => RelationalOperator -> k -> POMap k v -> [(k, v)]
lookupX !op !k
  = foldr (addToAntichain op) []
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

lookupLT :: PartialOrd k => k -> POMap k v -> [(k, v)]
lookupLT = inline lookupX LessThan
{-# INLINABLE lookupLT #-}

lookupLE :: PartialOrd k => k -> POMap k v -> [(k, v)]
lookupLE = inline lookupX LessEqual
{-# INLINABLE lookupLE #-}

lookupGE :: PartialOrd k => k -> POMap k v -> [(k, v)]
lookupGE = inline lookupX GreaterEqual
{-# INLINABLE lookupGE #-}

lookupGT :: PartialOrd k => k -> POMap k v -> [(k, v)]
lookupGT = inline lookupX GreaterThan
{-# INLINABLE lookupGT #-}


--
-- * Construction
--

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

delete :: PartialOrd k => k -> POMap k v -> POMap k v
delete = inline update (proxy# :: Proxy# 'Lazy) (const Nothing)
{-# INLINABLE delete #-}

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
alterWithKey s f k = mkPOMap . overChains handleChain oldWon newWon incomparable
  where
    handleChain = alterChain s f k
    oldWon chain chains' = chain : chains'
    newWon chain' chains = chain' : chains
    incomparable decomp =
      case f k Nothing of
        Nothing -> decomp
        Just v  -> Map.singleton k v : decomp
{-# INLINABLE alterWithKey #-}
{-# SPECIALIZE alterWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v #-}
{-# SPECIALIZE alterWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> POMap k v #-}

alterChain :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> Maybe v -> Maybe v) -> k -> Map k v -> LookupResult (Map k v)
alterChain s f !k = go
  where
    go Tip = NotFound $ case f k Nothing of
      Just v  -> seq' s v $ Map.singleton k v
      Nothing -> Tip
    go (Bin n k' v' l r) =
      case (k `leq` k', k' `leq` k) of
        (True, True) -> Found $ case f k (Just v') of
          Just v  -> seq' s v $ Bin n k' v l r
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
        Just v  -> Map.singleton k v : decomp)
{-# INLINABLE alterLookupWithKey #-}
{-# SPECIALIZE alterLookupWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v) #-}
{-# SPECIALIZE alterLookupWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> Maybe v -> Maybe v) -> k -> POMap k v -> (Maybe v, POMap k v) #-}

alterLookupChain :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> Maybe v -> Maybe v) -> k -> Map k v -> LookupResult (Maybe v, Map k v)
alterLookupChain s f !k = go
  where
    go Tip = NotFound (Nothing, case f k Nothing of
      Just v  -> seq' s v $ Map.singleton k v
      Nothing -> Tip)
    go (Bin n k' v' l r) =
      case (k `leq` k', k' `leq` k) of
        (True, True) -> Found (Just v', case f k (Just v') of
          Just v  -> seq' s v $ Bin n k' v l r
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
alterF s f k = fmap mkPOMap . overChains handleChain oldWon newWon incomparable
  where
    handleChain = alterFChain s k
    -- prepends the unaltered chain to the altered tail
    oldWon chain altered = fmap (chain:) altered
    -- prepends the altered chain to the unaltered tail
    newWon alt chains = fmap (:chains) (alt f)
    (<#>) = flip (<$>)
    -- prepends a new chain in the incomparable case if
    -- the alteration function produces a value
    incomparable decomp = f Nothing <#> \mv ->
      case mv of
        Just v  -> Map.singleton k v : decomp
        Nothing -> decomp
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
alterFChain s !k = go
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
      ret NotFound Nothing . oneShot $ \mv ->
        case mv of
          Just v  -> seq' s v $ Map.singleton k v
          Nothing -> Tip
    go (Bin n k' v l r) =
      case (k `leq` k', k' `leq` k) of
        (True, True)   ->
          ret Found (Just v) . oneShot $ \mv ->
            case mv of
              Just v' -> seq' s v $ Bin n k v' l r
              Nothing -> Tip
        (True, False)  -> lift (go l) . oneShot $ \l' -> Map.balanceL k' v l' r
        (False, True)  -> lift (go r) . oneShot $ \r' -> Map.balanceL k' v l r'
        (False, False) -> Incomparable

--
-- * Combine
--

-- ** Union

union :: PartialOrd k => POMap k v -> POMap k v -> POMap k v
union = inline unionWith const
{-# INLINABLE union #-}

unionWith :: PartialOrd k => (v -> v -> v) -> POMap k v -> POMap k v -> POMap k v
unionWith f = inline unionWithKey (const f)
{-# INLINABLE unionWith #-}

-- | /O(yolo)/ - Do not use this if you expect performance.
--
-- More realistically, this runs in /O(w*n^2*log n)/,
-- e.g. the most naive way possible.
unionWithKey :: PartialOrd k => (k -> v -> v -> v) -> POMap k v -> POMap k v -> POMap k v
unionWithKey f l r = List.foldl' (\m (k, v) -> inline insertWithKey (proxy# :: Proxy# 'Lazy) f k v m) r (toList l)
{-# INLINABLE unionWithKey #-}

unions :: PartialOrd k => [POMap k v] -> POMap k v
unions = inline unionsWith const
{-# INLINABLE unions #-}

unionsWith :: PartialOrd k => (v -> v -> v) -> [POMap k v] -> POMap k v
unionsWith f = List.foldl' (unionWith f) empty
{-# INLINABLE unionsWith #-}

-- * Difference

difference :: PartialOrd k => POMap k a -> POMap k b -> POMap k a
difference = inline differenceWith (\_ _ -> Nothing)
{-# INLINABLE difference #-}

differenceWith :: PartialOrd k => (a -> b -> Maybe a) -> POMap k a -> POMap k b -> POMap k a
differenceWith f = inline differenceWithKey (const f)
{-# INLINABLE differenceWith #-}

-- | /O(yolo)/ - Do not use this if you expect performance.
--
-- More realistically, this runs in /O(w*n^2*log n)/,
-- e.g. the most naive way possible.
differenceWithKey :: PartialOrd k => (k -> a -> b -> Maybe a) -> POMap k a -> POMap k b -> POMap k a
differenceWithKey f l
  = List.foldl' (\m (k, v) -> inline alterWithKey (proxy# :: Proxy# 'Lazy) (f' v) k m) l
  . toList
  where
    f' _ _ Nothing   = Nothing
    f' v k (Just v') = f k v' v
{-# INLINABLE differenceWithKey #-}

-- ** Intersection

intersection :: PartialOrd k => POMap k a -> POMap k b -> POMap k a
intersection = inline intersectionWith const
{-# INLINABLE intersection #-}

intersectionWith :: PartialOrd k => (a -> b -> c) -> POMap k a -> POMap k b -> POMap k c
intersectionWith f = inline intersectionWithKey (const f)
{-# INLINABLE intersectionWith #-}

-- | /O(yolo)/ - Do not use this if you expect performance.
--
-- More realistically, this runs in /O(w*n^2*log n)/,
-- e.g. the most naive way possible.
intersectionWithKey :: PartialOrd k => (k -> a -> b -> c) -> POMap k a -> POMap k b -> POMap k c
intersectionWithKey f l
  = fromList (proxy# :: Proxy# 'Lazy)
  . Maybe.mapMaybe (\(k,b) -> [(k, f k a b) | a <- lookup k l])
  . toList
{-# INLINABLE intersectionWithKey #-}


-- * Traversals

map :: (a -> b) -> POMap k a -> POMap k b
map f (POMap n chains) = POMap n (fmap (fmap f) chains)

{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
 #-}

mapWithKey :: (k -> a -> b) -> POMap k a -> POMap k b
mapWithKey f (POMap n d) = POMap n (fmap (Map.mapWithKey f) d)

{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs . mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs . mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs . map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
 #-}

traverseWithKey :: Applicative t => (k -> a -> t b) -> POMap k a -> t (POMap k b)
traverseWithKey f (POMap n d) = POMap n <$> traverse (Map.traverseWithKey f) d
{-# INLINE traverseWithKey #-}

mapAccum :: (a -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c)
mapAccum f = inline mapAccumWithKey (\a _ b -> f a b)

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> POMap k b -> (a, POMap k c)
mapAccumWithKey f acc (POMap n chains) = (acc', POMap n chains')
  where
    (acc', chains') = List.mapAccumL (Map.mapAccumWithKey f) acc chains

mapKeys :: PartialOrd k2 => (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeys f = fromList (proxy# :: Proxy# 'Lazy) . fmap (first f) . toList

mapKeysWith :: PartialOrd k2 => (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeysWith c f = fromListWith (proxy# :: Proxy# 'Lazy) c . fmap (first f) . toList

mapKeysMonotonic :: (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeysMonotonic f (POMap n d) = POMap n (fmap (Map.mapKeysMonotonic f) d)

--
-- * Folds
--

foldr' :: (a -> b -> b) -> b -> POMap k a -> b
foldr' f acc = List.foldr (flip (Map.foldr' f)) acc . chainDecomposition
{-# INLINE foldr' #-}

foldrWithKey :: (k -> a -> b -> b) -> b -> POMap k a -> b
foldrWithKey f acc = List.foldr (flip (Map.foldrWithKey f)) acc . chainDecomposition
{-# INLINE foldrWithKey #-}

foldrWithKey' :: (k -> a -> b -> b) -> b -> POMap k a -> b
foldrWithKey' f acc = List.foldr (flip (Map.foldrWithKey' f)) acc . chainDecomposition
{-# INLINE foldrWithKey' #-}

foldl' :: (b -> a -> b) -> b -> POMap k a -> b
foldl' f acc = List.foldl' (Map.foldl' f) acc . chainDecomposition
{-# INLINE foldl' #-}

foldlWithKey :: (b -> k -> a -> b) -> b -> POMap k a -> b
foldlWithKey f acc = List.foldl (Map.foldlWithKey f) acc . chainDecomposition
{-# INLINE foldlWithKey #-}

foldlWithKey' :: (b -> k -> a -> b) -> b -> POMap k a -> b
foldlWithKey' f acc = List.foldl' (Map.foldlWithKey' f) acc . chainDecomposition
{-# INLINE foldlWithKey' #-}

foldMapWithKey :: Monoid m => (k -> a -> m) -> POMap k a -> m
foldMapWithKey f = foldMap (Map.foldMapWithKey f ) . chainDecomposition
{-# INLINE foldMapWithKey #-}

-- * Conversion

elems :: POMap k v -> [v]
elems = concatMap Map.elems . chainDecomposition

keys :: POMap k v -> [k]
keys = concatMap Map.keys . chainDecomposition

assocs :: POMap k v -> [(k, v)]
assocs = concatMap Map.toList . chainDecomposition

toList :: POMap k v -> [(k, v)]
toList = assocs

-- TODO: keysSet, fromSet

fromList :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> [(k, v)] -> POMap k v
fromList s = List.foldr (uncurry (insert s)) empty
{-# INLINABLE fromList #-}
{-# SPECIALIZE fromList :: PartialOrd k => Proxy# 'Strict -> [(k, v)] -> POMap k v #-}
{-# SPECIALIZE fromList :: PartialOrd k => Proxy# 'Lazy -> [(k, v)] -> POMap k v #-}

fromListWith :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (v -> v -> v) -> [(k, v)] -> POMap k v
fromListWith s f = List.foldr (uncurry (insertWith s f)) empty
{-# INLINABLE fromListWith #-}
{-# SPECIALIZE fromListWith :: PartialOrd k => Proxy# 'Strict -> (v -> v -> v) -> [(k, v)] -> POMap k v #-}
{-# SPECIALIZE fromListWith :: PartialOrd k => Proxy# 'Lazy -> (v -> v -> v) -> [(k, v)] -> POMap k v #-}

fromListWithKey :: (PartialOrd k, SingIAreWeStrict s) => Proxy# s -> (k -> v -> v -> v) -> [(k, v)] -> POMap k v
fromListWithKey s f = List.foldr (uncurry (insertWithKey s f)) empty
{-# INLINABLE fromListWithKey #-}
{-# SPECIALIZE fromListWithKey :: PartialOrd k => Proxy# 'Strict -> (k -> v -> v -> v) -> [(k, v)] -> POMap k v #-}
{-# SPECIALIZE fromListWithKey :: PartialOrd k => Proxy# 'Lazy -> (k -> v -> v -> v) -> [(k, v)] -> POMap k v #-}

--
-- * Filter
--

filter :: (v -> Bool) -> POMap k v -> POMap k v
filter p = filterWithKey (const p)

filterWithKey :: (k -> v -> Bool) -> POMap k v -> POMap k v
filterWithKey p (POMap _ d) = mkPOMap (Map.filterWithKey p <$> d)

-- TODO: restrictKeys, withoutKeys

partition :: (v -> Bool) -> POMap k v -> (POMap k v, POMap k v)
partition p = partitionWithKey (const p)

partitionWithKey :: (k -> v -> Bool) -> POMap k v -> (POMap k v, POMap k v)
partitionWithKey p (POMap _ d)
  = (mkPOMap *** mkPOMap)
  . unzip
  . fmap (Map.partitionWithKey p)
  $ d

mapMaybe :: (a -> Maybe b) -> POMap k a -> POMap k b
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey :: (k -> a -> Maybe b) -> POMap k a -> POMap k b
mapMaybeWithKey f (POMap _ d) = mkPOMap (Map.mapMaybeWithKey f <$> d)

traverseMaybeWithKey :: Applicative f => (k -> a -> f (Maybe b)) -> POMap k a -> f (POMap k b)
traverseMaybeWithKey f (POMap _ d) = mkPOMap <$> traverse (Map.traverseMaybeWithKey f) d

mapEither :: (a -> Either b c) -> POMap k a -> (POMap k b, POMap k c)
mapEither p = mapEitherWithKey (const p)

mapEitherWithKey :: (k -> a -> Either b c) -> POMap k a -> (POMap k b, POMap k c)
mapEitherWithKey p (POMap _ d)
  = (mkPOMap *** mkPOMap)
  . unzip
  . fmap (Map.mapEitherWithKey p)
  $ d

-- TODO: Maybe `split*` variants, returning a triple, but that would
-- be rather inefficient anyway.

--
-- * Submap
--

isSubmapOf :: (PartialOrd k, Eq v) => POMap k v -> POMap k v -> Bool
isSubmapOf = isSubmapOfBy (==)
{-# INLINABLE isSubmapOf #-}

isSubmapOfBy :: (PartialOrd k) => (a -> b -> Bool) -> POMap k a -> POMap k b -> Bool
isSubmapOfBy f s m
  = all (\(k, v) -> fmap (f v) (lookup k m) == Just True)
  . toList
  $ s
{-# INLINABLE isSubmapOfBy #-}

isProperSubmapOf :: (PartialOrd k, Eq v) => POMap k v -> POMap k v -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)
{-# INLINABLE isProperSubmapOf #-}

isProperSubmapOfBy :: (PartialOrd k) => (a -> b -> Bool) -> POMap k a -> POMap k b -> Bool
isProperSubmapOfBy f s m = size s < size m && isSubmapOfBy f s m
{-# INLINABLE isProperSubmapOfBy #-}

--
-- * Min/Max
--

lookupMin :: POMap k v -> [(k, v)]
lookupMin = Maybe.mapMaybe Map.lookupMin . chainDecomposition
{-# INLINABLE lookupMin #-}

lookupMax :: POMap k v -> [(k, v)]
lookupMax = Maybe.mapMaybe Map.lookupMax . chainDecomposition
{-# INLINABLE lookupMax #-}



newtype Mod (n :: Nat)
  = Mod { unMod :: Integer }
  deriving Eq

instance Show (Mod n)  where
  show = show . unMod

instance Read (Mod n) where
  readsPrec p = fmap (first Mod) . readsPrec p

instance KnownNat n => PartialOrd (Mod n) where
  comparable a b = (unMod a `mod` natVal a) == (unMod b `mod` natVal b)
  leq a b
    | comparable a b = unMod a <= unMod b
    | otherwise = False

example :: POMap (Mod 3) ()
example = mkPOMap
  [ bin 10 (bin 4 (leaf 1) Tip) (leaf 13)
  , bin 5 (leaf 2) (leaf 80)
  , bin 9 (leaf 0) (bin 15 Tip (leaf 63))
  ]
  where
    bin k = Map.bin (Mod k) ()
    leaf k = Map.singleton (Mod k) ()

example2 :: POMap (Mod 3) ()
example2 = fromList (proxy# :: Proxy# 'Lazy) exampleList

exampleList :: [(Mod 3, ())]
exampleList = fmap ((id &&& const ()) . Mod) [ 0,1,2,4,63,15,80,9,5,13,10]
