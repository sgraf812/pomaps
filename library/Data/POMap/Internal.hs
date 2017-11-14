{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE TypeFamilies        #-}

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
import           Prelude            hiding (lookup, map)
import           Text.Read          (Lexeme (Ident), Read (..), lexP, parens,
                                     prec, readListPrecDefault)

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

seqList :: [a] -> [a]
seqList xs = foldr seq xs xs

data POMap k v = POMap !Int ![Map k v]

type role POMap nominal representational

mkPOMap :: [Map k v] -> POMap k v
mkPOMap decomp = POMap (foldr ((+) . Map.size) 0 decomp) (seqList decomp)
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
    return (fromList (proxy# :: Proxy# 'Lazy) xs)

  readListPrec = readListPrecDefault

-- | /O(n^2)/.
instance (PartialOrd k, Eq v) => Eq (POMap k v) where
  a == b
    | size a /= size b = False
    | otherwise = isSubmapOf a b && isSubmapOf b a

instance (NFData k, NFData v) => NFData (POMap k v) where
  rnf (POMap _ d) = rnf d

instance PartialOrd k => GHC.Exts.IsList (POMap k v) where
  type Item (POMap k v) = (k, v)
  fromList = fromList (proxy# :: Proxy# 'Lazy)
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
    incomparable decomp = f Nothing <#> \mv ->
      case mv of
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
      ret NotFound Nothing . oneShot $ \mv ->
        case mv of
          Just v  -> seq' s v (Map.singleton k v)
          Nothing -> Tip
    go (Bin n k' v l r) =
      case (k `leq` k', k' `leq` k) of
        (True, True)   ->
          ret Found (Just v) . oneShot $ \mv ->
            case mv of
              Just v' -> seq' s v' (Bin n k v' l r)
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

mapKeys :: PartialOrd k2 => (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeys f = fromList (proxy# :: Proxy# 'Lazy) . fmap (first f) . toList

mapKeysWith :: (PartialOrd k2, SingIAreWeStrict s) => Proxy# s -> (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeysWith s c f = fromListWith s c . fmap (first f) . toList
{-# INLINABLE mapKeysWith #-}
{-# SPECIALIZE mapKeysWith :: PartialOrd k2 => Proxy# 'Strict -> (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v #-}
{-# SPECIALIZE mapKeysWith :: PartialOrd k2 => Proxy# 'Lazy -> (v -> v -> v) -> (k1 -> k2) -> POMap k1 v -> POMap k2 v #-}

mapKeysMonotonic :: (k1 -> k2) -> POMap k1 v -> POMap k2 v
mapKeysMonotonic f (POMap _ d) = mkPOMap (fmap (Map.mapKeysMonotonic f) d)

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

lookupMin :: PartialOrd k => POMap k v -> [(k, v)]
lookupMin = dedupAntichain LessThan . Maybe.mapMaybe Map.lookupMin . chainDecomposition
{-# INLINABLE lookupMin #-}

lookupMax :: PartialOrd k => POMap k v -> [(k, v)]
lookupMax = dedupAntichain GreaterThan . Maybe.mapMaybe Map.lookupMax . chainDecomposition
{-# INLINABLE lookupMax #-}
