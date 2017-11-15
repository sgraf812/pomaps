{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Algebra.PartialOrd
import           Control.Arrow      (first)
import           Control.DeepSeq
import           Criterion.Main
import qualified Data.POMap.Lazy    as L
import qualified Data.POMap.Strict  as S
import qualified Data.Vector        as V
import           System.Random

newtype Divisibility
  = Div { _unDiv :: Int }
  deriving (Eq, Num, Show, Read, NFData)

instance PartialOrd Divisibility where
  leq (Div a) (Div b) = b `mod` a == 0

instance Bounded Divisibility where
  minBound = Div 1
  maxBound = Div maxBound

instance Random Divisibility where
  randomR (Div l, Div h) = first Div . randomR (l, h)
  random = randomR (minBound, maxBound)

genElems :: Int -> [(Divisibility, Int)]
genElems n = zip (randoms (mkStdGen 0) :: [Divisibility]) [1 :: Int .. n]

main :: IO ()
main = defaultMain
  [ bgroup "insert"
      [ bgroup s
          [ env
            (pure (genElems n))
            (bench (show n) . whnf (foldr (uncurry insert) L.empty))
          | n <- [100, 1000, 2000]
          ]
      | (s, insert) <- [("Lazy", L.insert), ("Strict", S.insert)]
      ]
  , bgroup "lookup(present)"
      [ env
        (let elems = genElems n
             m = L.fromList elems
             k = fst (elems !! (length elems `div` 2))
         in pure (m, k))
        (\ ~(m, k) -> bench (show n) (whnf (L.lookup k) m))
      | n <- [100, 1000, 2000]
      ]
  , bgroup "lookup(absent)"
      [ env
        (let elems = genElems n
             m = L.fromList elems
             k = fst (random (mkStdGen (-1)))
         in pure (m, k))
        (\ ~(m, k) -> bench (show n) (whnf (L.lookup k) m))
      | n <- [100, 1000, 2000]
      ]
  , bgroup "Vector.lookup(present)"
      [ env
        (let elems = genElems n
             v = V.fromListN n elems
             k = fst (elems !! (length elems `div` 2))
         in pure (v, k))
        (\ ~(v, k) -> bench (show n) (whnf (V.find ((== k) . fst)) v))
      | n <- [100, 1000, 2000]
      ]
  , bgroup "Vector.lookup(absent)"
      [ env
        (let elems = genElems n
             v = V.fromListN n elems
             k = fst (random (mkStdGen (-1)))
         in pure (v, k))
        (\ ~(v, k) -> bench (show n) (whnf (V.find ((== k) . fst)) v))
      | n <- [100, 1000, 2000]
      ]
  ]
