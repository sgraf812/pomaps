{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Algebra.PartialOrd
import           Control.Arrow      (first)
import           Control.DeepSeq
import           Criterion.Main
import qualified Data.POMap.Lazy    as L
import qualified Data.POMap.Strict  as S
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

main :: IO ()
main = defaultMain
  [ bgroup "insert"
      [ bgroup s
          [ env
            (pure . force . zip (randoms (mkStdGen 0) :: [Divisibility]) $ [1 :: Int .. n])
            (bench (show n) . whnf (foldr (uncurry insert) L.empty))
          | n <- [100, 1000, 2000]
          ]
      | (s, insert) <- [("Lazy", L.insert), ("Strict", S.insert)]
      ]
  ]
