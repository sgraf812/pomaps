import qualified Data.POMap.Properties
import qualified Data.POMap.Strictness
import qualified Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  props <- testSpec "properties" (parallel Data.POMap.Properties.spec)
  strict <- testSpec "strictness" (parallel Data.POMap.Strictness.spec)
  Test.Tasty.defaultMain $ Test.Tasty.testGroup "pomaps"
    [ props
    , strict
    ]
