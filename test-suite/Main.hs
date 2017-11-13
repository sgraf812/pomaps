-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import qualified Data.POMap.Properties
import qualified Data.POMap.Strictness
import           Test.Tasty.Hspec

main :: IO ()
main = do
    props <- testSpec "properties" (parallel Data.POMap.Properties.spec)
    strict <- testSpec "strictness" (parallel Data.POMap.Strictness.spec)
    Test.Tasty.defaultMain $ Test.Tasty.testGroup "pomaps"
      [ props
      , strict
      ]
