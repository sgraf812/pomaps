-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import           Criterion.Main
import           Data.POMap.Internal

main :: IO ()
main = defaultMain
	[ bench "fromList" (whnf fromList exampleList)
	]
