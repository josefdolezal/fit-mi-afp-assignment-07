import Test.Hspec

import qualified CalculatorSpec
import qualified Data.SortedListSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data.SortedList"  Data.SortedListSpec.spec
  describe "Calculator"       CalculatorSpec.spec
