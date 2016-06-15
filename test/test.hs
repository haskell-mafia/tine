import           Disorder.Core.Main

import qualified Test.Tine.Conduit
import qualified Test.Tine.Process

main :: IO ()
main =
  disorderMain [
      Test.Tine.Conduit.tests
    , Test.Tine.Process.tests
    ]
