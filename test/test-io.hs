import           Disorder.Core.Main

import qualified Test.IO.Tine.Conduit
import qualified Test.IO.Tine.Process

main :: IO ()
main =
  disorderMain [
      Test.IO.Tine.Conduit.tests
    , Test.IO.Tine.Process.tests
    ]
