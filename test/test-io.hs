import           Disorder.Core.Main

import qualified Test.IO.Tine.Process

main :: IO ()
main =
  disorderMain [
      Test.IO.Tine.Process.tests
    ]
