import           Disorder.Core.Main

import qualified Test.Tine.Process

main :: IO ()
main =
  disorderMain [
      Test.Tine.Process.tests
    ]
