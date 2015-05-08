import           Orphanarium.Core.Main

import qualified Test.Tine.Process

main :: IO ()
main =
  orphanariumMain [
      Test.Tine.Process.tests
    ]
