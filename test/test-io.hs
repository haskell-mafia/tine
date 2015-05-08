import           Orphanarium.Core.Main

import qualified Test.IO.Tine.Process

main :: IO ()
main =
  orphanariumMain [
      Test.IO.Tine.Process.tests
    ]
