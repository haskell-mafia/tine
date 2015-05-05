import           Orphanarium.Core.Main

import qualified Test.Tine.Data.Pin

main :: IO ()
main =
  orphanariumMain [
      Test.Tine.Data.Pin.tests
    ]
