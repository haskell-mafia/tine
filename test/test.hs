import           Orphanarium.Core.Main

import qualified Test.Tine.Data.Duration
import qualified Test.Tine.Data.Pin

main :: IO ()
main =
  orphanariumMain [
      Test.Tine.Data.Duration.tests
    , Test.Tine.Data.Pin.tests
    ]
