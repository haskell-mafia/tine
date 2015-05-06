import           Orphanarium.Core.Main

import qualified Test.IO.Tine.Snooze

main :: IO ()
main =
  orphanariumMain [
      Test.IO.Tine.Snooze.tests
    ]
