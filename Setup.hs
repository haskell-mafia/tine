import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import Distribution.Verbosity

main = do defaultMainWithHooks simpleUserHooks {
    buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do
      buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
  }
