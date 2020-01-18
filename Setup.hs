import Distribution.Types.HookedBuildInfo
import Distribution.Simple
import System.Process

main = defaultMainWithHooks hooks where
  hooks = simpleUserHooks {
    preBuild = \_ a -> do
      system "make -C rts/"
      pure emptyHookedBuildInfo
  }

