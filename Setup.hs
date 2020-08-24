import Distribution.Simple
main = defaultMain

{-
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Types.LocalBuildInfo

main = defaultMainWithHooks (simpleUserHooks{ postConf = buildCRuntime, buildHook = buildPre })

buildCRuntime :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO () --HookedBuildInfo
buildCRuntime args cflags _ _
  = do putStrLn "postConf"
       return () --emptyHookedBuildInfo

buildPre :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildPre packageDesc localBuildInfo userHooks buildFlags
 = do putStrLn "buildHook"
      return ()
-}
