import GHC.IO.Exception                   (ExitCode)
import Distribution.PackageDescription
import Distribution.Simple                hiding (packageName)
import Distribution.Simple.Setup
import System.Process
import System.Directory                   (createDirectoryIfMissing)

main :: IO ()
main =
    defaultMainWithHooks simpleUserHooks
    {
        preConf = buildDeps
      , buildHook = \pd _ _ _ -> let
                        name = packageName pd
                        lib = library pd in
                            case lib of
                                Nothing -> return ()
                                Just l -> buildLibrary name (libBuildInfo l)
    }


-- | Build static and shared libraries
buildLibrary :: String -> BuildInfo -> IO ()
buildLibrary name bi = do
    let outPath = outDir ++ "/" ++ outFile
        outDir  = "dist/build"
        outFile = findOpt "-o" defaultName (ldOptions bi)
        defaultName = "lib" ++ name ++ ".so"
    createDirectoryIfMissing True outDir
    buildSharedLibrary outPath bi
    return ()


-- | Build a shared library for loading into ghci
buildSharedLibrary :: String -> BuildInfo -> IO ExitCode
buildSharedLibrary outPath bi =
    rawSystem "g++" (
                     ["-shared", "-o", outPath]
                     ++
                     map ("-I"++) (includeDirs bi)
                     ++
                     map ("-L"++) (extraLibDirs bi)
                     ++ 
                     cSources bi
                     ++
                     map ("-l"++) (extraLibs bi)
                    )


buildDeps :: Args -> ConfigFlags -> IO HookedBuildInfo
buildDeps _ _ = do
    rawSystem "sh" ["./scripts/build-deps.sh"]
    return emptyHookedBuildInfo


packageName :: PackageDescription -> String
packageName pd = name pName
    where name (PackageName str) = str
          pName = pkgName pInf
          pInf = package pd


findOpt :: String -> String -> [String] -> String
findOpt _ def [] = def
findOpt opt def (x:xs) = if x == "-o" then head xs else findOpt opt def xs
