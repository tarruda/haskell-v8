import GHC.IO.Exception                   (ExitCode(ExitSuccess))
import Data.List                          (intersperse, elemIndices,
                                           splitAt, isSuffixOf)
import Data.Map                           (fromList, Map, insert,
                                           findWithDefault, foldlWithKey)
import Control.Monad                      (foldM)
import Distribution.PackageDescription
import Distribution.Simple                hiding (packageName)
import Distribution.Simple.Setup
import qualified System.FilePath.Glob as Glob
import System.Process
import Data.Time.Clock.POSIX              (utcTimeToPOSIXSeconds)
import System.Directory                   (createDirectoryIfMissing,
                                           doesFileExist,
                                           getModificationTime)

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


-- | Build static library composed of cbits + native library
buildLibrary :: String -> BuildInfo -> IO ()
buildLibrary name bi = do
    (srcs, hasCpp) <- sources
    let outDir         = "dist/build/objects"
        incDirs        = map ("-I"++) (includeDirs bi)
        tsFile         = ".compilation-timestamps"
        commands       = map (compileCommand incDirs outDir) srcs
        objects        = map (\(_, args) -> findOpt "-o" "" args) commands
    updatedTimestamps <- compile tsFile commands
    writeFile tsFile (serializeTimestamps updatedTimestamps)
    linkAll hasCpp bi name objects
    return ()


compile :: String -> [(String, [String])] -> IO (Map String Integer)
compile tsFile commands = do
    exists <- doesFileExist tsFile
    if exists
        then do
            tsContents <- readFile tsFile
            let timestamps = parseTimestamps tsContents
            compileModified timestamps commands
        else
            compileModified (fromList []) commands


compileModified :: Map String Integer ->
                   [(String, [String])] ->
                   IO (Map String Integer)
compileModified timestamps commands =
    foldM exec timestamps commands
    where
        exec timestamps (cmd, args) = do
            let (outDir, _) = splitLast '/' outFile
                outFile     = findOpt "-o" "" args
                srcFile     = last args
            outExists <- doesFileExist outFile
            ts <- getModificationTime srcFile
            let pt = round (utcTimeToPOSIXSeconds ts)
            createDirectoryIfMissing True outDir
            if pt /= findWithDefault 0 srcFile timestamps || not outExists
                then do
                    putStrLn $ "Compile: " ++ srcFile ++ " -> " ++ outFile
                    exitStatus <- rawSystem cmd args
                    if exitStatus == ExitSuccess
                        then return $ insert srcFile pt timestamps
                        else return timestamps
                else
                    return timestamps


linkAll :: Bool -> BuildInfo -> String -> [String] -> IO ExitCode
linkAll hasCpp bi name objects = do
    let outDir         = "dist/build"
        outLinked      = outDir ++ "/HS" ++ name ++ ".o"
        outShared      = outDir ++ "/libHS" ++ name ++ ".so"
        outStatic      = outDir ++ "/libHS" ++ name ++ ".a"
        libDirs        = map ("-L"++) (extraLibDirs bi)
        extraLibraries = map ("-l"++) (extraLibs bi)
    -- rawSystem "ar"  ( -- static library
    --                 ["cqs", "-o", outStatic] ++

    --                 )
    rawSystem "ghc" ( -- shared library
                    ["-shared", "-o", outShared, "-lHSrts"] ++
                    (if hasCpp then ["-lstdc++"] else []) ++
                    libDirs ++
                    extraLibraries ++
                    objects
                    )


sources :: IO ([String], Bool)
sources = do
    nativeSources <- Glob.globDir [cFiles, cppFiles] "cbits"
    haskellSources <- Glob.globDir [hsFiles] "src"
    let matched (xs, _) = concat xs
        rv = concat [matched nativeSources, matched haskellSources]
        hasCpp = any isCpp rv
        isCpp = isSuffixOf ".cpp"
    return (rv, hasCpp)


buildDeps :: Args -> ConfigFlags -> IO HookedBuildInfo
buildDeps _ _ = do
    rawSystem "sh" ["./scripts/build-deps.sh"]
    return emptyHookedBuildInfo



-- pure functions

serializeTimestamps :: Map String Integer -> String
serializeTimestamps = foldlWithKey folder "" where
    folder result fname modified =
        result ++ fname ++ " " ++ show modified ++ "\n"


parseTimestamps :: String -> Map String Integer
parseTimestamps = fromList . ((map (parseEntry . words)) . lines) where
    parseEntry (x:y:_) = (x, read y :: Integer)
    parseEntry _ = ("", 0)


compileCommand :: [String] -> String -> String -> (String, [String])
compileCommand includeDirs outDir filename =
    (compiler, compilerArgs ++ ["-fPIC", "-c", "-o", object, filename])
    where
        (compiler, object) = compilerAndObject outDir filename
        compilerArgs | compiler /= "ghc" = includeDirs
                     | otherwise =
                         [
                           "-ohi"
                         , interface
                         , "-hidir"
                         , outDir ++ "/src"
                         , "-split-objs"
                         ]
                         where
                             interface = oDir ++ name ++ ".hi"
                             (oDir, nameExt) = splitLast '/' object
                             (name, _) = splitLast '.' nameExt


dirname :: String -> String
dirname str = let (dir, _) = splitLast '/' str in dir


compilerAndObject :: String -> String -> (String, String)
compilerAndObject outDir filename = (compiler, object) where
   compiler = case extension of
                  ".cpp" -> "g++"
                  ".hs"  -> "ghc"
                  _      -> "gcc"
   object = outDir ++ "/" ++ oDir ++ oFile ++ ".o"
   (oDir, oFile) = splitLast '/' file
   (file, extension) = splitLast '.' filename


splitLast :: Char -> String -> (String, String)
splitLast char str = let indices = elemIndices char str
                in case length indices of
                    0 -> (str, "")
                    _ -> splitAt (last indices) str


packageName :: PackageDescription -> String
packageName pd = name pName ++ "-" ++ pVer
    where name (PackageName str) = str
          pName    = pkgName pInf
          pVersion = pkgVersion pInf
          pBranch  = map show (versionBranch pVersion)
          pVer     = concat $ intersperse "." pBranch
          pInf     = package pd


findOpt :: String -> String -> [String] -> String
findOpt _ def [] = def
findOpt opt def (x:xs) = if x == "-o" then head xs else findOpt opt def xs


cFiles :: Glob.Pattern
cFiles = Glob.compile "**/*.c"


cppFiles :: Glob.Pattern
cppFiles = Glob.compile "**/*.cpp"


hsFiles :: Glob.Pattern
hsFiles = Glob.compile "**/*.hs"
