{-# LANGUAGE CPP #-}

import Control.Monad (when, unless, mplus)

import Data.Maybe (listToMaybe, fromMaybe)
import Distribution.PackageDescription     
    ( PackageDescription, buildable, exeName, buildInfo, 
      executables, customFieldsBI, BuildInfo)

import Distribution.Verbosity              (normal)
import Distribution.Simple.Build           (build)
import Distribution.Simple.LocalBuildInfo  (LocalBuildInfo(..))
import Distribution.Simple.PreProcess      (knownSuffixHandlers)
import Distribution.Simple.Setup           (defaultBuildFlags)
import Distribution.Simple 
    ( Args, defaultMainWithHooks, UserHooks(..), simpleUserHooks) 

import System.Exit       (ExitCode(..))
import System.FilePath   ( (</>), splitDirectories, isAbsolute )
import System.IO         (openFile, IOMode (..))
import System.Process 
import System.Directory 
    ( getCurrentDirectory, createDirectoryIfMissing
    , setCurrentDirectory, findExecutable, canonicalizePath
    , removeFile, doesDirectoryExist
    )

main :: IO ()
main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' } 

testCraftwerk :: a -> (BuildInfo -> a) -> PackageDescription -> a
testCraftwerk dflt f pd = 
    fromMaybe dflt $ listToMaybe 
               [ f (buildInfo exe)
               | exe <- executables pd
               , exeName exe == "test-craftwerk" ]

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' args0 _unknown pd lbi = do
    let args = if null args0 then [] else "-t" : args0
        
    let testDir = buildDir lbi </> "test-craftwerk"
    baseDir <- getCurrentDirectory
    canonicalBuildDir <- canonicalizePath (buildDir lbi)
    t <- doesDirectoryExist testDir

    unless t $ do
        unless (testCraftwerk False buildable pd) $ do
          fail "Reconfigure with 'cabal configure -ftests' or 'cabal install -ftests' and try again."
        putStrLn "---[ Building Tests ]---"
        build pd lbi defaultBuildFlags knownSuffixHandlers
        putStrLn "---[ Tests Built ]---"

    setCurrentDirectory testDir

    exitcode <- system $ unwords $ ("./test-craftwerk") : args
    unless (exitcode == ExitSuccess) $ 
        fail "Test Failed"
