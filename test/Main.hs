module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Streamly.Compat.OsPath (fromPath, toPath, unsafeToPath)
import Streamly.Internal.Data.Array (Array)
import Streamly.FileSystem.Path (Path, OsWord)
import System.Exit (exitFailure)
import System.OsPath (OsPath)

import qualified Streamly.Compat.OsString as OsString
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified System.OsPath as OsPath

-------------------------------------------------------------------------------
-- Test helpers
-------------------------------------------------------------------------------

check :: IORef Int -> String -> Bool -> IO ()
check failures name ok =
    unless ok $ do
        putStrLn $ "FAILED: " ++ name
        modifyIORef' failures (+ 1)

osp :: String -> IO OsPath
osp = OsPath.encodeUtf

-- | Copy an array to a new buffer that has spare capacity at the end.
arrWithCapacity :: Array OsWord -> IO (Array OsWord)
arrWithCapacity arr = do
    marr <-
        MutArray.emptyOf (Array.length arr + 64)
            :: IO (MutArray.MutArray OsWord)
    marr1 <- MutArray.splice marr (Array.unsafeThaw arr)
    return $ Array.unsafeFreeze marr1

-- | Copy an array to a new buffer at a non-zero offset.
arrWithOffset :: Array OsWord -> Array OsWord
arrWithOffset arr =
    let prefix = Path.toArray (Path.fromString_ "xyz")
        arr1 = Array.fromList (Array.toList prefix ++ Array.toList arr)
     in snd (Array.unsafeBreakAt (Array.length prefix) arr1)

-- | Create a Path from an array that has spare capacity at the end of its
-- buffer.
withCapacity :: Path -> IO Path
withCapacity p =
    Path.unsafeFromArray <$> arrWithCapacity (Path.toArray p)

-- | Create a Path from an array that does not start at the beginning of its
-- buffer.
withOffset :: Path -> Path
withOffset = Path.unsafeFromArray . arrWithOffset . Path.toArray

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

samples :: [String]
samples =
    [ "/"
    , "."
    , "a"
    , "/usr/local/bin"
    , "relative/path/file.txt"
    , "dir/"
    , "\x00e9t\x00e9/\x4e2d\x6587/\x1f600"
    ]

testSample :: IORef Int -> String -> IO ()
testSample failures s = do
    o <- osp s
    let p = Path.fromString_ s
        name x = x ++ " " ++ show s

    -- OsPath -> Path
    check failures (name "unsafeToPath toString")
        (Path.toString (unsafeToPath o) == s)
    r <- try (toPath o) :: IO (Either SomeException Path)
    check failures (name "toPath toString")
        (either (const False) ((== s) . Path.toString) r)

    -- Path -> OsPath
    check failures (name "fromPath") (fromPath p == o)
    pc <- withCapacity p
    check failures (name "fromPath with capacity") (fromPath pc == o)
    check failures (name "fromPath with offset") (fromPath (withOffset p) == o)

    -- Round trips
    check failures (name "round trip OsPath")
        (fromPath (unsafeToPath o) == o)
    check failures (name "round trip Path")
        (Path.toString (unsafeToPath (fromPath p)) == s)

-- | OsString conversions do not validate, so they also work for strings
-- that are not valid paths.
testOsString :: IORef Int -> String -> IO ()
testOsString failures s = do
    o <- osp s
    let arr = OsString.toArray o
        name x = x ++ " " ++ show s
    check failures (name "OsString.toArray empty")
        (null s == Array.null arr)
    check failures (name "OsString round trip")
        (OsString.fromArray arr == o)
    arrC <- arrWithCapacity arr
    check failures (name "OsString.fromArray with capacity")
        (OsString.fromArray arrC == o)
    check failures (name "OsString.fromArray with offset")
        (OsString.fromArray (arrWithOffset arr) == o)
    unless (null s || '\0' `elem` s) $ do
        let parr = Path.toArray (Path.fromString_ s)
        check failures (name "OsString.toArray matches Path")
            (Array.toList arr == Array.toList parr)

testInvalid :: IORef Int -> String -> IO ()
testInvalid failures s = do
    o <- osp s
    r <- try (toPath o) :: IO (Either SomeException Path)
    check failures ("toPath rejects " ++ show s)
        (either (const True) (const False) r)

main :: IO ()
main = do
    failures <- newIORef 0
    mapM_ (testSample failures) samples
    mapM_ (testInvalid failures) ["", "a\0b"]
    mapM_ (testOsString failures) (samples ++ ["", "a\0b"])
    n <- readIORef failures
    when (n > 0) $ do
        putStrLn $ show n ++ " test(s) failed."
        exitFailure
    putStrLn "All tests passed."
