{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless, when)
import Data.List (isInfixOf)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Word (Word16, Word8)
import Streamly.Internal.Data.Array (Array)
import Streamly.Data.MutByteArray (Unbox)
import Streamly.FileSystem.Path (OsWord)
import System.Exit (exitFailure)
#if MIN_VERSION_filepath(1,5,0)
import System.OsString.Internal.Types (WindowsString(..))
#else
import "filepath" System.OsString.Internal.Types (WindowsString(..))
#endif

import qualified Data.ByteString.Short as SBS
import qualified Streamly.Compat.OsPath as OsPath
import qualified Streamly.Compat.OsString as OsString
import qualified Streamly.Compat.PosixPath as PosixPath
import qualified Streamly.Compat.WindowsPath as WindowsPath
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.PosixPath as SPosixPath
import qualified Streamly.Internal.FileSystem.WindowsPath as SWindowsPath
import qualified System.OsPath as FOsPath
import qualified System.OsPath.Posix as FPosixPath
import qualified System.OsPath.Windows as FWindowsPath

-------------------------------------------------------------------------------
-- Test helpers
-------------------------------------------------------------------------------

check :: IORef Int -> String -> Bool -> IO ()
check failures name ok =
    unless ok $ do
        putStrLn $ "FAILED: " ++ name
        modifyIORef' failures (+ 1)

-- | Copy an array to a new buffer that has spare capacity at the end.
arrWithCapacity :: Unbox a => Array a -> IO (Array a)
arrWithCapacity arr = do
    marr <- MutArray.emptyOf (Array.length arr + 64)
    marr1 <- MutArray.splice marr (Array.unsafeThaw arr)
    return $ Array.unsafeFreeze marr1

-- | Copy an array to a new buffer at a non-zero offset.
arrWithOffset :: (Unbox a, Num a) => Array a -> Array a
arrWithOffset arr =
    let prefix = [120, 121, 122]
        arr1 = Array.fromList (prefix ++ Array.toList arr)
     in snd (Array.unsafeBreakAt (length prefix) arr1)

-------------------------------------------------------------------------------
-- Path conversion tests
-------------------------------------------------------------------------------

-- | The functions of a conversion module and of the corresponding filepath
-- and streamly path types. @s@ is the filepath type, @p@ is the streamly
-- type and @a@ is the array element type.
data Conv s p a = Conv
    { convName :: String
    , encodeUtf :: String -> IO s
    , unsafeToPath :: s -> p
    , toPath :: s -> IO p
    , fromPath :: p -> s
    , fromString_ :: String -> p
    , toString :: p -> String
    , toArray :: p -> Array a
    , unsafeFromArray :: Array a -> p
    }

osPathConv :: Conv FOsPath.OsPath Path.Path OsWord
osPathConv = Conv
    { convName = "OsPath"
    , encodeUtf = FOsPath.encodeUtf
    , unsafeToPath = OsPath.unsafeToPath
    , toPath = OsPath.toPath
    , fromPath = OsPath.fromPath
    , fromString_ = Path.fromString_
    , toString = Path.toString
    , toArray = Path.toArray
    , unsafeFromArray = Path.unsafeFromArray
    }

posixPathConv :: Conv FPosixPath.PosixPath SPosixPath.PosixPath Word8
posixPathConv = Conv
    { convName = "PosixPath"
    , encodeUtf = FPosixPath.encodeUtf
    , unsafeToPath = PosixPath.unsafeToPath
    , toPath = PosixPath.toPath
    , fromPath = PosixPath.fromPath
    , fromString_ = SPosixPath.fromString_
    , toString = SPosixPath.toString
    , toArray = SPosixPath.toArray
    , unsafeFromArray = SPosixPath.unsafeFromArray
    }

windowsPathConv ::
    Conv FWindowsPath.WindowsPath SWindowsPath.WindowsPath Word16
windowsPathConv = Conv
    { convName = "WindowsPath"
    , encodeUtf = FWindowsPath.encodeUtf
    , unsafeToPath = WindowsPath.unsafeToPath
    , toPath = WindowsPath.toPath
    , fromPath = WindowsPath.fromPath
    , fromString_ = SWindowsPath.fromString_
    , toString = SWindowsPath.toString
    , toArray = SWindowsPath.toArray
    , unsafeFromArray = SWindowsPath.unsafeFromArray
    }

testValid :: (Eq s, Unbox a, Num a) =>
    IORef Int -> Conv s p a -> String -> IO ()
testValid failures c s = do
    o <- encodeUtf c s
    let p = fromString_ c s
        name x = convName c ++ " " ++ x ++ " " ++ show s

    -- filepath -> streamly
    check failures (name "unsafeToPath toString")
        (toString c (unsafeToPath c o) == s)
    r <- try (toPath c o >>= evaluate . toString c)
    check failures (name "toPath toString")
        (either (const False :: SomeException -> Bool) (== s) r)

    -- streamly -> filepath
    check failures (name "fromPath") (fromPath c p == o)
    arrC <- arrWithCapacity (toArray c p)
    check failures (name "fromPath with capacity")
        (fromPath c (unsafeFromArray c arrC) == o)
    check failures (name "fromPath with offset")
        (fromPath c (unsafeFromArray c (arrWithOffset (toArray c p))) == o)

    -- Round trips
    check failures (name "round trip filepath")
        (fromPath c (unsafeToPath c o) == o)
    check failures (name "round trip streamly")
        (toString c (unsafeToPath c (fromPath c p)) == s)

testInvalid :: IORef Int -> Conv s p a -> String -> IO ()
testInvalid failures c s = do
    o <- encodeUtf c s
    r <- try (toPath c o >>= evaluate . toString c)
    check failures (convName c ++ " toPath rejects " ++ show s)
        (either (const True :: SomeException -> Bool) (const False) r)

-- | Posix paths valid for streamly and filepath.
posixSamples :: [String]
posixSamples =
    [ "/"
    , "."
    , "a"
    , "/usr/local/bin"
    , "relative/path/file.txt"
    , "dir/"
    , "\x00e9t\x00e9/\x4e2d\x6587/\x1f600"
    ]

-- | Windows paths valid for streamly and filepath.
windowsSamples :: [String]
windowsSamples =
    [ "C:\\"
    , "a"
    , "C:\\Users\\x\\file.txt"
    , "\\\\server\\share\\x"
    , "dir\\"
    , "a/b"
    , "\x00e9t\x00e9\\\x4e2d\x6587\\\x1f600"
    ]

-- | A WindowsString of 3 bytes, an odd length.
testWindowsOddLength :: IORef Int -> IO ()
testWindowsOddLength failures = do
    let o = WindowsString (SBS.pack [97, 0, 98])
    r <- try (WindowsPath.toPath o >>= evaluate . SWindowsPath.toString)
    check failures "WindowsPath toPath rejects odd length"
        (either (isOddLengthError :: SomeException -> Bool) (const False) r)

    where

    isOddLengthError e = "is not a multiple of 2 bytes" `isInfixOf` show e

-------------------------------------------------------------------------------
-- OsString tests
-------------------------------------------------------------------------------

-- | OsString conversions do not validate, so they also work for strings
-- that are not valid paths.
testOsString :: IORef Int -> String -> IO ()
testOsString failures s = do
    o <- FOsPath.encodeUtf s
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

main :: IO ()
main = do
    failures <- newIORef 0

#if defined(mingw32_HOST_OS)
    let platformSamples = windowsSamples
#else
    let platformSamples = posixSamples
#endif
    mapM_ (testValid failures osPathConv) platformSamples
    mapM_ (testInvalid failures osPathConv) ["", "a\0b"]

    mapM_ (testValid failures posixPathConv) posixSamples
    mapM_ (testInvalid failures posixPathConv) ["", "a\0b"]

    mapM_ (testValid failures windowsPathConv) windowsSamples
    mapM_ (testInvalid failures windowsPathConv)
        ["", "a\0b", "x\\CON\\y", "a>b"]
    testWindowsOddLength failures

    mapM_ (testOsString failures) (platformSamples ++ ["", "a\0b"])

    n <- readIORef failures
    when (n > 0) $ do
        putStrLn $ show n ++ " test(s) failed."
        exitFailure
    putStrLn "All tests passed."
