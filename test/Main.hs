{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Exception (SomeException, evaluate, fromException, try)
import Control.Monad (unless, when)
import Data.Bits (shiftR, (.&.))
#if !MIN_VERSION_streamly_core(0,3,1)
import Data.Char (isAscii)
#endif
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate, isInfixOf)
import Data.Word (Word16, Word8)
import GHC.Exts (isTrue#, sameMutableByteArray#, unsafeCoerce#)
import Streamly.Data.MutByteArray (Unbox)
import Streamly.FileSystem.Path (OsWord)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import Streamly.Internal.Data.Path (PathException)
import System.Exit (exitFailure)
import System.Mem (performMajorGC)
#if MIN_VERSION_filepath(1,5,0)
import System.OsString.Internal.Types
    (OsString(..), PosixString(..), WindowsString(..))
#else
import "filepath" System.OsString.Internal.Types
    (OsString(..), PosixString(..), WindowsString(..))
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

-- | True if the array uses the buffer of the ShortByteString.
sameBuffer :: ShortByteString -> Array a -> Bool
sameBuffer (SBS ba#) (Array (MutByteArray mba#) _ _) =
    isTrue# (sameMutableByteArray# (unsafeCoerce# ba#) mba#)

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

-- | An empty slice at a non-zero offset of a non-empty buffer.
emptySlice :: (Unbox a, Num a) => Array a
emptySlice = snd (Array.unsafeBreakAt 3 (Array.fromList [120, 121, 122]))

-- | Encode Word16 values as UTF-16LE bytes.
packLE :: [Word16] -> ShortByteString
packLE = SBS.pack . concatMap le

    where

    le w = [fromIntegral (w .&. 0xff), fromIntegral (w `shiftR` 8)]

-- | A deterministic pseudo random sequence (a linear congruential
-- generator).
randoms :: Int -> [Int]
randoms seed =
    map (`shiftR` 16) $ tail $ iterate next seed

    where

    next x = (x * 1103515245 + 12345) `mod` 2147483648

-- | @n@ pseudo random strings of length 1 to 40 using the given characters.
randomStrings :: Int -> [Char] -> Int -> [String]
randomStrings seed chars n = take n (go (randoms seed))

    where

    nchars = length chars

    go (l:xs) =
        let len = 1 + l `mod` 40
            (cs, rest) = splitAt len xs
         in map (\i -> chars !! (i `mod` nchars)) cs : go rest
    go [] = []

-------------------------------------------------------------------------------
-- Conversion modules
-------------------------------------------------------------------------------

-- | The functions of a conversion module and of the corresponding filepath
-- and streamly types. @s@ is the filepath type, @p@ is the streamly type
-- and @a@ is the array element type.
data Conv s p a = Conv
    { convName :: String
    , encodeUtf :: String -> IO s
    , fromWords :: [a] -> s
    , toSBS :: s -> ShortByteString
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
#if defined(mingw32_HOST_OS)
    , fromWords = OsString . WindowsString . packLE
    , toSBS = getWindowsString . getOsString
#else
    , fromWords = OsString . PosixString . SBS.pack
    , toSBS = getPosixString . getOsString
#endif
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
    , fromWords = PosixString . SBS.pack
    , toSBS = getPosixString
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
    , fromWords = WindowsString . packLE
    , toSBS = getWindowsString
    , unsafeToPath = WindowsPath.unsafeToPath
    , toPath = WindowsPath.toPath
    , fromPath = WindowsPath.fromPath
    , fromString_ = SWindowsPath.fromString_
    , toString = SWindowsPath.toString
    , toArray = SWindowsPath.toArray
    , unsafeFromArray = SWindowsPath.unsafeFromArray
    }

-- | The OsString conversions in the same form, with an unvalidated array as
-- the streamly type. toPath never fails.
osStringConv :: Conv OsString (Array OsWord) OsWord
osStringConv = osPathConv
    { convName = "OsString"
    , unsafeToPath = OsString.toArray
    , toPath = pure . OsString.toArray
    , fromPath = OsString.fromArray
    , fromString_ = Path.toArray . Path.fromString_
    , toString = Path.toString . Path.unsafeFromArray
    , toArray = id
    , unsafeFromArray = id
    }

-------------------------------------------------------------------------------
-- Tests common to all conversion modules
-------------------------------------------------------------------------------

-- | Conversions of a string that is a valid path for both libraries.
testValid :: (Eq s, Eq a, Unbox a, Num a) =>
    IORef Int -> Conv s p a -> String -> IO ()
testValid failures c s = do
    o <- encodeUtf c s
    let p = fromString_ c s
        words_ = Array.toList (toArray c p)
        name x = convName c ++ " " ++ x ++ " " ++ show s

    -- filepath -> streamly
    let pu = unsafeToPath c o
    check failures (name "unsafeToPath toString") (toString c pu == s)
    check failures (name "unsafeToPath words")
        (Array.toList (toArray c pu) == words_)
    r <- try (toPath c o >>= evaluate . toString c)
    check failures (name "toPath toString")
        (either (const False :: SomeException -> Bool) (== s) r)

    -- streamly -> filepath, for each array layout
    check failures (name "fromPath") (fromPath c p == o)
    arrC <- arrWithCapacity (toArray c p)
    check failures (name "fromPath with capacity")
        (fromPath c (unsafeFromArray c arrC) == o)
    check failures (name "fromPath with offset")
        (fromPath c (unsafeFromArray c (arrWithOffset (toArray c p))) == o)
    check failures (name "fromPath pinned")
        (fromPath c (unsafeFromArray c (Array.fromList' words_)) == o)

    -- Round trips
    check failures (name "round trip filepath")
        (fromPath c (unsafeToPath c o) == o)
    check failures (name "round trip streamly")
        (toString c (unsafeToPath c (fromPath c p)) == s)

-- | toPath, unsafeToPath and fromPath of a whole buffer do not copy, fromPath
-- of a slice copies.
testZeroCopy :: (Unbox a, Num a) => IORef Int -> Conv s p a -> String -> IO ()
testZeroCopy failures c s = do
    o <- encodeUtf c s
    let name x = convName c ++ " " ++ x ++ " " ++ show s
        pu = unsafeToPath c o
    p <- toPath c o
    check failures (name "unsafeToPath shares")
        (sameBuffer (toSBS c o) (toArray c pu))
    check failures (name "toPath shares")
        (sameBuffer (toSBS c o) (toArray c p))
    check failures (name "fromPath of whole buffer shares")
        (sameBuffer (toSBS c (fromPath c pu)) (toArray c pu))
    let ps = unsafeFromArray c (arrWithOffset (toArray c pu))
    check failures (name "fromPath of slice copies")
        (not (sameBuffer (toSBS c (fromPath c ps)) (toArray c ps)))

-- | The result of a copying fromPath does not change when the source buffer
-- is modified.
testCopyIndependence :: (Eq s, Unbox a, Num a) =>
    IORef Int -> Conv s p a -> IO ()
testCopyIndependence failures c = do
    let ws = [97, 98, 99]
    marr <- MutArray.emptyOf 16
    marr1 <- MutArray.splice marr (Array.unsafeThaw (Array.fromList ws))
    o <- evaluate $ fromPath c (unsafeFromArray c (Array.unsafeFreeze marr1))
    MutArray.putIndex 0 marr1 120
    check failures (convName c ++ " copy is independent of the source")
        (o == fromWords c ws)

-- | Empty arrays convert to empty strings.
testEmpty :: (Eq s, Unbox a, Num a) => IORef Int -> Conv s p a -> IO ()
testEmpty failures c = do
    check failures (convName c ++ " fromPath empty array")
        (fromPath c (unsafeFromArray c (Array.fromList [])) == fromWords c [])
    check failures (convName c ++ " fromPath empty slice")
        (fromPath c (unsafeFromArray c emptySlice) == fromWords c [])
    check failures (convName c ++ " unsafeToPath empty")
        (Array.null (toArray c (unsafeToPath c (fromWords c []))))

-- | Words that are not valid in the encoding (e.g. invalid UTF-8 bytes or
-- lone surrogates) are preserved by the conversions, they are not decoded.
testRawWords :: (Eq s, Eq a, Unbox a) =>
    IORef Int -> Conv s p a -> [a] -> IO ()
testRawWords failures c ws = do
    let o = fromWords c ws
        name x = convName c ++ " " ++ x ++ " " ++ show (length ws) ++ " words"
    check failures (name "unsafeToPath raw")
        (Array.toList (toArray c (unsafeToPath c o)) == ws)
    r <- try (toPath c o >>= evaluate . Array.toList . toArray c)
    check failures (name "toPath raw")
        (either (const False :: SomeException -> Bool) (== ws) r)
    check failures (name "fromPath raw")
        (fromPath c (unsafeFromArray c (Array.fromList ws)) == o)

-- | toPath throws a PathException for an invalid path.
testInvalid :: IORef Int -> Conv s p a -> String -> IO ()
testInvalid failures c s = do
    o <- encodeUtf c s
    r <- try (toPath c o >>= evaluate . toString c)
    check failures (convName c ++ " toPath rejects " ++ show s)
        (either isPathException (const False) r)

    where

    isPathException :: SomeException -> Bool
    isPathException e =
        case fromException e :: Maybe PathException of
            Just _ -> True
            Nothing -> False

-- | A shared buffer stays valid after a garbage collection.
testGC :: IORef Int -> Conv s p a -> String -> IO ()
testGC failures c s = do
    o <- encodeUtf c s
    p <- evaluate (unsafeToPath c o)
    o1 <- evaluate (fromPath c (fromString_ c s))
    performMajorGC
    check failures (convName c ++ " unsafeToPath after GC")
        (toString c p == s)
    check failures (convName c ++ " fromPath after GC")
        (toString c (unsafeToPath c o1) == s)

-- | Whether streamly's fromString_ encodes the string correctly.
-- Path.fromString in streamly-core 0.3.0 truncates non-ASCII paths.
fromStringWorks :: String -> Bool
#if MIN_VERSION_streamly_core(0,3,1)
fromStringWorks _ = True
#else
fromStringWorks = all isAscii
#endif

testConv :: (Eq s, Eq a, Unbox a, Num a) =>
    IORef Int -> Conv s p a -> [String] -> [[a]] -> [String] -> IO ()
testConv failures c samples raw invalid = do
    let valid = filter fromStringWorks samples
    mapM_ (testValid failures c) valid
    mapM_ (testZeroCopy failures c) valid
    mapM_ (testRawWords failures c) raw
    mapM_ (testInvalid failures c) invalid
    testCopyIndependence failures c
    testEmpty failures c
    testGC failures c (last valid)

-------------------------------------------------------------------------------
-- Posix paths
-------------------------------------------------------------------------------

-- | Long enough to be allocated as a large object by GHC.
posixLong :: String
posixLong = '/' : intercalate "/" (replicate 1000 "component")

-- | Valid Posix paths. Posix allows any character except NUL.
posixSamples :: [String]
posixSamples =
    [ "/"
    , "."
    , ".."
    , "a"
    , "/usr/local/bin"
    , "relative/path/file.txt"
    , "dir/"
    , "a//b"
    , "./a/../b/."
    , " a b "
    , ".hidden"
    , "-n"
    , "a\\b"
    , "a:b*c?d<e>f|g\"h"
    , "\x01\x1f\x7f"
    , "\x00e9t\x00e9/\x4e2d\x6587/\x1f600"
    , "e\x0301"
    ]
    ++ randomStrings 1 "ab/. \x00e9\x4e2d\x1f600" 100
    ++ [posixLong]

-- | Bytes that are not valid UTF-8.
posixRaw :: [[Word8]]
posixRaw =
    [ [0xff]
    , [0xc3]
    , [0x80, 0x2f, 0xfe]
    , [0xed, 0xa0, 0x80]
    , [0x61, 0xc3, 0x28, 0x62]
    ]

posixInvalid :: [String]
posixInvalid = ["", "\0", "a\0", "\0a", "a\0b"]

-------------------------------------------------------------------------------
-- Windows paths
-------------------------------------------------------------------------------

windowsLong :: String
windowsLong = "C:\\" ++ intercalate "\\" (replicate 1000 "component")

-- | Valid Windows paths.
windowsSamples :: [String]
windowsSamples =
    [ "C:\\"
    , "C:"
    , "C:x"
    , "c:\\Users\\x\\file.txt"
    , "C:/x\\y"
    , "\\\\server\\share\\x"
    , "\\x"
    , "."
    , "..\\a"
    , "a"
    , "dir\\"
    , "a/b"
    , "a\\\\b"
    , " a b "
    , "dir\\file.tar.gz"
    , "console\\com10\\nul1"
    , "\x00e9t\x00e9\\\x4e2d\x6587\\\x1f600"
    , "e\x0301"
    ]
    -- The leading letter avoids roots, e.g. a UNC root from two separators.
    ++ map ('x' :) (randomStrings 2 "ab\\/. \x00e9\x4e2d\x1f600" 100)
    ++ [windowsLong]

-- | UTF-16 units that are not valid UTF-16: lone surrogates.
windowsRaw :: [[Word16]]
windowsRaw =
    [ [0xd800]
    , [0xdc00]
    , [0x61, 0xd800, 0x62]
    , [0xdc00, 0xd800]
    , [0xffff, 0xfffe]
    ]

windowsInvalid :: [String]
windowsInvalid =
    [ ""
    , "\0"
    , "a\0b"
    , "a\x01"
    , "a\x1f"
    , "a<b"
    , "a>b"
    , "x\\a:b"
    , "a\"b"
    , "a|b"
    , "a?b"
    , "a*b"
    , "x\\CON\\y"
    , "x\\con.txt"
    , "nul"
    , "x\\Com1"
    , "LPT9\\x"
    , "C:\\\\x"
    , "\\\\\\x"
    , "\\\\prn\\x"
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
-- OsString
-------------------------------------------------------------------------------

-- | OsString conversions do not validate, so strings that are not valid
-- paths also convert.
testOsStringInvalidPaths :: IORef Int -> String -> IO ()
testOsStringInvalidPaths failures s = do
    o <- FOsPath.encodeUtf s
    let arr = OsString.toArray o
    check failures ("OsString round trip " ++ show s)
        (OsString.fromArray arr == o)
    check failures ("OsString.toArray length " ++ show s)
        (null s == Array.null arr)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    failures <- newIORef 0

#if defined(mingw32_HOST_OS)
    let platformSamples = windowsSamples
        platformRaw = windowsRaw
        platformInvalid = windowsInvalid
#else
    let platformSamples = posixSamples
        platformRaw = posixRaw
        platformInvalid = posixInvalid
#endif

    testConv failures osPathConv platformSamples platformRaw platformInvalid
    testConv failures posixPathConv posixSamples posixRaw posixInvalid
    testConv failures windowsPathConv windowsSamples windowsRaw windowsInvalid
    testWindowsOddLength failures

    testConv failures osStringConv platformSamples platformRaw []
    mapM_ (testOsStringInvalidPaths failures) platformInvalid

    n <- readIORef failures
    when (n > 0) $ do
        putStrLn $ show n ++ " test(s) failed."
        exitFailure
    putStrLn "All tests passed."
