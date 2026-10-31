{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Compat.OsString
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Conversion between the 'OsString' type from @os-string@ (or @filepath@
-- before 1.5) and a streamly 'Array' of 'OsWord'.
--
-- 'OsWord' is 'Data.Word.Word8' on Posix and 'Data.Word.Word16' on Windows.
-- The array holds the 'OsString' in the platform encoding: bytes on Posix
-- and UTF-16LE units on Windows. The conversions do not decode, encode or
-- validate the contents, so they can be used for any 'OsString', e.g.
-- environment variables and command line arguments. To convert to a
-- validated 'Streamly.FileSystem.Path.Path' use "Streamly.Compat.OsPath".
--
-- The 'OsString' and the 'Array' resulting from a zero-copy conversion share
-- the same memory. Do not mutate the array (e.g. using
-- 'Streamly.Internal.Data.Array.unsafeThaw') after converting it to or from
-- an 'OsString'.

module Streamly.Compat.OsString
    ( toArray
    , fromArray
    )
where

#if MIN_VERSION_filepath(1,5,0)
import System.OsString.Internal.Types (OsString(..))
#if defined(mingw32_HOST_OS)
import System.OsString.Internal.Types (WindowsString(..))
#else
import System.OsString.Internal.Types (PosixString(..))
#endif
import "os-string" System.OsString.Data.ByteString.Short (ShortByteString(..))
#else
import "filepath" System.OsString.Internal.Types (OsString(..))
#if defined(mingw32_HOST_OS)
import "filepath" System.OsString.Internal.Types (WindowsString(..))
#else
import "filepath" System.OsString.Internal.Types (PosixString(..))
#endif
import "filepath" System.OsPath.Data.ByteString.Short (ShortByteString(..))
#endif
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.FileSystem.Path (OsWord)
import GHC.Exts (sizeofByteArray#, Int(..), unsafeCoerce#)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.MutByteArray as MBA

#if defined(mingw32_HOST_OS)
getString :: WindowsString -> ShortByteString
getString = getWindowsString
#else
getString :: PosixString -> ShortByteString
getString = getPosixString
#endif

#if defined(mingw32_HOST_OS)
mkString :: ShortByteString -> WindowsString
mkString = WindowsString
#else
mkString :: ShortByteString -> PosixString
mkString = PosixString
#endif

-- Note: MutableByteArray# and ByteArray# are the same heap object (ARR_WORDS)
-- and unsafeFreezeByteArray# is a no-op, so coercing one to the other is
-- equivalent to an unsafe freeze or thaw.

-- | Convert an 'OsString' to an 'Array' of 'OsWord' at zero cost. The array
-- shares the buffer of the 'OsString'. The contents are not validated.
--
-- On Windows, if the length of the 'OsString' in bytes is odd, the array has
-- an odd byte length. Such an array is invalid, readers of the array may read
-- past its end. 'OsString' values created by the @os-string@ and @filepath@
-- APIs always have an even length on Windows.
--
{-# INLINE toArray #-}
toArray :: OsString -> Array OsWord
toArray s =
    let !(SBS barr#) = getString $ getOsString s
        mbarr = MutByteArray (unsafeCoerce# barr#)
      in Array mbarr 0 (I# (sizeofByteArray# barr#))

-- | Convert an 'Array' of 'OsWord' to an 'OsString'. The contents are not
-- validated.
--
-- If the array spans its whole underlying buffer then the buffer is shared at
-- zero cost. Otherwise, e.g. when the array is a slice of a larger array or
-- has spare capacity at the end, it is copied to a new buffer, because a
-- 'ShortByteString' cannot represent a slice.
--
{-# INLINE fromArray #-}
fromArray :: Array OsWord -> OsString
fromArray (Array mba@(MutByteArray mbarr#) start end) =
    let sz = I# (sizeofByteArray# (unsafeCoerce# mbarr#))
     in if start == 0 && end == sz
        then OsString $ mkString (SBS (unsafeCoerce# mbarr#))
        else let !(MutByteArray arr#) =
                    unsafePerformIO
                        $ MBA.unsafeCloneSlice start (end - start) mba
             in OsString $ mkString (SBS (unsafeCoerce# arr#))
