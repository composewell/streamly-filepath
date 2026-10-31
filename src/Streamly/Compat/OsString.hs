{-# LANGUAGE CPP #-}

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

import Streamly.Compat.Internal.Array
    ( OsString(..)
#if defined(mingw32_HOST_OS)
    , WindowsString(..)
#else
    , PosixString(..)
#endif
    , fromShortByteString
    , toShortByteString
    )
import Streamly.Internal.Data.Array (Array)
import Streamly.FileSystem.Path (OsWord)

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
#if defined(mingw32_HOST_OS)
toArray = fromShortByteString . getWindowsString . getOsString
#else
toArray = fromShortByteString . getPosixString . getOsString
#endif

-- | Convert an 'Array' of 'OsWord' to an 'OsString'. The contents are not
-- validated.
--
-- If the array spans its whole underlying buffer then the buffer is shared at
-- zero cost. Otherwise, e.g. when the array is a slice of a larger array or
-- has spare capacity at the end, it is copied to a new buffer, because a
-- @ShortByteString@ cannot represent a slice.
--
{-# INLINE fromArray #-}
fromArray :: Array OsWord -> OsString
#if defined(mingw32_HOST_OS)
fromArray = OsString . WindowsString . toShortByteString
#else
fromArray = OsString . PosixString . toShortByteString
#endif
