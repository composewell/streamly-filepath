{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Compat.OsPath
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Conversion between the 'Path' type from @streamly-core@ and the 'OsPath'
-- type from @filepath@.
--
-- Both types store the path as an array of 'OsWord' in the platform encoding:
-- 'Data.Word.Word8' bytes on Posix and little-endian 'Data.Word.Word16' units
-- (UTF-16LE) on Windows. A conversion therefore does not decode or encode the
-- path. When the underlying buffer is used whole it is shared, not copied.
--
-- The 'Path' and the 'OsPath' resulting from a zero-copy conversion share the
-- same memory. Do not mutate the array of a 'Path' (e.g. using
-- 'Streamly.Internal.Data.Array.unsafeThaw') after converting it to or from
-- an 'OsPath'.

module Streamly.Compat.OsPath
    ( unsafeToPath
    , toPath
    , fromPath
    )
where

import Control.Monad.Catch (MonadThrow)
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
import Streamly.FileSystem.Path (Path, OsWord)
import System.OsPath (OsPath)
import GHC.Exts (sizeofByteArray#, Int(..), unsafeCoerce#)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.MutByteArray as MBA
import qualified Streamly.Internal.FileSystem.Path as Path

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

{-# INLINE toOsWordArray #-}
toOsWordArray :: OsPath -> Array OsWord
toOsWordArray p =
    let !(SBS barr#) = getString $ getOsString p
        mbarr = MutByteArray (unsafeCoerce# barr#)
      in Array mbarr 0 (I# (sizeofByteArray# barr#)) :: Array OsWord

-- The array is shared only if it spans the whole buffer, otherwise the slice
-- is copied into a new buffer. The ShortByteString length is the size of the
-- buffer, so it cannot represent a slice or an array with spare capacity.
{-# INLINE fromOsWordArray #-}
fromOsWordArray :: Array OsWord -> OsPath
fromOsWordArray (Array mba@(MutByteArray mbarr#) start end) =
    let sz = I# (sizeofByteArray# (unsafeCoerce# mbarr#))
     in if start == 0 && end == sz
        then OsString $ mkString (SBS (unsafeCoerce# mbarr#))
        else let !(MutByteArray arr#) =
                    unsafePerformIO
                        $ MBA.unsafeCloneSlice start (end - start) mba
             in OsString $ mkString (SBS (unsafeCoerce# arr#))

-- | Adapt an 'OsPath' type to the streamly 'Path' type at zero cost. The
-- 'Path' shares the buffer of the 'OsPath'.
--
-- /Unsafe/: The user is responsible to make sure that the 'OsPath' is valid as
-- per 'Streamly.FileSystem.Path.validatePath'. For example, an empty 'OsPath'
-- results in an invalid 'Path'.
--
{-# INLINE unsafeToPath #-}
unsafeToPath :: OsPath -> Path
unsafeToPath = Path.unsafeFromArray . toOsWordArray

-- | Adapt an 'OsPath' type to the streamly 'Path' type without copying. The
-- 'Path' shares the buffer of the 'OsPath'. The path is validated using
-- 'Streamly.FileSystem.Path.validatePath', which takes time proportional to
-- the length of the path.
--
-- Throws an 'InvalidPath' exception if the path is not valid, for example if
-- it is empty or contains a NUL character.
--
{-# INLINE toPath #-}
toPath :: MonadThrow m => OsPath -> m Path
toPath = Path.fromArray . toOsWordArray

-- | Adapt a Streamly 'Path' type to 'OsPath' type. If the array of the
-- 'Path' spans its whole underlying buffer then the buffer is shared at zero
-- cost. Otherwise, e.g. when the 'Path' is a slice of a larger path, the path
-- is copied to a new buffer.
--
{-# INLINE fromPath #-}
fromPath :: Path -> OsPath
fromPath = fromOsWordArray . Path.toArray
