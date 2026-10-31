{-# LANGUAGE FlexibleContexts #-}

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
--
-- To convert an @OsString@ to an array without path validation use
-- "Streamly.Compat.OsString".

module Streamly.Compat.OsPath
    ( unsafeToPath
    , toPath
    , fromPath
    )
where

import Control.Monad.Catch (MonadThrow, throwM)
import Foreign.Storable (sizeOf)
import Streamly.Compat.OsString (fromArray, toArray)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Path (PathException(..))
import Streamly.FileSystem.Path (Path, OsWord)
import System.OsPath (OsPath)

import qualified Streamly.Internal.FileSystem.Path as Path

-- | Adapt an 'OsPath' type to the streamly 'Path' type at zero cost. The
-- 'Path' shares the buffer of the 'OsPath'.
--
-- /Unsafe/: The user is responsible to make sure that the 'OsPath' is valid as
-- per 'Streamly.FileSystem.Path.validatePath'. For example, an empty 'OsPath'
-- results in an invalid 'Path'. On Windows the length of the 'OsPath' in bytes
-- must be even.
--
{-# INLINE unsafeToPath #-}
unsafeToPath :: OsPath -> Path
unsafeToPath = Path.unsafeFromArray . toArray

-- | Adapt an 'OsPath' type to the streamly 'Path' type without copying. The
-- 'Path' shares the buffer of the 'OsPath'. The path is validated using
-- 'Streamly.FileSystem.Path.validatePath', which takes time proportional to
-- the length of the path.
--
-- Throws an 'InvalidPath' exception if the path is not valid, for example if
-- it is empty or contains a NUL character. On Windows, also throws an
-- 'InvalidPath' exception if the length of the 'OsPath' in bytes is odd.
--
{-# INLINE toPath #-}
toPath :: MonadThrow m => OsPath -> m Path
toPath p =
    let arr@(Array _ start end) = toArray p
        len = end - start
        wordSize = sizeOf (undefined :: OsWord)
     in if len `rem` wordSize /= 0
        then throwM $ InvalidPath
            $ "Encoded path length " ++ show len
                ++ " is not a multiple of " ++ show wordSize ++ " bytes."
        else Path.fromArray arr

-- | Adapt a Streamly 'Path' type to 'OsPath' type. If the array of the
-- 'Path' spans its whole underlying buffer then the buffer is shared at zero
-- cost. Otherwise, e.g. when the 'Path' is a slice of a larger path, the path
-- is copied to a new buffer.
--
{-# INLINE fromPath #-}
fromPath :: Path -> OsPath
fromPath = fromArray . Path.toArray
