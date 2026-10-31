-- |
-- Module      : Streamly.Compat.PosixPath
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Conversion between the 'PosixPath' type from @streamly-core@ and the
-- @PosixPath@ type ('PosixString') from @filepath@. The conversions work on
-- all platforms, e.g. to handle Posix paths on Windows.
--
-- Both types store the path as an array of 'Word8' bytes. A conversion
-- therefore does not decode or encode the path. When the underlying buffer is
-- used whole it is shared, not copied.
--
-- The 'PosixPath' and the 'PosixString' resulting from a zero-copy conversion
-- share the same memory. Do not mutate the array of a 'PosixPath' (e.g. using
-- 'Streamly.Internal.Data.Array.unsafeThaw') after converting it.

module Streamly.Compat.PosixPath
    ( unsafeToPath
    , toPath
    , fromPath
    )
where

import Control.Monad.Catch (MonadThrow)
import Data.Word (Word8)
import Streamly.Compat.Internal.Array
    ( PosixString(..)
    , checkWordSize
    , fromShortByteString
    , toShortByteString
    )
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.FileSystem.PosixPath (PosixPath)

import qualified Streamly.Internal.FileSystem.PosixPath as Path

{-# INLINE toArray #-}
toArray :: PosixString -> Array Word8
toArray = fromShortByteString . getPosixString

-- | Adapt a 'PosixString' to the streamly 'PosixPath' type at zero cost. The
-- 'PosixPath' shares the buffer of the 'PosixString'.
--
-- /Unsafe/: The user is responsible to make sure that the 'PosixString' is
-- valid as per 'Path.validatePath'. For example, an empty 'PosixString'
-- results in an invalid 'PosixPath'.
--
{-# INLINE unsafeToPath #-}
unsafeToPath :: PosixString -> PosixPath
unsafeToPath = Path.unsafeFromArray . toArray

-- | Adapt a 'PosixString' to the streamly 'PosixPath' type without copying.
-- The 'PosixPath' shares the buffer of the 'PosixString'. The path is
-- validated using 'Path.validatePath' with the Posix rules, which takes time
-- proportional to the length of the path.
--
-- Throws an @InvalidPath@ exception if the path is not valid, for example if
-- it is empty or contains a NUL character.
--
{-# INLINE toPath #-}
toPath :: MonadThrow m => PosixString -> m PosixPath
toPath s = do
    let arr = toArray s
    checkWordSize arr
    Path.fromArray arr

-- | Adapt a streamly 'PosixPath' to 'PosixString'. If the array of the
-- 'PosixPath' spans its whole underlying buffer then the buffer is shared at
-- zero cost. Otherwise, e.g. when the 'PosixPath' is a slice of a larger path,
-- the path is copied to a new buffer.
--
{-# INLINE fromPath #-}
fromPath :: PosixPath -> PosixString
fromPath = PosixString . toShortByteString . Path.toArray
