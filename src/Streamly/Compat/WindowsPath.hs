-- |
-- Module      : Streamly.Compat.WindowsPath
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Conversion between the 'WindowsPath' type from @streamly-core@ and the
-- @WindowsPath@ type ('WindowsString') from @filepath@. The conversions work
-- on all platforms, e.g. to handle Windows paths on Posix.
--
-- Both types store the path as an array of 'Word16' units in UTF-16LE
-- encoding. A conversion therefore does not decode or encode the path. When
-- the underlying buffer is used whole it is shared, not copied.
--
-- The 'WindowsPath' and the 'WindowsString' resulting from a zero-copy
-- conversion share the same memory. Do not mutate the array of a 'WindowsPath'
-- (e.g. using 'Streamly.Internal.Data.Array.unsafeThaw') after converting it.

module Streamly.Compat.WindowsPath
    ( unsafeToPath
    , toPath
    , fromPath
    )
where

import Control.Monad.Catch (MonadThrow)
import Data.Word (Word16)
import Streamly.Compat.Internal.Array
    ( WindowsString(..)
    , checkWordSize
    , fromShortByteString
    , toShortByteString
    )
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.FileSystem.WindowsPath (WindowsPath)

import qualified Streamly.Internal.FileSystem.WindowsPath as Path

{-# INLINE toArray #-}
toArray :: WindowsString -> Array Word16
toArray = fromShortByteString . getWindowsString

-- | Adapt a 'WindowsString' to the streamly 'WindowsPath' type at zero cost.
-- The 'WindowsPath' shares the buffer of the 'WindowsString'.
--
-- /Unsafe/: The user is responsible to make sure that the 'WindowsString' is
-- valid as per 'Path.validatePath'. For example, an empty 'WindowsString'
-- results in an invalid 'WindowsPath'. The length of the 'WindowsString' in
-- bytes must be even.
--
{-# INLINE unsafeToPath #-}
unsafeToPath :: WindowsString -> WindowsPath
unsafeToPath = Path.unsafeFromArray . toArray

-- | Adapt a 'WindowsString' to the streamly 'WindowsPath' type without
-- copying. The 'WindowsPath' shares the buffer of the 'WindowsString'. The
-- path is validated using 'Path.validatePath' with the Windows rules, which
-- takes time proportional to the length of the path.
--
-- Throws an @InvalidPath@ exception if the path is not valid, for example if
-- it is empty, contains a NUL character or a reserved name such as @CON@.
-- Also throws an @InvalidPath@ exception if the length of the
-- 'WindowsString' in bytes is odd.
--
{-# INLINE toPath #-}
toPath :: MonadThrow m => WindowsString -> m WindowsPath
toPath s = do
    let arr = toArray s
    checkWordSize arr
    Path.fromArray arr

-- | Adapt a streamly 'WindowsPath' to 'WindowsString'. If the array of the
-- 'WindowsPath' spans its whole underlying buffer then the buffer is shared at
-- zero cost. Otherwise, e.g. when the 'WindowsPath' is a slice of a larger
-- path, the path is copied to a new buffer.
--
{-# INLINE fromPath #-}
fromPath :: WindowsPath -> WindowsString
fromPath = WindowsString . toShortByteString . Path.toArray
