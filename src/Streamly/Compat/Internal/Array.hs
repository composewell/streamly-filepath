{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Compat.Internal.Array
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Conversion between 'ShortByteString' and a streamly 'Array', and the
-- string types of @os-string@ (or @filepath@ before 1.5).

module Streamly.Compat.Internal.Array
    ( OsString(..)
    , PosixString(..)
    , WindowsString(..)
    , ShortByteString(..)
    , fromShortByteString
    , toShortByteString
    , checkWordSize
    )
where

import Control.Monad.Catch (MonadThrow, throwM)
#if MIN_VERSION_filepath(1,5,0)
import System.OsString.Internal.Types
    (OsString(..), PosixString(..), WindowsString(..))
import "os-string" System.OsString.Data.ByteString.Short (ShortByteString(..))
#else
import "filepath" System.OsString.Internal.Types
    (OsString(..), PosixString(..), WindowsString(..))
import "filepath" System.OsPath.Data.ByteString.Short (ShortByteString(..))
#endif
import Foreign.Storable (Storable, sizeOf)
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Path (PathException(..))
import GHC.Exts (sizeofByteArray#, Int(..), unsafeCoerce#)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.MutByteArray as MBA

-- Note: MutableByteArray# and ByteArray# are the same heap object (ARR_WORDS)
-- and unsafeFreezeByteArray# is a no-op, so coercing one to the other is
-- equivalent to an unsafe freeze or thaw.

-- | Create an array sharing the buffer of the 'ShortByteString'.
{-# INLINE fromShortByteString #-}
fromShortByteString :: ShortByteString -> Array a
fromShortByteString (SBS barr#) =
    let mbarr = MutByteArray (unsafeCoerce# barr#)
      in Array mbarr 0 (I# (sizeofByteArray# barr#))

-- | Share the buffer of the array if it spans the whole buffer, otherwise
-- copy the slice into a new buffer. A 'ShortByteString' cannot represent a
-- slice or an array with spare capacity because its length is the size of
-- the buffer.
{-# INLINE toShortByteString #-}
toShortByteString :: Array a -> ShortByteString
toShortByteString (Array mba@(MutByteArray mbarr#) start end) =
    let sz = I# (sizeofByteArray# (unsafeCoerce# mbarr#))
     in if start == 0 && end == sz
        then SBS (unsafeCoerce# mbarr#)
        else let !(MutByteArray arr#) =
                    unsafePerformIO
                        $ MBA.unsafeCloneSlice start (end - start) mba
             in SBS (unsafeCoerce# arr#)

-- | Throw an 'InvalidPath' exception if the length of the array in bytes is
-- not a multiple of the size of the array element.
{-# INLINE checkWordSize #-}
checkWordSize :: forall m a. (MonadThrow m, Storable a) => Array a -> m ()
checkWordSize (Array _ start end) =
    let len = end - start
        wordSize = sizeOf (undefined :: a)
     in if len `rem` wordSize /= 0
        then throwM $ InvalidPath
            $ "Encoded path length " ++ show len
                ++ " is not a multiple of " ++ show wordSize ++ " bytes."
        else pure ()
