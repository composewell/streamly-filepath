{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

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

{-# INLINE toOsWordArray #-}
toOsWordArray :: OsPath -> Array OsWord
toOsWordArray p =
    let !(SBS barr#) = getString $ getOsString p
        mbarr = MutByteArray (unsafeCoerce# barr#)
      in Array mbarr 0 (I# (sizeofByteArray# barr#)) :: Array OsWord

{-# INLINE fromOsWordArray #-}
fromOsWordArray :: Array OsWord -> OsPath
fromOsWordArray (Array mba@(MutByteArray mbarr#) start end) =
    let sz = I# (sizeofByteArray# (unsafeCoerce# mbarr#))
     in if start == 0 && end == sz
        -- freeze it instead of coercing? What if mba has been coerced from a
        -- frozen byetarray e.g. in toOsWordArray above?
        then OsString $ mkString (SBS (unsafeCoerce# mbarr#))
        else let !(MutByteArray arr#) =
                    unsafePerformIO
                        $ MBA.unsafeCloneSlice start (end - start) mba
             in OsString $ mkString (SBS (unsafeCoerce# arr#))

-- | Adapt an 'OsPath' type to the streamly 'Path' type at zero cost.
--
-- Unsafe: The user is responsible to make sure that the 'OsPath' is valid as
-- per 'Path.validatePath'.
--
unsafeToPath :: OsPath -> Path
unsafeToPath = Path.unsafeFromArray . toOsWordArray

-- | Adapt an 'OsPath' type to the streamly 'Path' type at zero cost.
--
toPath :: MonadThrow m => OsPath -> m Path
toPath = Path.fromArray . toOsWordArray

-- | Adapt a Streamly 'Path' type to 'OsPath' type at zero cost.
fromPath :: Path -> OsPath
fromPath = fromOsWordArray . Path.toArray
