{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}

module Streamly.Compat.OsPath
    ( toByteArray
    , fromByteArray
    , toPath
    , fromPath
    )
where

import Data.Word (Word8)
#if MIN_VERSION_filepath(1,5,0)
import System.OsString.Internal.Types (OsString(..), PosixString(..))
import "os-string" System.OsString.Data.ByteString.Short (ShortByteString(..))
#else
import "filepath" System.OsString.Internal.Types (OsString(..), PosixString(..))
import "filepath" System.OsPath.Data.ByteString.Short (ShortByteString(..))
#endif
import Streamly.Internal.Data.MutByteArray (MutByteArray(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.FileSystem.Path (Path)
import System.OsPath (OsPath)
import GHC.Exts (sizeofByteArray#, Int(..), unsafeCoerce#)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.MutByteArray as MBA
import qualified Streamly.Internal.FileSystem.Path as Path

toByteArray :: OsPath -> Array Word8
toByteArray p =
    let !(SBS barr#) = getPosixString $ getOsString p
        mbarr = MutByteArray (unsafeCoerce# barr#)
      in Array mbarr 0 (I# (sizeofByteArray# barr#)) :: Array Word8

fromByteArray :: Array Word8 -> OsPath
fromByteArray (Array mba@(MutByteArray mbarr#) start end) =
    let sz = I# (sizeofByteArray# (unsafeCoerce# mbarr#))
     in if start == 0 && end == sz
        -- freeze it instead of coercing? What if mba has been coerced from a
        -- frozen byetarray e.g. in toByteArray above?
        then OsString $ PosixString (SBS (unsafeCoerce# mbarr#))
        else let !(MutByteArray arr#) =
                    unsafePerformIO
                        $ MBA.unsafeCloneSlice start (end - start) mba
             in OsString $ PosixString (SBS (unsafeCoerce# arr#))

toPath :: OsPath -> Path
toPath = Path.unsafeFromArray . toByteArray

fromPath :: Path -> OsPath
fromPath = fromByteArray . Path.toArray
