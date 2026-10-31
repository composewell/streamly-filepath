# Audit of streamly-filepath 0.1.0

Scope: `src/Streamly/Compat/OsPath.hs`, `streamly-filepath.cabal`,
`cabal.project`, `stack.yaml`, `flake.nix`, CI configuration.

Checked against the sources of `streamly-core` 0.2.3, 0.3.0, 0.3.1 and
0.3.3, `filepath` 1.4.100.0, 1.4.200.1, 1.4.300.1, 1.4.301.0, 1.5.0.0 and
1.5.4.0, and `os-string` 2.0.0 and 2.0.7.

## Summary

| #  | Severity | Kind                        | Status     |
|----|----------|-----------------------------|------------|
| 1  | High     | Build / packaging           | Fixed      |
| 2  | Medium   | Memory safety (Windows)     | Fixed      |
| 3  | Low      | Documentation               | Fixed      |
| 4  | Low      | Documentation               | Fixed      |
| 5  | Medium   | Missing tests               | Fixed      |
| 6  | Low      | Missing API                 | Open       |
| 7  | Low      | Missing API                 | Fixed      |
| 8  | Info     | Aliasing                    | Documented |
| 9  | Info     | Build config                | Open       |
| 10 | Info     | Build config                | Open       |
| 11 | Medium   | Interoperability (upstream) | Documented |

## 1. Wrong lower bound on streamly-core

The library imports `Streamly.FileSystem.Path (Path, OsWord)` and
`Streamly.Internal.FileSystem.Path (unsafeFromArray, fromArray, toArray)`.
The module `Streamly.FileSystem.Path` and the type `OsWord` were introduced
in `streamly-core-0.3.0`. In `streamly-core-0.2.x` the module
`Streamly.Internal.FileSystem.Path` exists, but it exports a different API
(`fromChunk`, `toChunk`, typed `File`/`Dir` paths) and no `OsWord`.

The cabal file declared `streamly-core >= 0.2 && < 0.4`, so the solver can
pick `streamly-core-0.2.x`, for example under a Stackage snapshot that ships
0.2, and the build then fails with a missing module error.

Fix: the bound is now `streamly-core >= 0.3 && < 0.4`.

The released `streamly-filepath-0.1.0` on Hackage carries the same
`>= 0.2` bound. It needs a Hackage metadata revision raising the lower bound
to 0.3.

## 2. `toPath` accepted odd-length paths on Windows

On Windows `OsWord` is `Word16`, and the `OsPath` buffer is UTF-16LE, so its
length in bytes must be even. A `WindowsString` can be built from any
`ShortByteString` using its constructor, so an odd length is possible.
`toPath` used the whole buffer as an `Array Word16` and passed it to
`Path.fromArray`. The array stream reader in `streamly-core`
(`MutArray.toStreamWith`) advances the read offset by 2 bytes per `Word16`
and stops only when the offset equals the end offset. For a buffer of 5
bytes the offset takes the values 0, 2, 4, 6, ... and never equals 5, so the
reader runs past the end of the buffer until it reads a NUL word or faults.

Fix: `toPath` now throws an `InvalidPath` exception if the length in bytes is
not a multiple of `sizeOf (undefined :: OsWord)`. On Posix the word size is 1
and the check never fails. `unsafeToPath` does not check; its documentation
now states the requirement.

The commented-out Windows branch of `fromArray` in
`Streamly.Internal.FileSystem.Path.Common` shows that `streamly-core`
intended the same check but does not perform it.

## 3. `toPath` was documented as zero cost

The Haddock of `toPath` said "at zero cost". `toPath` does not copy, but it
calls `Path.fromArray`, which runs `validatePath` over the whole path: O(n) on
Posix (a scan for NUL) and more on Windows (drive, UNC and reserved name
checks). The documentation now states that the path is validated and lists
the exceptions.

## 4. `fromPath` was documented as zero cost

`fromPath` shares the buffer only if the array starts at offset 0 and ends at
the end of its `MutByteArray`. A `ShortByteString` has no offset or length
field of its own, so any other array is copied. This is the case for paths
produced by `splitPath`, `takeFileName`, `splitRoot` and other operations
that return slices, and for arrays built in a `MutArray` with spare capacity
and then frozen. The documentation now says when a copy is made.

The question in the old comment ("freeze it instead of coercing?") is
answered in a code comment: `MutableByteArray#` and `ByteArray#` are the same
heap object (`ARR_WORDS`) and `unsafeFreezeByteArray#` is a no-op, so
`unsafeCoerce#` between them is an unsafe freeze or thaw.

## 5. No test suite

The package had no tests, so CI built the library only. A test suite
`test/Main.hs` now checks, for ASCII and non-ASCII paths:

* `Path.toString (unsafeToPath o) == s` and the same for `toPath`, where `o`
  is the `OsPath` made from the string `s` by `System.OsPath.encodeUtf`.
  This checks that both libraries use the same encoding.
* `fromPath p == o`, where `p = Path.fromString_ s`, for three layouts of
  the array of `p`: the whole buffer (zero-copy branch), an array with spare
  capacity after the end, and a slice at a non-zero offset (both copying
  branches).
* Round trips `OsPath -> Path -> OsPath` and `Path -> OsPath -> Path`.
* `toPath` throws an exception for the empty path and for a path containing
  NUL.

The test suite runs with `-M1G` as the RTS heap limit.

## 6. No conversion for `PosixPath` and `WindowsPath`

`filepath` provides the platform-independent types
`System.OsPath.Posix.PosixPath` (`PosixString`) and
`System.OsPath.Windows.WindowsPath` (`WindowsString`). `streamly-core`
provides `PosixPath` (array of `Word8`) and `WindowsPath` (array of `Word16`)
in `Streamly.Internal.FileSystem.PosixPath` and
`Streamly.Internal.FileSystem.WindowsPath`. The representations match, so
the same zero-copy conversion applies, but the package only converts the
platform `OsPath` and `Path`. A program that handles Windows paths on Linux,
or Posix paths on Windows, cannot use this package.

Not implemented here, because the `streamly-core` types live in internal
modules whose API may change between minor versions.

## 7. No conversion for `OsString`

`OsPath` is a type synonym for `OsString`, so the functions already accept an
`OsString`. There was no conversion between `OsString` and a plain streamly
`Array OsWord` without path validation, which is needed for environment
variables and command line arguments that are not paths. `unsafeToPath`
skips validation but produces a `Path`.

Fix: the new module `Streamly.Compat.OsString` exports

* `toArray :: OsString -> Array OsWord`, which shares the buffer and never
  copies, and
* `fromArray :: Array OsWord -> OsString`, which shares the buffer when the
  array spans the whole buffer and copies otherwise.

Neither function validates the contents; the empty string and strings
containing NUL convert without an exception. `Streamly.Compat.OsPath` now
uses these two functions. The test suite checks round trips, the three
array layouts for `fromArray`, and that `toArray` gives the same words as
`Path.fromString_` for valid paths.

## 8. Shared buffers

`unsafeToPath`, `toPath` and the zero-copy branch of `fromPath` make the
`Path` and the `OsPath` point to the same buffer. Both types are immutable
through their public APIs. Mutating the array of a `Path` through internal
APIs, e.g. `Array.unsafeThaw` followed by a write, also changes the
`OsPath`, and the `OsPath` buffer was allocated as an immutable `ByteArray#`.
The module documentation now states this.

## 9. stack.yaml

* `extra-deps` lists the streamly git repository twice at the same commit.
  The first entry has no `subdirs`, so it builds the full `streamly`
  package, which this package does not depend on. Only the `core` subdir is
  needed.
* `streamly-core-0.3.x` is on Hackage; a Hackage version in `extra-deps` is
  preferable to a git commit.
* The `extra-include-dirs: /opt/local/include` (MacPorts SDL) and
  `flags: mintty` entries are not needed by this package.

These were not changed, since the stack build was not tested in this audit.

## 10. Build heap limit and flake input

* The build does not limit the GHC heap. Adding the following to
  `cabal.project` caps each GHC process at 3 GB:

  ```
  package *
    ghc-options: +RTS -M3G -RTS
  ```

  The test suite now has an RTS heap limit (item 5).
* `flake.nix` fetches `basepkgs` with `git+ssh://`, which requires an SSH key
  with access to `composewell/streamly-packages`. If that repository is
  public, a `github:` URL works without a key.

## 11. Non-ASCII paths from `Path.fromString` in streamly-core 0.3.0

In `streamly-core-0.3.0`, `Path.fromString` allocates an array with one
element per `Char` and then writes the UTF-8 encoding into it, so any path
with a non-ASCII character is truncated. For the path `"été"` (3 characters,
5 bytes `C3 A9 74 C3 A9` in UTF-8) the array holds the first 3 bytes, which
decode to `"ét"`. When the cut falls inside a multi-byte sequence,
`Path.toString` fails with `decodeUtf8With: Not enough input`. The `OsPath`
made from the same string with `System.OsPath.encodeUtf` holds all 5 bytes,
so `fromPath` of the streamly path is not equal to it. The changelog of
`streamly-core-0.3.1` lists the fix: "Fixed `Path.fromString` truncation when
unicode chars are present".

The conversions in this package copy the array as is and are correct with
0.3.0. The library bound stays at `>= 0.3`; the test suite requires
`streamly-core >= 0.3.1` because it builds paths with `Path.fromString_`.
Users who build paths from non-ASCII strings should use 0.3.1 or later.

## Verification

GHC 9.6.7 on Linux, with the RTS heap limit `-M3G` for GHC:

| streamly-core | filepath  | os-string | Result |
|---------------|-----------|-----------|--------|
| 0.3.3         | 1.4.300.1 | 2.0.11    | Build and tests pass |
| 0.3.3         | 1.5.5.0   | 2.0.11    | Build and tests pass |
| 0.3.1         | 1.4.300.1 | 2.0.11    | Build and tests pass |
| 0.3.0         | 1.4.300.1 | 2.0.11    | Library builds, tests fail: item 11 |

Replacing `unsafeCloneSlice start` by `unsafeCloneSlice 0` in
`fromOsWordArray` makes the offset tests fail, so the tests exercise the
copying branch. The Windows code paths were not compiled.
