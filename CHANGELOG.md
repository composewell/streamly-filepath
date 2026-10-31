# Changelog

## Unreleased

* Added `Streamly.Compat.OsString` with `toArray` and `fromArray`,
  conversions between `OsString` and an `Array` of `OsWord` without
  validation. `toArray` never copies; `fromArray` copies only when the array
  does not span its whole buffer.
* Added `Streamly.Compat.PosixPath` and `Streamly.Compat.WindowsPath` to
  convert between the `PosixPath` and `WindowsPath` types of `filepath` and
  `streamly-core` on any platform.

## 0.1.0 (Sep 2025)

* Initial version.
