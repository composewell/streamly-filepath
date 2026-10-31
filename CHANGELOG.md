# Changelog

## Unreleased

* Added `Streamly.Compat.OsString` with `toArray` and `fromArray`,
  conversions between `OsString` and an `Array` of `OsWord` without
  validation. `toArray` never copies; `fromArray` copies only when the array
  does not span its whole buffer.

## 0.1.0 (Sep 2025)

* Initial version.
