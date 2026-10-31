# Streamly compatibility with filepath

Facilities for interconverting between the path types of the streamly
package and the `filepath` package:

* `Streamly.Compat.OsPath`: `Path` and `OsPath`.
* `Streamly.Compat.PosixPath`: `PosixPath` and `PosixPath`
  (`PosixString`), on any platform.
* `Streamly.Compat.WindowsPath`: `WindowsPath` and `WindowsPath`
  (`WindowsString`), on any platform.
* `Streamly.Compat.OsString`: a streamly `Array` of `OsWord` and
  `OsString`, without path validation.

# Streamly

Please visit [Streamly homepage](https://streamly.composewell.com) for more
details about streamly.

# Licensing

Available under [Apache-2.0 license](LICENSE).
