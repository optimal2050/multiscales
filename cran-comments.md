## Resubmission

This is a resubmission. In this version I have:

* made Arrow IPC storage fall back to uncompressed sidecars and datasets when
  the requested compression codec is unavailable. This fixes the Debian
  incoming-check failures seen when Arrow was built without Zstandard support.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

The package is pure R with no compiled code and no system requirements.

## Test environments

* Windows 11, R 4.6.1 and R 4.5.3 (local, `--as-cran`)
* win-builder, R-devel
* R-hub: ubuntu-release, linux (R-devel), macos-arm64
