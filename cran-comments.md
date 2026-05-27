## Test environments
* Local: macOS, R 4.3.x
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest): R-release
* win-builder: R-devel and R-release

## R CMD check results
0 errors ✓ | 0 warnings ✓ | 3 notes ✓

The 3 NOTEs are:

1. **"unable to verify current time"** - System-level issue during check, not package-related. This can be safely ignored.

2. **"File LICENSE is not mentioned in the DESCRIPTION file"** - The DESCRIPTION correctly specifies `License: Apache License 2.0`. The full LICENSE file is provided as standard practice for Apache 2.0 licensed packages.

3. **"Non-standard files/directories found at top level"** - The noted files/directories are:
   - `cran-comments.md` - Standard CRAN submission documentation (in .Rbuildignore)
   - `examples/` - Development examples directory (in .Rbuildignore)
   - `sql/` - Contains SQL query templates used by the package (standard for OHDSI packages)

## Submission notes
This is a major version update (v2.0.0) with breaking changes from v1.x series.

### Major changes in v2.0.0:
* Introduced results data model aligned with OHDSI HADES standards
* Revised diagnostic framework to align with SelfControlledCaseSeries package
* Added new diagnostics: MDRR (Minimum Detectable Relative Risk), pre-exposure proportion, EASE
* Removed support for CDM versions < 5.0
* Breaking changes to diagnostic names and thresholds (documented in NEWS.md)

### Reverse dependencies
None identified.

## Additional notes
All breaking changes are comprehensively documented in NEWS.md.
Package is part of the OHDSI HADES (Health Analytics Data-to-Evidence Suite) ecosystem.
