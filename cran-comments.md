## Test environments
* Local: macOS, R 4.3.x
* GitHub Actions (ubuntu-latest, windows-latest, macOS-latest): R-release
* win-builder: R-devel and R-release

## R CMD check results
0 errors ✓ | 0 warnings ✓ | 3 notes ✓

The 3 NOTEs are:

1. **"checking for future file timestamps ... NOTE: unable to verify current time"** - System-level issue during check environment, not package-related. This can be safely ignored.

2. **"checking R code for possible problems ... NOTE: Picked up _JAVA_OPTIONS: -Xmx8g"** - Informational message from Java environment used by DatabaseConnector dependency. Not a package issue.

3. **"checking for non-standard things in the check directory ... NOTE: Found the following files/directories: 'scc_result'"** - Temporary results directory created during example execution with `\donttest{}`. Properly cleaned up after examples complete.

## Submission notes
This is a major version update (v2.0.0) with breaking changes from v1.x series (previously only available via github).

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
