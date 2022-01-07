SelfControlledCohort 1.6.0
==========================

Changes: 

- Removed support for older versions of DatabaseConnector (< 5.0.0)
- Deprecated usage of `oracleTempSchema` in favour of `tempEmulationSchema`
- Allow calls to run with a DatabaseConnector `connection` object 
- No longer supports undocumented use of `connectionDetails$conn` for storing connections
  when calling `runSelfControlledCohort`
- Added function computeSccRiskWindows to allow computation of exposure
  risk windows without having to run full SCC analysis
- Storage of results (before rate ratios are computed) now in a results
  table.
- Storage of time at risk results no longer merged with estimates
- Added tests on sqlite for faster unit testing while working on development
- Added testing on redshift
- Use of DatabaseConnector::renderTranslateQueryApplyBatched allowing optional callbacks for
  working with large data sets (too large for memory)
- Added computation of conventional p-values to results

SelfControlledCohort 1.5.1
==========================

Changes: 

- Added support for github actions and removed travis configuration
- Added `computeTarDistribution` flag to `runSelfControlledCohort`, which computes the distribution of exposure time
 windows for subjects experiencing the outcome and average absolute time between exposure and outcome.
- Added vignette for usage with some examples

Bug Fixes:
- Fixed issue with DatabaseConnector v 4.0.0 connectionDetails object on osx and linux caused by usage of
`connectionDetails$conn` for storage of a connection