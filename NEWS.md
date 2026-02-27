SelfControlledCohort 2.0.0
==========================
This is a major release and breaks backwards compatability with version 1.x.x series code.

Changes:

- Introduced a results data model definition csv file in line with other OHDSI HADES analytics packages
- Removal of use of remotes for Eunomia
- Creation of results data model sql for analysis in database
- Export of large result sets from database with export managers
- Calibration of effect estimates now computed with EmpiricalCalibration package
- Storing risk window statistics is no longer an optional parameter
- Removed support for CDM versions < 5.0 (and no longer includes check or parameter)
- 
This release implements revised diagnostics aligned with SelfControlledCaseSeries package standards.

**Breaking Changes:**

- Diagnostic names and thresholds have changed to match SCCS standards
- Old diagnostics (`counts`, `window_balance`, `cohort_stability`, `pre_exposure`) have been removed
- `getDefaultDiagnosticThresholds()` now returns different threshold names

**New study Diagnostic functions**

- **MDRR** - Minimum Detectable Relative Risk (power analysis)
- **PRE_EXPOSURE_PROPORTION** - Tests for outcomes before exposure start
- **PRE_EXPOSURE_P_VALUE** - Statistical test for pre-exposure gain
- **TIME_TREND_P_VALUE** - Tests for temporal trends in outcome risk
- **SPARSE_EXPOSED** / **SPARSE_UNEXPOSED** - Checks for sufficient events in both windows
- **EVENT_DEPENDENT_OBSERVATION** - Retained from previous version

**New Functions:**

- `computeMdrrForRateRatio()` - Calculate MDRR for power assessment
- `testPreExposureGain()` - Test for pre-exposure outcomes
- `testTimeTrend()` - Test for time trends using Poisson GLM
- `checkSparseData()` - Check for sparse data issues
- Strategus compatability functions 

Changes:

- Revised diagnostic framework to align with SCCS package
- Added power analysis (MDRR) diagnostic
- Added pre-exposure gain detection
- Added time trend analysis with Poisson GLM
- Removed design-related diagnostics (window balance, cohort stability)
- Updated diagnostic thresholds to match SCCS standards
- Improved diagnostic result formatting with values in failure messages


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