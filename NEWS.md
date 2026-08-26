SelfControlledCohort 2.1.0
==========================

Changes:

- Performance: MDRR, pre-exposure gain, event-dependent observation, and EASE diagnostics now use batched SQL reads and ParallelLogger-based parallelism, and no longer materialize full result sets in memory.
- Study diagnostics are now configurable via Strategus through `createRunSelfControlledCohortArgs()` (`runDiagnostics`, `diagnostics`, `diagnosticThresholds`).
- Added a `CohortGenerator` dependency; `execute()` now validates that all requested exposure/outcome cohorts were generated (present in the cohort checksum table).

Bug Fixes:

- Fixed a bug where exposure-based negative controls (`controlType = "exposure"`) were treated as outcomes, contaminating `outcome_cohort_id` in results.
- Fixed `scc_outcome_exposure` to reflect the full input exposure-outcome list (rather than only pairs with results) and to correctly accumulate `scc_result` across all calibration groups/batches.
- `scc_result`, `scc_stat`, `scc_diagnostics_summary`, and `scc_outcome_exposure` are now always exported (as empty files when an analysis yields no effect estimates), so the export folder and manifest stay consistent.

SelfControlledCohort 2.0.0
==========================
This is a major release and breaks backwards compatibility with version 1.x.x series code.

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
- **EASE** expected absolute systematic error, observed residual bias
- **EVENT_DEPENDENT_OBSERVATION** - what percentage of events occur just before the observation period ends?


Changes:

- Revised diagnostic framework to align with SCCS package
- Added power analysis (MDRR) diagnostic
- Added pre-exposure gain detection
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
