SelfControlledCohort 1.6.0
==========================

Changes: 

- Added support for github actions and removed travis configuration
- Added computeTarDistribution flag to runSelfControlledCohort, which computes the distribution of exposure time
 windows for subjects experiencing the outcome and average absolute time between exposure and outcome.
- Added vignette for usage with some examples

Bug Fixes:
- Fixed issue with DatabaseConnector v 4.0.0 connectionDetails object on osx and linux caused by usage of
`connectionDetails$conn` for storage of a connection