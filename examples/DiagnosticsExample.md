# Example: Using SCC Diagnostics

## Basic Usage

```r
library(SelfControlledCohort)

# Run SCC with diagnostics enabled (default)
runSelfControlledCohort(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "cdm",
  exposureIds = c(1118084, 1124300),  # ACE inhibitors
  outcomeIds = c(313217),              # Atrial fibrillation
  databaseId = "CCAE",
  resultExportPath = "results",
  runDiagnostics = TRUE,
  diagnostics = "all"  # Run all available diagnostics
)
```

## Running Specific Diagnostics

```r
# Only run critical diagnostics
runSelfControlledCohort(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "cdm",
  exposureIds = c(1118084),
  outcomeIds = c(313217),
  databaseId = "CCAE",
  runDiagnostics = TRUE,
  diagnostics = c("counts", "event_dependent", "pre_exposure")
)
```

## Customizing Thresholds

```r
# Use custom diagnostic thresholds
customThresholds <- getDefaultDiagnosticThresholds()
customThresholds$minExposedCount <- 10  # Require at least 10 exposed outcomes
customThresholds$minPersonCount <- 50   # Require at least 50 persons
customThresholds$eventDependentCensoringMaxProportion <- 0.15  # Allow up to 15% censoring

runSelfControlledCohort(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "cdm",
  exposureIds = c(1118084),
  outcomeIds = c(313217),
  databaseId = "CCAE",
  runDiagnostics = TRUE,
  diagnosticThresholds = customThresholds
)
```

## Running Diagnostics Separately

```r
# First run the analysis without diagnostics
runSelfControlledCohort(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "cdm",
  exposureIds = c(1118084),
  outcomeIds = c(313217),
  databaseId = "CCAE",
  resultsTable = "my_schema.scc_results",
  riskWindowsTable = "my_schema.scc_risk_windows",
  resultsDatabaseSchema = "my_schema",
  runDiagnostics = FALSE  # Skip diagnostics initially
)

# Later, run diagnostics on the saved results
connection <- DatabaseConnector::connect(connectionDetails)
resultExportManager <- getDefaultExportManager("diagnostics_output", "CCAE")

runSccDiagnostics(
  connection = connection,
  cdmDatabaseSchema = "cdm",
  resultsTable = "my_schema.scc_results",
  riskWindowsTable = "my_schema.scc_risk_windows",
  outcomeTable = "condition_era",
  outcomeDatabaseSchema = "cdm",
  exposureTable = "drug_era",
  exposureDatabaseSchema = "cdm",
  analysisId = 1,
  databaseId = "CCAE",
  diagnostics = "all",
  resultExportManager = resultExportManager
)

DatabaseConnector::disconnect(connection)
```

## Available Diagnostics

1. **counts**: Minimum sample size requirements
   - Checks: minimum persons, minimum exposed outcomes, minimum unexposed outcomes
   
2. **event_dependent**: Event-dependent observation
   - Checks: whether outcomes lead to censoring (e.g., death)
   
3. **pre_exposure**: Pre-exposure outcome rate stability
   - Checks: temporal trends in outcome rates before exposure
   
4. **window_balance**: Risk window balance
   - Checks: whether exposed and unexposed windows have similar durations
   
5. **cohort_stability**: Cohort stability
   - Checks: proportion of persons with complete observation windows

## Interpreting Results

Diagnostic results are saved to `scc_diagnostics_summary.csv`:

```r
# Read diagnostic results
diagnostics <- read.csv("results/scc_diagnostics_summary.csv")

# Check which tests failed
failed <- diagnostics[diagnostics$pass == 0, ]
print(failed)

# View specific diagnostic values
library(dplyr)
diagnostics %>%
  filter(diagnostic_name == "EVENT_DEPENDENT_OBSERVATION") %>%
  select(target_cohort_id, outcome_cohort_id, diagnostic_value, pass)
```

## Default Thresholds

```r
# View default thresholds
getDefaultDiagnosticThresholds()

# Returns:
# $eventDependentCensoringMaxProportion = 0.10  (max 10% censored)
# $preExposureRateRatioMaxDeviation = 2.0       (max 2x difference)
# $minExposedCount = 3                          (min 3 outcomes)
# $minUnexposedCount = 3                        (min 3 outcomes)
# $minPersonCount = 10                          (min 10 persons)
# $maxWindowImbalanceRatio = 10                 (max 10:1 ratio)
# $minObservationProportion = 0.70              (min 70% complete)
```
