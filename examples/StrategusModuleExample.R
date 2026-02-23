# Example: Using SelfControlledCohort Module with Strategus
# This example demonstrates how to create module specifications using exposure-outcome pairs

library(SelfControlledCohort)
library(Strategus)

# Step 1: Create exposure-outcome pairs
# Define exposure-outcome relationships including negative controls
exposureOutcome1 <- createExposureOutcome(
  exposureId = 1,    # ACE inhibitors cohort
  outcomeId = 3      # Angioedema outcome
)

exposureOutcome2 <- createExposureOutcome(
  exposureId = 1,    # ACE inhibitors cohort
  outcomeId = 4,     # Cough outcome
  trueEffectSize = 1 # Negative control (set to 1)
)

exposureOutcome3 <- createExposureOutcome(
  exposureId = 2,    # Beta blockers cohort
  outcomeId = 3      # Angioedema outcome
)

exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2, exposureOutcome3)

# Step 2: Define analysis settings
runSelfControlledCohortArgs1 <- createRunSelfControlledCohortArgs(
  firstExposureOnly = FALSE,
  riskWindowStart = 1,
  riskWindowEnd = 30,
  unexposedWindowEnd = -1
)

sccAnalysis1 <- createSccAnalysis(
  analysisId = 1,
  description = "SCC analysis with 30-day risk window",
  runSelfControlledCohortArgs = runSelfControlledCohortArgs1,
  controlType = "outcome",
  runDiagnostics = TRUE,
  diagnostics = c("all"),
  diagnosticThresholds = getDefaultDiagnosticThresholds()
)

analysisSettings <- list(sccAnalysis1)

# Step 3: Create module specifications for Strategus
moduleSpecs <- createSelfControlledCohortModuleSpecifications(
  analysisSettings = analysisSettings,
  exposureOutcomeList = exposureOutcomeList,
  computeThreads = 4
)

# The moduleSpecs object can now be used with Strategus
print("Module specifications created successfully!")
print(sprintf("Module: %s v%s", moduleSpecs$module, moduleSpecs$version))
print(sprintf("Number of exposure-outcome pairs: %d", length(moduleSpecs$settings$exposureOutcomeList)))
print(sprintf("Number of analyses: %d", length(moduleSpecs$settings$analysisSettings)))

# Note: Negative controls are automatically identified by trueEffectSize = 1
# In this example, exposureOutcome2 (ACE inhibitors -> Cough) is a negative control

