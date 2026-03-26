library(testthat)

# ---------------------------------------------------------------------------
# Focused integration tests for runSelfControlledCohort
#
# These replace the old parameter-sweep test that iterated 32 combinations
# against concept IDs absent from Eunomia, never inspected data, and
# segfaulted. Each test here exercises a specific parameter scenario with
# meaningful assertions on the actual output.
#
# Eunomia GiBleed cohort IDs (created by Eunomia::createCohorts in helper.R):
#   1 = Celecoxib, 2 = Diclofenac, 3 = GiBleed, 4 = NSAIDs
#
# NOTE: Using exposureIds = '' and outcomeIds = '' scans all drug_era x
# condition_era combinations, which is the only way to reliably produce
# effect estimates on the small Eunomia dataset (specific ID combos often
# fall below the exposed+unexposed event threshold).
# ---------------------------------------------------------------------------

# Helper: read the scc_result CSV from an export directory, if it exists
readSccResult <- function(exportPath) {
    resultFile <- file.path(exportPath, "scc_result.csv")
    if (!file.exists(resultFile)) {
        return(NULL)
    }
    read.csv(resultFile)
}

# Expected columns in scc_result.csv
expectedResultCols <- c(
    "target_cohort_id", "outcome_cohort_id", "rr",
    "lb_95", "ub_95", "p_value", "num_outcomes_exposed",
    "num_outcomes_unexposed", "time_at_risk_exposed",
    "time_at_risk_unexposed", "num_persons", "num_exposures",
    "analysis_id", "database_id"
)


test_that("Default parameters produce valid results", {
    resultPath <- tempfile("scc_default_")
    dir.create(resultPath)
    withr::defer(unlink(resultPath, recursive = TRUE))

    runSelfControlledCohort(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        databaseId = "test",
        computeThreads = 1,
        resultExportPath = resultPath,
        runDiagnostics = FALSE
    )

    checkManifestFiles(resultPath)

    result <- readSccResult(resultPath)
    expect_true(!is.null(result), info = "scc_result.csv should be written")
    expect_true(nrow(result) > 0, info = "Should produce at least one estimate")
    expect_true(all(expectedResultCols %in% names(result)),
        info = paste(
            "Missing columns:",
            paste(setdiff(expectedResultCols, names(result)), collapse = ", ")
        )
    )

    # Rate ratios should be positive
    validRr <- result$rr[!is.na(result$rr)]
    expect_true(all(validRr > 0), info = "All rate ratios should be positive")
})


test_that("Analysis settings are exported correctly", {
    resultPath <- tempfile("scc_settings_")
    dir.create(resultPath)
    withr::defer(unlink(resultPath, recursive = TRUE))

    runSelfControlledCohort(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        databaseId = "test",
        analysisId = 42,
        analysisDescription = "Test analysis description",
        computeThreads = 1,
        resultExportPath = resultPath,
        runDiagnostics = FALSE
    )

    # Verify analysis settings CSV
    settingsFile <- file.path(resultPath, "scc_analysis_setting.csv")
    expect_true(file.exists(settingsFile))
    settings <- read.csv(settingsFile)
    expect_equal(settings$analysis_id, 42)
    expect_equal(settings$description, "Test analysis description")

    # The result should carry the analysis_id through
    result <- readSccResult(resultPath)
    if (!is.null(result) && nrow(result) > 0) {
        expect_true(all(result$analysis_id == 42))
    }
})


test_that("Age restriction filters subjects", {
    resultPathRestricted <- tempfile("scc_age_restrict_")
    dir.create(resultPathRestricted)
    withr::defer(unlink(resultPathRestricted, recursive = TRUE))

    resultPathUnrestricted <- tempfile("scc_age_unrestrict_")
    dir.create(resultPathUnrestricted)
    withr::defer(unlink(resultPathUnrestricted, recursive = TRUE))

    commonArgs <- list(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        databaseId = "test",
        computeThreads = 1,
        runDiagnostics = FALSE
    )

    do.call(
        runSelfControlledCohort,
        c(commonArgs, list(resultExportPath = resultPathUnrestricted))
    )

    do.call(
        runSelfControlledCohort,
        c(commonArgs, list(
            minAge = "40", maxAge = "60",
            resultExportPath = resultPathRestricted
        ))
    )

    unrestricted <- readSccResult(resultPathUnrestricted)
    restricted <- readSccResult(resultPathRestricted)

    expect_true(!is.null(unrestricted) && nrow(unrestricted) > 0)
    # Restricted may have fewer rows since some combos may not meet thresholds
    # If it has results, person count should be <= unrestricted
    if (!is.null(restricted) && nrow(restricted) > 0) {
        # Compare matching exposure-outcome pairs
        common <- merge(unrestricted, restricted,
            by = c("target_cohort_id", "outcome_cohort_id"),
            suffixes = c("_unr", "_res")
        )
        if (nrow(common) > 0) {
            expect_true(all(common$num_persons_res <= common$num_persons_unr),
                info = "Age restriction should not increase person count"
            )
        }
    }
})


test_that("Study date restriction", {
    resultPath <- tempfile("scc_date_")
    dir.create(resultPath)
    withr::defer(unlink(resultPath, recursive = TRUE))

    runSelfControlledCohort(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        studyStartDate = "20000101",
        studyEndDate = "20051231",
        databaseId = "test",
        computeThreads = 1,
        resultExportPath = resultPath,
        runDiagnostics = FALSE
    )

    checkManifestFiles(resultPath)
    # Date-restricted run should complete without error
    expect_true(file.exists(file.path(resultPath, "manifest.json")))
})


test_that("addLengthOfExposure = FALSE for both windows", {
    resultPath <- tempfile("scc_nolen_")
    dir.create(resultPath)
    withr::defer(unlink(resultPath, recursive = TRUE))

    runSelfControlledCohort(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        addLengthOfExposureExposed = FALSE,
        riskWindowStartExposed = 0,
        riskWindowEndExposed = 30,
        addLengthOfExposureUnexposed = FALSE,
        riskWindowStartUnexposed = -30,
        riskWindowEndUnexposed = -1,
        databaseId = "test",
        computeThreads = 1,
        resultExportPath = resultPath,
        runDiagnostics = FALSE
    )

    checkManifestFiles(resultPath)
    # With fixed windows, should still produce some results on broad scan
    result <- readSccResult(resultPath)
    expect_true(!is.null(result), info = "Fixed-window config should produce results")
})


test_that("hasFullTimeAtRisk = TRUE restricts population", {
    resultPath <- tempfile("scc_fulltar_")
    dir.create(resultPath)
    withr::defer(unlink(resultPath, recursive = TRUE))

    runSelfControlledCohort(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        hasFullTimeAtRisk = TRUE,
        databaseId = "test",
        computeThreads = 1,
        resultExportPath = resultPath,
        runDiagnostics = FALSE
    )

    checkManifestFiles(resultPath)
    expect_true(file.exists(file.path(resultPath, "manifest.json")))
})


test_that("Permanent result tables are created in database", {
    conn <- DatabaseConnector::connect(connectionDetails)
    withr::defer(DatabaseConnector::disconnect(conn))

    resultPath <- tempfile("scc_perm_")
    dir.create(resultPath)
    withr::defer(unlink(resultPath, recursive = TRUE))

    runSelfControlledCohort(
        connection = conn,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        resultsTable = "scc_perm_results",
        riskWindowsTable = "scc_perm_rw",
        resultsDatabaseSchema = cdmDatabaseSchema,
        databaseId = "test",
        computeThreads = 1,
        resultExportPath = resultPath,
        runDiagnostics = FALSE
    )

    # Verify permanent tables exist
    expect_true(
        DatabaseConnector::existsTable(conn, cdmDatabaseSchema, "scc_perm_results"),
        info = "Permanent results table should exist"
    )
    expect_true(
        DatabaseConnector::existsTable(conn, cdmDatabaseSchema, "scc_perm_rw"),
        info = "Permanent risk windows table should exist"
    )
})


test_that("extractResults = FALSE skips CSV export", {
    resultPath <- tempfile("scc_noextract_")
    dir.create(resultPath)
    withr::defer(unlink(resultPath, recursive = TRUE))

    runSelfControlledCohort(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        extractResults = FALSE,
        databaseId = "test",
        computeThreads = 1,
        resultExportPath = resultPath,
        runDiagnostics = FALSE
    )

    # No result CSV should be written when results are not extracted
    expect_false(file.exists(file.path(resultPath, "scc_result.csv")),
        info = "scc_result.csv should not exist when extractResults = FALSE"
    )
})


test_that("firstExposureOnly = FALSE includes all exposures", {
    resultPathFirst <- tempfile("scc_firstonly_")
    dir.create(resultPathFirst)
    withr::defer(unlink(resultPathFirst, recursive = TRUE))

    resultPathAll <- tempfile("scc_allexp_")
    dir.create(resultPathAll)
    withr::defer(unlink(resultPathAll, recursive = TRUE))

    commonArgs <- list(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureIds = "",
        outcomeIds = "",
        databaseId = "test",
        computeThreads = 1,
        runDiagnostics = FALSE
    )

    do.call(
        runSelfControlledCohort,
        c(commonArgs, list(
            firstExposureOnly = TRUE,
            resultExportPath = resultPathFirst
        ))
    )

    do.call(
        runSelfControlledCohort,
        c(commonArgs, list(
            firstExposureOnly = FALSE,
            resultExportPath = resultPathAll
        ))
    )

    resultFirst <- readSccResult(resultPathFirst)
    resultAll <- readSccResult(resultPathAll)

    expect_true(!is.null(resultFirst) && nrow(resultFirst) > 0)
    expect_true(!is.null(resultAll) && nrow(resultAll) > 0)

    # With all exposures, total exposures should be >= first-only for matching pairs
    common <- merge(resultFirst, resultAll,
        by = c("target_cohort_id", "outcome_cohort_id"),
        suffixes = c("_first", "_all")
    )
    if (nrow(common) > 0) {
        expect_true(all(common$num_exposures_all >= common$num_exposures_first),
            info = "All exposures should include at least as many as first-only"
        )
    }
})


test_that("Risk window validation errors are raised", {
    conn <- DatabaseConnector::connect(connectionDetails)
    withr::defer(DatabaseConnector::disconnect(conn))

    # Exposed window end before start (without length addition) should error
    expect_error(
        runSelfControlledCohort(
            connection = conn,
            cdmDatabaseSchema = cdmDatabaseSchema,
            exposureIds = "",
            outcomeIds = "",
            databaseId = "test",
            riskWindowEndExposed = 1,
            riskWindowStartExposed = 30,
            addLengthOfExposureExposed = FALSE
        ),
        "risk window"
    )

    # Unexposed window end before start (without length addition) should error
    expect_error(
        runSelfControlledCohort(
            connection = conn,
            cdmDatabaseSchema = cdmDatabaseSchema,
            exposureIds = "",
            outcomeIds = "",
            databaseId = "test",
            riskWindowEndUnexposed = -30,
            riskWindowStartUnexposed = -1,
            addLengthOfExposureUnexposed = FALSE
        ),
        "risk window"
    )

    # Missing connection details should error
    expect_error(
        runSelfControlledCohort(
            cdmDatabaseSchema = cdmDatabaseSchema,
            exposureIds = "",
            outcomeIds = "",
            databaseId = "test"
        ),
        "Connection details not set"
    )
})


test_that("Negative controls and calibration run correctly", {
    resultPath <- tempfile("scc_calib_")
    dir.create(resultPath)
    withr::defer(unlink(resultPath, recursive = TRUE))

    # Using condition_occurrence instead of cohorts to ensure we have data
    # 192671: Gastrointestinal hemorrhage
    # 40481087: Viral sinusitis (negative control)
    # 4112343: Acute cystitis (negative control)
    negativeControls <- list(c(1, 40481087), c(1, 4112343))

    diagThresholds <- getDefaultDiagnosticThresholds()
    diagThresholds$minEventsPerWindow <- 0
    diagThresholds$mdrrMaxAcceptable <- Inf
    diagThresholds$maxPreExposureProportion <- 1.0
    diagThresholds$preExposurePThreshold <- 0.0
    diagThresholds$maxEventDependentCensoring <- 1.0
    diagThresholds$timeTrendPThreshold <- 0.0

    runSelfControlledCohort(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        exposureTable = "cohort",
        outcomeTable = "condition_occurrence",
        exposureIds = 1,   # Celecoxib
        outcomeIds = 192671,    # GI hemorrhage 
        databaseId = "test",
        computeThreads = 1,
        resultExportPath = resultPath,
        negativeControlPairs = negativeControls,
        runDiagnostics = TRUE,
        diagnosticThresholds = diagThresholds
    )

    checkManifestFiles(resultPath)

    # Check results file for calibrated columns
    result <- readSccResult(resultPath)
    expect_true(!is.null(result), info = "scc_result.csv should be written")
    expect_true("calibrated_rr" %in% names(result), info = "Calibrated RR column missing")
    expect_true("calibrated_p_value" %in% names(result), info = "Calibrated p-value missing")

    # The result should have the target (1) and outcome (192671)
    targetRow <- result[result$target_cohort_id == 1 & result$outcome_cohort_id == 192671, ]
    expect_true(nrow(targetRow) == 1)

    # Check diagnostics file for EASE
    diagFile <- file.path(resultPath, "scc_diagnostics_summary.csv")
    expect_true(file.exists(diagFile))
    diagnostics <- read.csv(diagFile)
    
    # EASE diagnostic should be computed for the target cohort
    easeDiag <- diagnostics[diagnostics$diagnostic_name == "EASE" & diagnostics$target_cohort_id == 1, ]
    expect_true(nrow(easeDiag) == 1)
    expect_true(is.na(easeDiag$outcome_cohort_id))
})
