library(testthat)
library(SelfControlledCohort)

test_that("getModuleInfo returns correct structure", {
    info <- getModuleInfo()
    expect_type(info, "list")
    expect_true(all(c("name", "version", "description", "author", "maintainer", "date") %in% names(info)))
    expect_equal(info$name, "SelfControlledCohort")
})

test_that("checkModuleVersion works", {
    currentVersion <- getModuleInfo()$version

    # Same version should pass
    expect_no_error(checkModuleVersion(currentVersion))

    # Newer version should error
    newerVersion <- "99.99.99"
    expect_error(checkModuleVersion(newerVersion), "Module specifications version.*is newer than installed package version")

    # Older version should warn if current is >= 2.0.0
    if (package_version(currentVersion) >= "2.0.0") {
        olderVersion <- "1.0.0"
        expect_warning(checkModuleVersion(olderVersion), "Module specifications version.*is older than installed package version")
    }
})

test_that("createSelfControlledCohortModuleSpecifications returns correct structure", {
    analysisSettings <- list(
        list(
            analysisId = 1,
            description = "Test",
            runSelfControlledCohortArgs = list()
        )
    )
    exposureCohortIds <- c(1, 2)
    outcomeCohortIds <- c(3, 4)

    specs <- createSelfControlledCohortModuleSpecifications(
        analysisSettings = analysisSettings,
        exposureCohortIds = exposureCohortIds,
        outcomeCohortIds = outcomeCohortIds
    )

    expect_s3_class(specs, "SelfControlledCohortModuleSpecifications")
    expect_s3_class(specs, "ModuleSpecifications")
    expect_equal(specs$module, "SelfControlledCohort")
    expect_equal(specs$settings$exposureCohortIds, exposureCohortIds)
    expect_equal(specs$settings$outcomeCohortIds, outcomeCohortIds)
})

test_that("execute function runs correctly", {
    # Setup temp directory for results
    tempResultsDir <- tempfile("scc_strategus_results")
    dir.create(tempResultsDir)
    withr::defer(unlink(tempResultsDir, recursive = TRUE))

    jobContext <- list(
        connectionDetails = list(), # Mock connection details
        executionSettings = list(
            databaseId = "test",
            cdmDatabaseSchema = "main",
            workDatabaseSchema = "main",
            cohortTable = "cohort",
            resultsFolder = tempResultsDir,
            tempEmulationSchema = NULL
        ),
        moduleExecutionSettings = list(
            version = getModuleInfo()$version,
            settings = list(
                analysisSettings = list(
                    list(
                        analysisId = 1,
                        description = "Test Analysis",
                        runSelfControlledCohortArgs = list()
                    )
                ),
                exposureCohortIds = c(1),
                outcomeCohortIds = c(3),
                negativeControls = data.frame(exposureId = 1, outcomeId = 3),
                controlType = "outcome",
                computeThreads = 1,
                runDiagnostics = FALSE,
                diagnostics = "all",
                diagnosticThresholds = list()
            )
        )
    )

    # Mock runSelfControlledCohort to avoid dependency on database/Eunomia for this test
    testthat::with_mocked_bindings(
        {
            # Also need to mock getCohortTableNames since we use a mock connection
            testthat::with_mocked_bindings(
                {
                    expect_no_error(execute(jobContext))
                },
                getCohortTableNames = function(baseName) {
                    list(cohortTable = baseName)
                },
                .package = "CohortGenerator"
            )
        },
        runSelfControlledCohort = function(...) {
            args <- list(...)
            # Create the expected output directory and files
            dir.create(args$resultExportPath, recursive = TRUE, showWarnings = FALSE)
            write.csv(data.frame(target_cohort_id = 1), file.path(args$resultExportPath, "scc_result.csv"))
            write("{}", file.path(args$resultExportPath, "manifest.json"))
            return(invisible(NULL))
        },
        .package = "SelfControlledCohort"
    )

    # Check if results directory was created
    sccResultsPath <- file.path(tempResultsDir, "selfControlledCohort", "A_1")
    expect_true(dir.exists(sccResultsPath))
    expect_true(file.exists(file.path(sccResultsPath, "scc_result.csv")))

    # Test skipping analysis if manifest exists
    testthat::with_mocked_bindings(
        {
            expect_message(execute(jobContext), "Results manifest found.*skipping analysis")
        },
        runSelfControlledCohort = function(...) {
            stop("Should not be called because manifest exists")
        },
        .package = "SelfControlledCohort"
    )

    # Test execute with NULL diagnostics settings to trigger defaults
    jobContext$moduleExecutionSettings$settings$runDiagnostics <- NULL
    jobContext$moduleExecutionSettings$settings$diagnostics <- NULL
    jobContext$moduleExecutionSettings$settings$diagnosticThresholds <- NULL
    # Remove manifest to allow execution
    unlink(file.path(sccResultsPath, "manifest.json"))

    testthat::with_mocked_bindings(
        {
            testthat::with_mocked_bindings(
                {
                    expect_no_error(execute(jobContext))
                },
                getCohortTableNames = function(baseName) {
                    list(cohortTable = baseName)
                },
                .package = "CohortGenerator"
            )
        },
        runSelfControlledCohort = function(...) {
            args <- list(...)
            # Verify defaults are set
            expect_true(args$runDiagnostics)
            expect_equal(args$diagnostics, "all")
            expect_type(args$diagnosticThresholds, "list")

            dir.create(args$resultExportPath, recursive = TRUE, showWarnings = FALSE)
            write("{}", file.path(args$resultExportPath, "manifest.json"))
            return(invisible(NULL))
        },
        .package = "SelfControlledCohort"
    )
})
