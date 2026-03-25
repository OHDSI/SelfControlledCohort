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
            runSelfControlledCohortArgs = list(),
            controlType = "outcome",
            runDiagnostics = TRUE,
            diagnostics = c("all"),
            diagnosticThresholds = list()
        )
    )
    exposureOutcome1 <- createExposureOutcome(1, 3)
    exposureOutcome2 <- createExposureOutcome(2, 4)
    exposureOutcomeList <- list(exposureOutcome1, exposureOutcome2)

    specs <- createSelfControlledCohortModuleSpecifications(
        analysisSettings = analysisSettings,
        exposureOutcomeList = exposureOutcomeList
    )

    expect_equal(specs$module, "SelfControlledCohortModule")
    expect_equal(specs$parameters$exposureOutcomeList, exposureOutcomeList)
    expect_equal(specs$parameters$analysisSettings[[1]]$controlType, "outcome")
})