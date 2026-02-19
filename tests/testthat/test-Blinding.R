library(testthat)
library(SelfControlledCohort)

test_that("computeBlindingStatus works with all diagnostics passing", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "TIME_TREND_P_VALUE", "SPARSE_EXPOSED", "SPARSE_UNEXPOSED"),
        diagnostic_value = c(1.5, 0.5, 10, 10),
        pass = c(1, 1, 1, 1)
    )

    blindingRows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    expect_equal(nrow(blindingRows), 2)
    expect_true("UNBLIND" %in% blindingRows$diagnostic_name)
    expect_true("UNBLIND_FOR_CALIBRATION" %in% blindingRows$diagnostic_name)

    unblindRow <- blindingRows[blindingRows$diagnostic_name == "UNBLIND", ]
    unblindCalibrationRow <- blindingRows[blindingRows$diagnostic_name == "UNBLIND_FOR_CALIBRATION", ]

    expect_equal(unblindRow$pass, 1L)
    expect_equal(unblindCalibrationRow$pass, 1L)
})

test_that("computeBlindingStatus works with MDRR failing", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "TIME_TREND_P_VALUE", "SPARSE_EXPOSED", "SPARSE_UNEXPOSED"),
        diagnostic_value = c(2.5, 0.5, 10, 10),
        pass = c(0, 1, 1, 1) # MDRR fails
    )

    blindingRows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    unblindRow <- blindingRows[blindingRows$diagnostic_name == "UNBLIND", ]
    unblindCalibrationRow <- blindingRows[blindingRows$diagnostic_name == "UNBLIND_FOR_CALIBRATION", ]

    expect_equal(unblindRow$pass, 0L) # Should fail unblind
    expect_equal(unblindCalibrationRow$pass, 1L) # Should pass calibration gate (non-MDRR pass)
})

test_that("computeBlindingStatus works with non-MDRR diagnostic failing", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "TIME_TREND_P_VALUE", "SPARSE_EXPOSED", "SPARSE_UNEXPOSED"),
        diagnostic_value = c(1.5, 0.01, 10, 10),
        pass = c(1, 0, 1, 1) # Time trend fails
    )

    blindingRows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    unblindRow <- blindingRows[blindingRows$diagnostic_name == "UNBLIND", ]
    unblindCalibrationRow <- blindingRows[blindingRows$diagnostic_name == "UNBLIND_FOR_CALIBRATION", ]

    expect_equal(unblindRow$pass, 0L)
    expect_equal(unblindCalibrationRow$pass, 0L)
})

test_that("computeBlindingStatus handles multiple pairs", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = c(1, 1, 2, 2),
        outcome_cohort_id = c(10, 10, 20, 20),
        diagnostic_name = c("MDRR", "SPARSE_EXPOSED", "MDRR", "SPARSE_EXPOSED"),
        diagnostic_value = c(1.5, 10, 2.5, 10),
        pass = c(1, 1, 0, 1)
    )

    blindingRows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    # 2 pairs * 2 blinding types = 4 rows
    expect_equal(nrow(blindingRows), 4)

    # Pair 1 (all pass)
    pair1Unblind <- blindingRows[blindingRows$target_cohort_id == 1 & blindingRows$diagnostic_name == "UNBLIND", ]
    expect_equal(pair1Unblind$pass, 1L)

    # Pair 2 (MDRR fail)
    pair2Unblind <- blindingRows[blindingRows$target_cohort_id == 2 & blindingRows$diagnostic_name == "UNBLIND", ]
    expect_equal(pair2Unblind$pass, 0L)
    pair2Calib <- blindingRows[blindingRows$target_cohort_id == 2 & blindingRows$diagnostic_name == "UNBLIND_FOR_CALIBRATION", ]
    expect_equal(pair2Calib$pass, 1L)
})

test_that("getDiagnosticsSummary works correctly", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 10,
        diagnostic_name = c("SPARSE_EXPOSED", "UNBLIND", "UNBLIND_FOR_CALIBRATION"),
        diagnostic_value = c(10, NA, NA),
        pass = c(1, 1, 1)
    )

    summary <- getDiagnosticsSummary(diagnosticResults)

    expect_equal(nrow(summary), 1)
    expect_true("UNBLIND" %in% names(summary))
    expect_true("UNBLIND_FOR_CALIBRATION" %in% names(summary))
    expect_equal(summary$UNBLIND, 1L)
})
