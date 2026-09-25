library(testthat)
library(SelfControlledCohort)

test_that("computeBlindingStatus: all diagnostics pass (EASE pass)", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "PRE_EXPOSURE_P_VALUE", "EVENT_DEPENDENT_OBSERVATION"),
        diagnostic_value = c(1.5, 0.5, 0.1),
        pass = c(1L, 1L, 1L)
    )
    diagnosticResults <- rbind(diagnosticResults, data.frame(
        database_id = "test", analysis_id = 1, target_cohort_id = 1, outcome_cohort_id = 0,
        diagnostic_name = "EASE", diagnostic_value = 0.1, pass = 1L
    ))

    rows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    expect_equal(nrow(rows), 2)
    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND"], 1L)
    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND_FOR_CALIBRATION"], 1L)
    expect_false(any(rows$outcome_cohort_id == 0))
})

test_that("computeBlindingStatus: MDRR failing blocks UNBLIND but not UNBLIND_FOR_CALIBRATION", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "PRE_EXPOSURE_P_VALUE", "EVENT_DEPENDENT_OBSERVATION"),
        diagnostic_value = c(2.5, 0.5, 0.1),
        pass = c(0L, 1L, 1L)
    )
    diagnosticResults <- rbind(diagnosticResults, data.frame(
        database_id = "test", analysis_id = 1, target_cohort_id = 1, outcome_cohort_id = 0,
        diagnostic_name = "EASE", diagnostic_value = 0.1, pass = 1L
    ))

    rows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND"], 0L)
    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND_FOR_CALIBRATION"], 1L)
})

test_that("computeBlindingStatus: non-MDRR diagnostic failing blocks both", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "PRE_EXPOSURE_P_VALUE", "EVENT_DEPENDENT_OBSERVATION"),
        diagnostic_value = c(1.5, 0.01, 0.1),
        pass = c(1L, 0L, 1L)
    )
    diagnosticResults <- rbind(diagnosticResults, data.frame(
        database_id = "test", analysis_id = 1, target_cohort_id = 1, outcome_cohort_id = 0,
        diagnostic_name = "EASE", diagnostic_value = 0.1, pass = 1L
    ))

    rows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND"], 0L)
    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND_FOR_CALIBRATION"], 0L)
})

test_that("computeBlindingStatus: EASE failing blocks both", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "PRE_EXPOSURE_P_VALUE", "EVENT_DEPENDENT_OBSERVATION"),
        diagnostic_value = c(1.5, 0.5, 0.1),
        pass = c(1L, 1L, 1L)
    )
    diagnosticResults <- rbind(diagnosticResults, data.frame(
        database_id = "test", analysis_id = 1, target_cohort_id = 1, outcome_cohort_id = 0,
        diagnostic_name = "EASE", diagnostic_value = 0.5, pass = 0L
    ))

    rows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND"], 0L)
    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND_FOR_CALIBRATION"], 0L)
})

test_that("computeBlindingStatus: EASE not evaluated (pass = NA) is pass-through", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "PRE_EXPOSURE_P_VALUE", "EVENT_DEPENDENT_OBSERVATION"),
        diagnostic_value = c(1.5, 0.5, 0.1),
        pass = c(1L, 1L, 1L)
    )
    diagnosticResults <- rbind(diagnosticResults, data.frame(
        database_id = "test", analysis_id = 1, target_cohort_id = 1, outcome_cohort_id = 0,
        diagnostic_name = "EASE", diagnostic_value = NA_real_, pass = NA_integer_
    ))

    rows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND"], 1L)
    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND_FOR_CALIBRATION"], 1L)
})

test_that("computeBlindingStatus: EASE missing entirely is pass-through", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 4,
        diagnostic_name = c("MDRR", "PRE_EXPOSURE_P_VALUE", "EVENT_DEPENDENT_OBSERVATION"),
        diagnostic_value = c(1.5, 0.5, 0.1),
        pass = c(1L, 1L, 1L)
    )

    rows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    expect_equal(nrow(rows), 2)
    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND"], 1L)
    expect_equal(rows$pass[rows$diagnostic_name == "UNBLIND_FOR_CALIBRATION"], 1L)
})

test_that("computeBlindingStatus: EASE applies to every outcome of a target", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = c(1, 1, 2, 2),
        outcome_cohort_id = c(10, 11, 20, 21),
        diagnostic_name = c("MDRR", "MDRR", "MDRR", "MDRR"),
        diagnostic_value = c(1.5, 1.5, 1.5, 1.5),
        pass = c(1L, 1L, 1L, 1L)
    )
    # EASE: target 1 fails, target 2 passes
    diagnosticResults <- rbind(diagnosticResults, data.frame(
        database_id = "test", analysis_id = 1,
        target_cohort_id = c(1, 2),
        outcome_cohort_id = c(0, 0),
        diagnostic_name = c("EASE", "EASE"),
        diagnostic_value = c(0.5, 0.1),
        pass = c(0L, 1L)
    ))

    rows <- SelfControlledCohort:::.computeBlindingStatus(diagnosticResults)

    # 4 pairs * 2 blinding types = 8 rows, none for outcome = 0
    expect_equal(nrow(rows), 8)
    expect_false(any(rows$outcome_cohort_id == 0))

    # target 1 outcomes are blinded by EASE, target 2 outcomes are not
    t1 <- rows[rows$target_cohort_id == 1 & rows$diagnostic_name == "UNBLIND", ]
    t2 <- rows[rows$target_cohort_id == 2 & rows$diagnostic_name == "UNBLIND", ]
    expect_equal(nrow(t1), 2)
    expect_true(all(t1$pass == 0L))
    expect_true(all(t2$pass == 1L))
})

test_that("getDiagnosticsSummary works correctly", {
    diagnosticResults <- data.frame(
        database_id = "test",
        analysis_id = 1,
        target_cohort_id = 1,
        outcome_cohort_id = 10,
        diagnostic_name = c("MDRR", "UNBLIND", "UNBLIND_FOR_CALIBRATION"),
        diagnostic_value = c(1.5, NA, NA),
        pass = c(1L, 1L, 1L)
    )

    summary <- getDiagnosticsSummary(diagnosticResults)

    expect_equal(nrow(summary), 1)
    expect_true("UNBLIND" %in% names(summary))
    expect_true("UNBLIND_FOR_CALIBRATION" %in% names(summary))
    expect_equal(summary$UNBLIND, 1L)
})
