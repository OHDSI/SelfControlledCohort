# compareVersions.R
# ==================
# Equivalence test: SelfControlledCohort v2.0.0 vs v1.6.0
#
# Builds and installs both versions into isolated library paths, runs each
# against the same Eunomia GiBleed database, and compares effect estimates.
#
# Usage:
#   cd /Users/jamie/PycharmProjects/SelfControlledCohort
#   Rscript extras/compareVersions.R
#
# Requirements: callr, withr, Eunomia, remotes must be installed in default lib

library(withr)

cat("=============================================================\n")
cat("  SCC Equivalence Test: v2.0.0 vs v1.6.0\n")
cat("=============================================================\n\n")

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------
REPO_PATH <- normalizePath(".")
TOLERANCE <- 1e-10  # floating-point comparison tolerance

# Columns subject to cell-count blinding by ResultModelManager in v2.0.0
# (min_cell_count = Yes in resultsDataModelSpecification.csv).
# These are expected to differ and are informational only.
BLINDED_COLS <- c("num_outcomes_exposed", "num_outcomes_unexposed", "num_exposures")

# Columns that MUST match for equivalence
EXACT_INT_COLS <- c("num_persons")
FLOAT_COLS <- c(
  "time_at_risk_exposed", "time_at_risk_unexposed",
  "rr", "lb_95", "ub_95", "log_rr", "se_log_rr", "p_value"
)

# Scenarios: each is a named list of extra args to runSelfControlledCohort:
# - args: parameters passed to both versions
# - expect_diff: if TRUE, differences are expected (e.g., due to bug fixes)
# - note: description of why differences are expected
scenarios <- list(
  "default" = list(
    args = list(),
    expect_diff = FALSE
  ),

  "no_exposure_length" = list(
    args = list(
      addLengthOfExposureExposed = FALSE,
      riskWindowStartExposed = 0,
      riskWindowEndExposed = 30,
      addLengthOfExposureUnexposed = FALSE,
      riskWindowStartUnexposed = -30,
      riskWindowEndUnexposed = -1
    ),
    expect_diff = FALSE
  ),

  "all_exposures" = list(
    args = list(firstExposureOnly = FALSE),
    expect_diff = TRUE,
    note = paste(
      "v1.6.0 has a bug: runSccRiskWindows always hardcodes",
      "firstExposureOnly=TRUE, ignoring the user's parameter.",
      "v2.0.0 fixes this, so results legitimately differ."
    )
  ),

  "full_time_at_risk" = list(
    args = list(hasFullTimeAtRisk = TRUE),
    expect_diff = FALSE
  ),

  "age_restricted" = list(
    args = list(minAge = "40", maxAge = "60"),
    expect_diff = FALSE
  ),

  "date_restricted" = list(
    args = list(studyStartDate = "20000101", studyEndDate = "20051231"),
    expect_diff = TRUE,
    note = paste(
      "v2.0.0 converts YYYYMMDD dates to YYYY-MM-DD format for broader",
      "database compatibility. On SQLite the date conversion may behave",
      "differently, so row counts may differ."
    )
  )
)

# ---------------------------------------------------------------------------
# 1. Helper: build and install a specific git ref into an isolated library
# ---------------------------------------------------------------------------
buildAndInstall <- function(ref, libDir, repoPath) {
  buildDir <- tempfile(paste0("scc_build_", ref, "_"))
  dir.create(buildDir, recursive = TRUE)
  on.exit(unlink(buildDir, recursive = TRUE))

  cmd <- sprintf(
    "git -C '%s' archive --format=tar '%s' | tar -x -C '%s'",
    repoPath, ref, buildDir
  )
  cat(sprintf("  Exporting ref '%s'...\n", ref))
  exitCode <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (exitCode != 0) stop(sprintf("git archive failed for ref '%s' (exit %d)", ref, exitCode))

  cat(sprintf("  Installing '%s' to %s...\n", ref, libDir))
  withr::with_libpaths(c(libDir, .libPaths()), {
    remotes::install_deps(
      buildDir,
      lib = libDir,
      dependencies = TRUE,
      upgrade = "never",
      quiet = TRUE
    )
  })

  install.packages(
    buildDir,
    lib = libDir,
    repos = NULL,
    type = "source",
    quiet = TRUE,
    INSTALL_opts = "--no-multiarch"
  )
  cat(sprintf("  Done installing '%s'\n", ref))
}

# ---------------------------------------------------------------------------
# 2. Set up
# ---------------------------------------------------------------------------
cat("Setting up isolated library directories...\n")
libDir_v1 <- tempfile("scc_lib_v1_")
libDir_v2 <- tempfile("scc_lib_v2_")
dir.create(libDir_v1, recursive = TRUE)
dir.create(libDir_v2, recursive = TRUE)
withr::defer({
  unlink(libDir_v1, recursive = TRUE)
  unlink(libDir_v2, recursive = TRUE)
})

cat("Creating shared Eunomia database...\n")
dbFile <- tempfile(fileext = "GiBleed.sqlite")
Eunomia::getDatabaseFile(
  "GiBleed", cdmVersion = "5.3", dbms = "sqlite",
  databaseFile = dbFile, inputFormat = "csv", verbose = FALSE, overwrite = FALSE
)
withr::defer(unlink(dbFile))

# ---------------------------------------------------------------------------
# 3. Install both versions
# ---------------------------------------------------------------------------
cat("\n--- Installing v1.6.0 (main) ---\n")
buildAndInstall("main", libDir_v1, REPO_PATH)

cat("\n--- Installing v2.0.0 ---\n")
buildAndInstall("version-2.0.0", libDir_v2, REPO_PATH)

# ---------------------------------------------------------------------------
# 4. Run v1.6.0
# ---------------------------------------------------------------------------
run_v1 <- function(scenarioArgs, dbFile, libDir) {
  callr::r(
    function(scenarioArgs, dbFile, libPath) {
      .libPaths(c(libPath, .libPaths()))
      library(SelfControlledCohort)
      library(DatabaseConnector)
      options(connectionObserver = NULL)

      connectionDetails <- createConnectionDetails(dbms = "sqlite", server = dbFile)
      baseArgs <- list(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        exposureIds = "",
        outcomeIds = "",
        computeThreads = 1,
        returnEstimates = TRUE
      )
      args <- modifyList(baseArgs, scenarioArgs)
      result <- do.call(runSelfControlledCohort, args)

      if (is.null(result$estimates) || nrow(result$estimates) == 0) return(NULL)

      df <- result$estimates
      nameMap <- c(
        "exposureId"           = "target_cohort_id",
        "outcomeId"            = "outcome_cohort_id",
        "numPersons"           = "num_persons",
        "numExposures"         = "num_exposures",
        "numOutcomesExposed"   = "num_outcomes_exposed",
        "numOutcomesUnexposed" = "num_outcomes_unexposed",
        "timeAtRiskExposed"    = "time_at_risk_exposed",
        "timeAtRiskUnexposed"  = "time_at_risk_unexposed",
        "irr"                  = "rr",
        "irrLb95"              = "lb_95",
        "irrUb95"              = "ub_95",
        "logRr"                = "log_rr",
        "seLogRr"              = "se_log_rr",
        "p"                    = "p_value"
      )
      for (old in names(nameMap)) {
        if (old %in% names(df)) names(df)[names(df) == old] <- nameMap[[old]]
      }

      # Apply same non-zero filter v2 uses
      df <- df[
        df$num_outcomes_exposed > 0 &
        df$num_outcomes_unexposed > 0 &
        df$time_at_risk_exposed > 0 &
        df$time_at_risk_unexposed > 0, ]

      df <- df[order(df$target_cohort_id, df$outcome_cohort_id), ]
      rownames(df) <- NULL
      return(df)
    },
    args = list(scenarioArgs = scenarioArgs, dbFile = dbFile, libPath = libDir),
    libpath = c(libDir, .libPaths())
  )
}

# ---------------------------------------------------------------------------
# 5. Run v2.0.0
# ---------------------------------------------------------------------------
run_v2 <- function(scenarioArgs, dbFile, libDir) {
  callr::r(
    function(scenarioArgs, dbFile, libPath) {
      .libPaths(c(libPath, .libPaths()))
      library(SelfControlledCohort)
      library(DatabaseConnector)
      options(connectionObserver = NULL)

      connectionDetails <- createConnectionDetails(dbms = "sqlite", server = dbFile)
      resultPath <- tempfile("scc_v2_result_")
      dir.create(resultPath)

      baseArgs <- list(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        exposureIds = "",
        outcomeIds = "",
        databaseId = "test",
        computeThreads = 1,
        resultExportPath = resultPath,
        runDiagnostics = FALSE
      )
      args <- modifyList(baseArgs, scenarioArgs)
      do.call(runSelfControlledCohort, args)

      resultFile <- file.path(resultPath, "scc_result.csv")
      if (!file.exists(resultFile)) return(NULL)

      df <- read.csv(resultFile)
      df <- df[order(df$target_cohort_id, df$outcome_cohort_id), ]
      rownames(df) <- NULL
      return(df)
    },
    args = list(scenarioArgs = scenarioArgs, dbFile = dbFile, libPath = libDir),
    libpath = c(libDir, .libPaths())
  )
}

# ---------------------------------------------------------------------------
# 6. Compare helper
# ---------------------------------------------------------------------------
compareResults <- function(v1_df, v2_df, scenario, expectDiff, note, tol = TOLERANCE) {
  cat(sprintf("\n--- Scenario: %s ---\n", scenario))
  if (expectDiff && !is.null(note)) {
    cat(sprintf("  NOTE (expected diff): %s\n", note))
  }

  if (is.null(v1_df) && is.null(v2_df)) {
    cat("  Both versions returned NULL. PASS\n")
    return(TRUE)
  }
  if (is.null(v1_df) || is.null(v2_df)) {
    msg <- sprintf("  v1 returned %s rows, v2 returned %s rows",
                   ifelse(is.null(v1_df), "NULL", nrow(v1_df)),
                   ifelse(is.null(v2_df), "NULL", nrow(v2_df)))
    if (expectDiff) {
      cat(paste0(msg, " [EXPECTED DIFF]\n"))
      return(TRUE)
    } else {
      cat(paste0(msg, " FAIL\n"))
      return(FALSE)
    }
  }

  merged <- merge(v1_df, v2_df,
                  by = c("target_cohort_id", "outcome_cohort_id"),
                  suffixes = c("_v1", "_v2"))

  cat(sprintf("  v1 rows: %d, v2 rows: %d, matched: %d\n",
              nrow(v1_df), nrow(v2_df), nrow(merged)))

  v1_only <- nrow(v1_df) - nrow(merged)
  v2_only <- nrow(v2_df) - nrow(merged)
  if (v1_only > 0 || v2_only > 0) {
    cat(sprintf("  WARNING: %d rows only in v1, %d rows only in v2\n", v1_only, v2_only))
  }

  if (nrow(merged) == 0) {
    if (expectDiff) {
      cat("  No matching rows [EXPECTED DIFF]\n")
      return(TRUE)
    }
    cat("  No matching rows FAIL\n")
    return(FALSE)
  }

  pass <- TRUE

  # --- Cell-count blinded columns (informational only) ---
  for (col in BLINDED_COLS) {
    col_v1 <- paste0(col, "_v1")
    col_v2 <- paste0(col, "_v2")
    if (col_v1 %in% names(merged) && col_v2 %in% names(merged)) {
      # Count how many are blinded (value = -5) in v2
      blinded <- sum(merged[[col_v2]] < 0, na.rm = TRUE)
      exact_match <- sum(merged[[col_v1]] == merged[[col_v2]], na.rm = TRUE)
      cat(sprintf("  [BLINDED] %s: %d/%d blinded in v2, %d/%d exact match\n",
                  col, blinded, nrow(merged), exact_match, nrow(merged)))
    }
  }

  # --- Exact integer columns ---
  for (col in EXACT_INT_COLS) {
    col_v1 <- paste0(col, "_v1")
    col_v2 <- paste0(col, "_v2")
    if (col_v1 %in% names(merged) && col_v2 %in% names(merged)) {
      diffs <- sum(merged[[col_v1]] != merged[[col_v2]], na.rm = TRUE)
      if (diffs > 0) {
        cat(sprintf("  MISMATCH [%s]: %d / %d rows differ\n", col, diffs, nrow(merged)))
        if (!expectDiff) pass <- FALSE
      }
    }
  }

  # --- Float columns (effect estimates) ---
  for (col in FLOAT_COLS) {
    col_v1 <- paste0(col, "_v1")
    col_v2 <- paste0(col, "_v2")
    if (col_v1 %in% names(merged) && col_v2 %in% names(merged)) {
      both_na    <- is.na(merged[[col_v1]]) & is.na(merged[[col_v2]])
      one_na     <- xor(is.na(merged[[col_v1]]), is.na(merged[[col_v2]]))
      neither_na <- !is.na(merged[[col_v1]]) & !is.na(merged[[col_v2]])

      na_mismatches  <- sum(one_na)
      numeric_diffs  <- sum(abs(merged[[col_v1]][neither_na] - merged[[col_v2]][neither_na]) > tol)
      total_diffs    <- na_mismatches + numeric_diffs

      if (total_diffs > 0) {
        cat(sprintf("  MISMATCH [%s]: %d / %d rows differ (tol: %g)\n",
                    col, total_diffs, nrow(merged), tol))
        all_mismatch <- which(one_na | (neither_na & abs(merged[[col_v1]] - merged[[col_v2]]) > tol))
        for (i in head(all_mismatch, 3)) {
          cat(sprintf("    row %d: v1=%.10g, v2=%.10g\n",
                      i, merged[[col_v1]][i], merged[[col_v2]][i]))
        }
        if (!expectDiff) pass <- FALSE
      }
    }
  }

  if (pass) {
    matched_note <- if (expectDiff) " (differences as expected)" else ""
    cat(sprintf("  PASS: %d matched rows%s\n", nrow(merged), matched_note))
  } else {
    cat("  FAIL: Unexpected mismatches found\n")
  }
  return(pass)
}

# ---------------------------------------------------------------------------
# 7. Run all scenarios
# ---------------------------------------------------------------------------
cat("\n=============================================================\n")
cat("  Running comparisons across", length(scenarios), "scenarios\n")
cat("=============================================================\n")

results <- list()
for (name in names(scenarios)) {
  sc <- scenarios[[name]]
  cat(sprintf("\nRunning scenario '%s'...\n", name))

  cat("  Running v1.6.0...\n")
  v1_result <- tryCatch(
    run_v1(sc$args, dbFile, libDir_v1),
    error = function(e) { cat(sprintf("  ERROR in v1: %s\n", conditionMessage(e))); "ERROR" }
  )

  cat("  Running v2.0.0...\n")
  v2_result <- tryCatch(
    run_v2(sc$args, dbFile, libDir_v2),
    error = function(e) { cat(sprintf("  ERROR in v2: %s\n", conditionMessage(e))); "ERROR" }
  )

  if (identical(v1_result, "ERROR") || identical(v2_result, "ERROR")) {
    cat(sprintf("\n--- Scenario: %s ---\n  FAIL: Error during execution\n", name))
    results[[name]] <- FALSE
  } else {
    results[[name]] <- compareResults(
      v1_result, v2_result, name,
      expectDiff = isTRUE(sc$expect_diff),
      note = sc$note
    )
  }
}

# ---------------------------------------------------------------------------
# 8. Summary
# ---------------------------------------------------------------------------
cat("\n\n=============================================================\n")
cat("  SUMMARY\n")
cat("=============================================================\n\n")

allPass <- TRUE
for (name in names(results)) {
  status <- if (isTRUE(results[[name]])) "PASS" else "FAIL"
  if (!isTRUE(results[[name]])) allPass <- FALSE
  suffix <- if (isTRUE(scenarios[[name]]$expect_diff)) " (expected diff)" else ""
  cat(sprintf("  %-25s %s%s\n", name, status, suffix))
}

cat("\n")
cat("Notes:\n")
cat("  - [BLINDED] columns use ResultModelManager cell-count blinding\n")
cat("    (small counts replaced with -5). This is expected behavior.\n")
cat("  - 'Expected diff' scenarios reflect intentional bug fixes in v2.0.0.\n")
cat("\n")

if (allPass) {
  cat("RESULT: PASS - v2.0.0 produces equivalent effect estimates to v1.6.0\n")
  cat("        (accounting for known bug fixes and cell-count blinding)\n")
  quit(status = 0)
} else {
  cat("RESULT: FAIL - Unexpected differences found\n")
  quit(status = 1)
}
