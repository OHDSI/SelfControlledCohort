# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of SelfControlledCohort
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

test_that("testPreExposureGain identifies biased data", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails(dbms = "duckdb")
  connection <- connect(connectionDetails)
  on.exit(disconnect(connection))

  DatabaseConnector::renderTranslateExecuteSql(connection, "
    DROP TABLE IF EXISTS test_risk_windows;
    CREATE TABLE test_risk_windows (person_id INT, exposure_id INT, analysis_id INT, exposure_start_date DATE);
    INSERT INTO test_risk_windows VALUES (1, 1, 1, CAST('20200101' AS DATE)), (2, 1, 1, CAST('20200101' AS DATE));
    
    DROP TABLE IF EXISTS observation_period;
    CREATE TABLE observation_period (person_id INT, observation_period_start_date DATE, observation_period_end_date DATE);
    INSERT INTO observation_period VALUES (1, CAST('20190101' AS DATE), CAST('20211231' AS DATE)), (2, CAST('20190101' AS DATE), CAST('20211231' AS DATE));

    DROP TABLE IF EXISTS test_outcomes;
    CREATE TABLE test_outcomes (subject_id INT, cohort_definition_id INT, cohort_start_date DATE);
    -- High risk before: 10 outcomes before, 0 after
    INSERT INTO test_outcomes VALUES (2, 10, CAST('20191215' AS DATE)), 
                                     (2, 10, CAST('20191216' AS DATE)),
                                     (2, 10, CAST('20191217' AS DATE)),
                                     (2, 10, CAST('20191218' AS DATE)),
                                     (2, 10, CAST('20191219' AS DATE)),
                                     (2, 10, CAST('20191220' AS DATE)),
                                     (2, 10, CAST('20191221' AS DATE)),
                                     (2, 10, CAST('20191222' AS DATE)),
                                     (2, 10, CAST('20191223' AS DATE)),
                                     (2, 10, CAST('20191224' AS DATE));
  ")

  results <- SelfControlledCohort:::testPreExposureGain(
    connection = connection,
    riskWindowsTable = "test_risk_windows",
    outcomeTable = "test_outcomes",
    outcomeDatabaseSchema = "",
    cdmDatabaseSchema = "",
    analysisId = 1,
    tempEmulationSchema = NULL
  )

  expect_gt(results$preExposureRateRatio[1], 1)
  expect_lt(results$pValue[1], 0.05)
})



test_that(".computeEventDependentDiagnostic identifies censoring", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails(dbms = "duckdb")
  connection <- connect(connectionDetails)
  on.exit(disconnect(connection))

  op <- querySql(connection, "SELECT person_id, observation_period_end_date FROM main.observation_period LIMIT 2")
  colnames(op) <- tolower(colnames(op))
  
  p1_end <- gsub("-", "", as.character(op$observation_period_end_date[1]))
  p2_end <- gsub("-", "", as.character(op$observation_period_end_date[2]))
  
  DatabaseConnector::renderTranslateExecuteSql(connection, "
    CREATE TABLE risk_windows_ed (person_id INT, exposure_id INT, analysis_id INT, 
                                 risk_window_start_unexposed DATE, risk_window_end_exposed DATE);
    INSERT INTO risk_windows_ed VALUES (@p1, 1, 1, CAST('20000101' AS DATE), CAST('20991231' AS DATE)), (@p2, 1, 1, CAST('20000101' AS DATE), CAST('20991231' AS DATE));
    DROP TABLE IF EXISTS condition_occurrence;
    CREATE TABLE condition_occurrence (person_id INT, condition_concept_id INT, condition_start_date DATE);
    INSERT INTO condition_occurrence (person_id, condition_concept_id, condition_start_date)
    VALUES (@p1, 10, DATEADD(day, -60, CAST('@p1_end' AS DATE))), (@p2, 10, DATEADD(day, -5, CAST('@p2_end' AS DATE)));
  ", p1 = op$person_id[1], p2 = op$person_id[2], p1_end = p1_end, p2_end = p2_end)

  thresholds <- getDefaultDiagnosticThresholds()
  thresholds$maxEventDependentCensoring <- 0.1

  results <- SelfControlledCohort:::.computeEventDependentDiagnostic(
    connection = connection,
    cdmDatabaseSchema = "main",
    riskWindowsTable = "risk_windows_ed",
    outcomeTable = "condition_occurrence", 
    outcomeDatabaseSchema = "main",
    analysisId = 1,
    thresholds = thresholds,
    tempEmulationSchema = NULL
  )

  expect_equal(results$diagnostic_value[1], 0.5)
  expect_equal(results$pass[1], 0L)
})

test_that("computeEase calculates systematic error correctly", {
  negatives_clean <- data.frame(rr = c(0.9, 1.1, 1.0, 1.2, 0.8), seLogRr = 0.1)
  ease_clean <- computeEase(negatives_clean)
  expect_true(ease_clean < 0.1)
  
  negatives_biased <- data.frame(rr = c(2.0, 2.5, 3.0, 2.2, 2.8), seLogRr = 0.1)
  ease_biased <- computeEase(negatives_biased)
  expect_true(ease_biased > 0.5)
  
  expect_true(is.na(computeEase(data.frame(rr = 1, seLogRr = 0.1))))
})
