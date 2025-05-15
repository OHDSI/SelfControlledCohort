/************************************************************************
Copyright 2025 Observational Health Data Sciences and Informatics

This file is part of SelfControlledCohort

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/

{@outcome_ids != ''} ? {
TRUNCATE TABLE #scc_outcome_ids;
DROP TABLE IF EXISTS #scc_outcome_ids;
}

{@exposure_ids != ''} ? {
TRUNCATE TABLE #scc_exposure_ids;
DROP TABLE IF EXISTS #scc_exposure_ids;
}

TRUNCATE TABLE #scc_exposure_summary;
DROP TABLE IF EXISTS #scc_exposure_summary;

TRUNCATE TABLE #scc_outcome_summary;
DROP TABLE IF EXISTS #scc_outcome_summary;

{@drop_results_table} ? {
TRUNCATE TABLE @risk_windows_table;
DROP TABLE IF EXISTS @risk_windows_table;

TRUNCATE TABLE @results_table;
DROP TABLE IF EXISTS @results_table;
}
