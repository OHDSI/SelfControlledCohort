/************************************************************************
Copyright 2016 Observational Health Data Sciences and Informatics

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
{DEFAULT @cdm_database_schema = 'cdm4_sim'}
{DEFAULT @exposure_database_schema = 'cdm4_sim'} 
{DEFAULT @exposure_table = 'drug_era'} 
{DEFAULT @exposure_start_date = 'drug_era_start_date'} 
{DEFAULT @exposure_end_date = 'drug_era_end_date'} 
{DEFAULT @exposure_concept_id = 'drug_concept_id'} 
{DEFAULT @exposure_person_id = 'person_id'} 
{DEFAULT @outcome_database_schema = 'cdm4_sim'} 
{DEFAULT @outcome_table = 'condition_era'} 
{DEFAULT @outcome_start_date = 'condition_era_start_date'} 
{DEFAULT @outcome_end_date = 'condition_era_end_date'} 
{DEFAULT @outcome_concept_id = 'condition_concept_id'} 
{DEFAULT @outcome_person_id = 'person_id'} 
{DEFAULT @first_occurrence_drug_only = TRUE} 
{DEFAULT @first_occurrence_condition_only = TRUE} 
{DEFAULT @outcome_condition_type_concept_ids = 38000247} 
{DEFAULT @exposure_ids = '1125315, 1713332, 19011773, 1174888'} 
{DEFAULT @outcome_id = 444382} 
{DEFAULT @gender_concept_ids = '8507,8532'} 
{DEFAULT @min_age = 0} 
{DEFAULT @max_age = 100} 
{DEFAULT @study_start_date = ''} 
{DEFAULT @study_end_date = ''} 
{DEFAULT @stratify_by_gender = TRUE} 
{DEFAULT @stratify_by_age = TRUE} 
{DEFAULT @stratify_by_year = TRUE} 
{DEFAULT @use_length_of_exposure_exposed = TRUE} 
{DEFAULT @time_at_risk_exposed_start = 1} 
{DEFAULT @surveillance_exposed = 30} 
{DEFAULT @use_length_of_exposure_unexposed = TRUE} 
{DEFAULT @time_at_risk_unexposed_start = -1} 
{DEFAULT @surveillance_unexposed = -30} 
{DEFAULT @has_full_time_at_risk = TRUE} 
{DEFAULT @washout_window = 183} 
{DEFAULT @followup_window = 183} 
{DEFAULT @shrinkage = '0.0001'} 

IF OBJECT_ID('tempdb..#results', 'U') IS NOT NULL
	DROP TABLE #results;

IF OBJECT_ID('tempdb..#age_group', 'U') IS NOT NULL
	DROP TABLE #results;

CREATE TABLE #age_group (
	age_group_name VARCHAR(255),
	age_group_min INT,
	age_group_max INT
);

INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('0-9',0,9);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('10-19',10,19);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('20-29',20,29);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('30-39',30,39);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('40-49',40,49);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('50-59',50,59);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('60-69',60,69);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('70-79',70,79);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('80-89',80,89);
INSERT INTO #age_group (age_group_name, age_group_min, age_group_max) VALUES ('90-99',90,99);

SELECT
	d1.@exposure_concept_id AS exposure_concept_id,
	{@stratify_by_gender} ? {CAST(gender_concept_id AS VARCHAR) AS gender_concept_id,} : {CAST('ALL' AS VARCHAR) AS gender_concept_id,}
	{@stratify_by_age} ? {ag1.age_group_name,} : {CAST('ALL' AS VARCHAR) AS age_group_name,}
	{@stratify_by_year} ? {CAST(YEAR(d1.@exposure_start_date) AS VARCHAR) AS index_year,} : {CAST('ALL' AS VARCHAR) AS index_year,}
	COUNT(DISTINCT d1.@exposure_person_id) AS num_persons,
	COUNT(d1.@exposure_person_id) AS num_exposures,
	SUM(
		--need to account for potential censoring due to observation period length	
		CASE WHEN 
			DATEADD(
				dd,
				{@use_length_of_exposure_exposed} ? {DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)} : {0} + @surveillance_exposed,
				@exposure_start_date
			) <= op1.observation_period_end_date
		THEN 
			DATEDIFF(
				dd,
				DATEADD(DAY, @time_at_risk_exposed_start, @exposure_start_date),	
				DATEADD(DAY, {@use_length_of_exposure_exposed} ? {DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)} : {0} + @surveillance_exposed,
				@exposure_start_date)
			)
		ELSE
			DATEDIFF(
				dd, 
				DATEADD(DAY, @time_at_risk_exposed_start, @exposure_start_date), 
				op1.observation_period_end_date
			)
		END
	) / 365.25 AS time_at_risk_exposed,		
	SUM(
		CASE WHEN 
			DATEADD(
				dd,
				{@use_length_of_exposure_unexposed} ? {-1*DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)} : {0} + @surveillance_unexposed,
				@exposure_start_date
			) >= op1.observation_period_start_date
		THEN 		
			DATEDIFF(
				dd,
				DATEADD(DAY, {@use_length_of_exposure_unexposed} ? {-1*DATEDIFF(DAY, d1.@exposure_start_date, d1.@exposure_end_date)} : {0} + @surveillance_unexposed, @exposure_start_date), 
				DATEADD(DAY, @time_at_risk_unexposed_start, @exposure_start_date)
			)
		ELSE  
			DATEDIFF(
				dd, 
				op1.observation_period_start_date, 
				DATEADD(DAY, @time_at_risk_unexposed_start, @exposure_start_date)
			)
		END
	) / 365.25 AS time_at_risk_unexposed
INTO 
	#scc_exposure_summary
FROM
	(
		SELECT 
			@exposure_person_id, 
			@exposure_concept_id, 
			@exposure_start_date, 
			@exposure_end_date
		FROM
			(
				SELECT 
					@exposure_person_id, 
					@exposure_concept_id, 
					@exposure_start_date, 
					@exposure_end_date
					{@first_occurrence_drug_only} ? {,ROW_NUMBER() OVER (PARTITION BY @exposure_person_id, @exposure_concept_id ORDER BY @exposure_start_date) AS rn1} 
				FROM 
					@exposure_database_schema.@exposure_table 
				WHERE 
					1=1
					{@exposure_ids != ''} ? {AND @exposure_concept_id IN (@exposure_ids)}
			) t1
		{@first_occurrence_drug_only} ? {WHERE rn1 = 1}
	) d1
	INNER JOIN
		@cdm_database_schema.person p1
	ON 
		d1.@exposure_person_id = p1.person_id
	INNER JOIN
		@cdm_database_schema.observation_period op1
	ON 
		d1.@exposure_person_id = op1.person_id,
	#age_group ag1
WHERE 
		d1.@exposure_start_date >= op1.observation_period_start_date
	AND 
		d1.@exposure_start_date <= op1.observation_period_end_date
	AND 
		YEAR(d1.@exposure_start_date) - year_of_birth >= ag1.age_group_min
	AND 
		YEAR(d1.@exposure_start_date) - year_of_birth <= ag1.age_group_max
	{@study_start_date != ''} ? {AND @exposure_start_date >= '@study_start_date'}
	{@study_end_date != ''} ? {AND @exposure_start_date <= '@study_end_date'}
	{@min_age != ''} ? {AND YEAR(d1.@exposure_start_date) - p1.year_of_birth  >= @min_age}
	{@max_age != ''} ? {AND YEAR(d1.@exposure_start_date) - p1.year_of_birth  <= @max_age}
	{@gender_concept_ids != ''} ? {AND p1.gender_concept_id IN (@gender_concept_ids)}
	{@has_full_time_at_risk}  ? {
		AND 
			op1.observation_period_end_date >= DATEADD(DAY, {@use_length_of_exposure_exposed} ? {DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)} : {0} + @surveillance_exposed, @exposure_start_date)
		AND 
			op1.observation_period_start_date <= DATEADD(DAY, {@use_length_of_exposure_exposed} ? {DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)} : {0} + @surveillance_unexposed, @exposure_start_date)	
	}
	{@washout_window != ''} ? {AND DATEDIFF(DAY, op1.observation_period_start_date, d1.@exposure_start_date) >= @washout_window}
	{@followup_window != ''} ? {AND DATEDIFF(DAY,d1.@exposure_start_date, op1.observation_period_end_date) >= @followup_window}
GROUP BY 
	d1.@exposure_concept_id
	{@stratify_by_gender} ? {, gender_concept_id}
	{@stratify_by_age} ? {, ag1.age_group_name}
	{@stratify_by_year} ? {, YEAR(d1.@exposure_start_date)}
;


--query 2:  number of events in each time window
SELECT 
	d1.@exposure_concept_id AS exposure_concept_id,
	c1.@outcome_concept_id AS outcome_concept_id,
	{@stratify_by_gender} ? {CAST(gender_concept_id AS VARCHAR) AS gender_concept_id,} : {CAST('ALL' AS VARCHAR) AS gender_concept_id,}
	{@stratify_by_age} ? {ag1.age_group_name,} : {CAST('ALL' AS VARCHAR) AS age_group_name,}
	{@stratify_by_year} ? {CAST(YEAR(d1.@exposure_start_date) AS VARCHAR) AS index_year,} : {CAST('ALL' AS VARCHAR) AS index_year,}
	SUM(
		CASE WHEN 
				c1.@outcome_start_date >= DATEADD(DAY,@time_at_risk_exposed_start,@exposure_start_date)
			AND 
				c1.@outcome_start_date <= DATEADD(DAY, {@use_length_of_exposure_exposed}?{DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)}:{0} + @surveillance_exposed ,@exposure_start_date)
			AND 
				c1.@outcome_start_date <= op1.observation_period_end_date
		THEN 
			1 
		ELSE 
			0 
		END
	) AS num_outcomes_exposed,
	SUM(
		CASE WHEN 
			c1.@outcome_start_date <= DATEADD(DAY,@time_at_risk_unexposed_start, @exposure_start_date)
		AND 
			c1.@outcome_start_date >= DATEADD(DAY,{@use_length_of_exposure_unexposed} ? {-1*DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)} : {0} + @surveillance_unexposed ,@exposure_start_date)	
		AND 
			c1.@outcome_start_date >= op1.observation_period_start_date
		THEN 
			1 
		ELSE 
			0 
		END
	) AS num_outcomes_unexposed
	--no additional time censored to be calculated, because doing so would violate assumption of Poisson rates and cause problems with patients with events in UNEXPOSED-time not counting EXPOSED-exposure time
INTO 
	#scc_outcome_summary
FROM
	(
		SELECT 
			@exposure_person_id, 
			@exposure_concept_id, 
			@exposure_start_date, 
			@exposure_end_date
		FROM
			(
				SELECT 
					@exposure_person_id, 
					@exposure_concept_id, 
					@exposure_start_date, 
					@exposure_end_date
					{@first_occurrence_drug_only}?{,ROW_NUMBER() OVER (PARTITION BY @exposure_person_id, @exposure_concept_id ORDER BY @exposure_start_date) AS rn1}
				FROM 
					@exposure_database_schema.@exposure_table 
				WHERE 
						1=1
					{@exposure_ids != ''} ? {AND @exposure_concept_id IN (@exposure_ids)}
	
			) T1
		{@first_occurrence_drug_only} ? {WHERE rn1 = 1}
	) D1
INNER JOIN
	@cdm_database_schema.person p1
ON 
	d1.@exposure_person_id = p1.person_id
INNER JOIN
	(
		SELECT 
			@outcome_person_id, 
			@outcome_concept_id, 
			@outcome_start_date, 
			@outcome_end_date
		FROM
			(
				SELECT 
					@outcome_person_id, 
					@outcome_concept_id, 
					@outcome_start_date, 
					@outcome_end_date
					{@first_occurrence_condition_only} ? {,ROW_NUMBER() OVER (PARTITION BY @outcome_person_id, @outcome_concept_id {@outcome_condition_type_concept_ids != '' & @outcome_table == 'condition_occurrence'} ? {, condition_type_concept_id} ORDER BY @outcome_start_date) AS rn1}
				FROM 
					@outcome_database_schema.@outcome_table 
				WHERE 
						1=1
					{@outcome_id != ''} ? {AND @outcome_concept_id IN (@outcome_id)}
					{@outcome_condition_type_concept_ids != '' & @outcome_table == 'condition_occurrence'} ? {AND condition_type_concept_id IN (@outcome_condition_type_concept_ids)}
			) T1
		{@first_occurrence_condition_only} ? {WHERE rn1 = 1}
	) c1
ON 
	p1.person_id = c1.@outcome_person_id
INNER JOIN
	@cdm_database_schema.observation_period op1
ON 
	d1.@exposure_person_id = op1.person_id,
	#age_group ag1
WHERE 
		d1.@exposure_start_date >= op1.observation_period_start_date
	AND 
		d1.@exposure_start_date <= op1.observation_period_end_date
	AND 
		YEAR(d1.@exposure_start_date) - year_of_birth >= ag1.age_group_min
	AND 
		YEAR(d1.@exposure_start_date) - year_of_birth <= ag1.age_group_max
	{@study_start_date != ''} ? {AND @exposure_start_date >= '@study_start_date'}
	{@study_end_date != ''} ? {AND @exposure_start_date <= '@study_end_date'}
	{@min_age != ''} ? {AND YEAR(d1.@exposure_start_date) - p1.year_of_birth >= @min_age}
	{@max_age != ''} ? {AND YEAR(d1.@exposure_start_date) - p1.year_of_birth <= @max_age}
	{@gender_concept_ids != ''} ? {AND p1.gender_concept_id IN (@gender_concept_ids)}
	{@has_full_time_at_risk} ? {
		AND 
			op1.observation_period_end_date >= DATEADD(
				dd, 
				{@use_length_of_exposure_exposed = 1} ? {DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)} : {0} + @surveillance_exposed,
				@exposure_start_date)		
		AND 
			op1.observation_period_start_date <= DATEADD(
				dd,
				{@use_length_of_exposure_exposed} ? {DATEDIFF(DAY,d1.@exposure_start_date,d1.@exposure_end_date)} : {0} + @surveillance_unexposed,
				@exposure_start_date)	
	}
	{@washout_window != 0} ? {AND DATEDIFF(DAY,op1.observation_period_start_date,d1.@exposure_start_date) >= @washout_window}
	{@followup_window != 0} ? {AND DATEDIFF(DAY,d1.@exposure_start_date, op1.observation_period_end_date) >= @followup_window}
GROUP BY 
	d1.@exposure_concept_id, 
	c1.@outcome_concept_id
	{@stratify_by_gender} ? {, gender_concept_id}
	{@stratify_by_age} ? {, ag1.age_group_name}
	{@stratify_by_year} ? {, YEAR(d1.@exposure_start_date)}
;

--now create final summary table
SELECT 
	exposure_concept_id AS exposure_id,
	outcome_concept_id AS outcome_id,
	gender_concept_id,
	age_group_name,
	index_year,
	num_persons,
	num_exposures,
	num_outcomes_exposed,
	num_outcomes_unexposed,
	time_at_risk_exposed,
	time_at_risk_unexposed,
	--error handling, cant have 0 time at risk, or else division is undefined, set IRR = 1
	CASE WHEN 
			t1.time_at_risk_exposed = 0 
		OR 
			t1.time_at_risk_unexposed = 0 
	THEN 
		1
	--if youve got time-at-risk but no unexposed events, you also have a problem
	--to correct for that, do shrinkage to numerator and denominator of rate
	ELSE 
		(@shrinkage + (t1.num_outcomes_exposed/t1.time_at_risk_exposed)) / (@shrinkage + (t1.num_outcomes_unexposed/ t1.time_at_risk_unexposed))
	END AS incidence_rate_ratio,
	
	--calculating IRRLB95  : LOG( IRR - 1.96*SElogRR)
	EXP(
		LOG( 
			CASE WHEN 
					t1.time_at_risk_exposed = 0 
				OR 
					t1.time_at_risk_unexposed = 0 
			THEN 
				1
			--if youve got time-at-risk but no unexposed events, you also have a problem
			--to correct for that, do shrinkage to numerator and denominator of rate
			ELSE 
				(@shrinkage + (t1.num_outcomes_exposed/t1.time_at_risk_exposed)) / (@shrinkage + (t1.num_outcomes_unexposed/ t1.time_at_risk_unexposed))
			END
		)
		- 1.96 * SQRT((1.0/ CASE WHEN t1.num_outcomes_exposed = 0 THEN 0.5 ELSE t1.num_outcomes_exposed END) + (1.0/ CASE WHEN t1.num_outcomes_unexposed = 0 THEN 0.5 ELSE t1.num_outcomes_unexposed END) ) 
	) AS irr_lb_95,

	--calculating IRRUB95 : LOG ( IRR + 1.96*SElogRR)
	EXP(
		LOG( 
			CASE WHEN 
					t1.time_at_risk_exposed = 0 
				OR 
					t1.time_at_risk_unexposed = 0 
			THEN 
				1
			--if youve got time-at-risk but no unexposed events, you also have a problem
			--to correct for that, do shrinkage to numerator and denominator of rate
			ELSE 
				(@shrinkage + (t1.num_outcomes_exposed/t1.time_at_risk_exposed)) / (@shrinkage + (t1.num_outcomes_unexposed/ t1.time_at_risk_unexposed))
			END
		)
		+ 1.96 * SQRT((1.0/ CASE WHEN t1.num_outcomes_exposed = 0 THEN 0.5 ELSE t1.num_outcomes_exposed END) + (1.0/ CASE WHEN t1.num_outcomes_unexposed = 0 THEN 0.5 ELSE t1.num_outcomes_unexposed END) ) 
	) AS irr_ub_95,	
	
	--calcuating SElogIRR
	SQRT( (1.0/ CASE WHEN t1.num_outcomes_exposed = 0 THEN 0.5 ELSE t1.num_outcomes_exposed END) + (1.0/ CASE WHEN t1.num_outcomes_unexposed = 0 THEN 0.5 ELSE t1.num_outcomes_unexposed END) ) AS se_log_rr
INTO 
	#results
FROM
	(
		SELECT 
			e2.exposure_concept_id,
			e2.outcome_concept_id,
			CAST(e2.gender_concept_id AS VARCHAR(255)) AS gender_concept_id,
			CAST(e2.age_group_name AS VARCHAR(255)) AS age_group_name,
			CAST(e2.index_year AS VARCHAR(255)) AS index_year,
			e2.num_persons,
			e2.num_exposures,
			CASE WHEN o2.num_outcomes_exposed IS NULL THEN 0 ELSE o2.num_outcomes_exposed END AS num_outcomes_exposed,
			CASE WHEN o2.num_outcomes_unexposed IS NULL THEN 0 ELSE o2.num_outcomes_unexposed END AS num_outcomes_unexposed,
			e2.time_at_risk_exposed,
			e2.time_at_risk_unexposed
		FROM 
			(
				SELECT 
					e1.*, 
					o1.outcome_concept_id
				FROM 
					#scc_exposure_summary e1,
					(
						SELECT DISTINCT 
							outcome_concept_id 
						FROM 
							#scc_outcome_summary
					) o1
			) e2
		LEFT JOIN 
			#scc_outcome_summary o2
		ON 
				e2.exposure_concept_id = o2.exposure_concept_id
			AND 
				e2.outcome_concept_id = o2.outcome_concept_id
			AND 
				e2.gender_concept_id = o2.gender_concept_id
			AND 
				e2.age_group_name = o2.age_group_name
			AND 
				e2.index_year = o2.index_year

		{@stratify_by_gender} ? {
		UNION
			SELECT 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				'ALL' AS gender_concept_id,
				CAST(e2.age_group_name AS VARCHAR(255)) AS age_group_name,
				CAST(e2.index_year AS VARCHAR(255)) AS index_year,
				SUM(e2.num_persons) AS num_persons,
				SUM(e2.num_exposures) AS num_exposures,
				SUM(CASE WHEN O2.num_outcomes_exposed IS NULL THEN 0 ELSE o2.num_outcomes_exposed END) AS num_outcomes_exposed,
				SUM(CASE WHEN o2.num_outcomes_unexposed IS NULL THEN 0 ELSE o2.num_outcomes_unexposed END) AS num_outcomes_unexposed,
				SUM(e2.time_at_risk_exposed) AS time_at_risk_exposed,
				SUM(e2.time_at_risk_unexposed) AS time_at_risk_unexposed
			FROM 
				(
					SELECT 
						e1.*, 
						o1.outcome_concept_id
					FROM 
						#scc_exposure_summary e1,
						(
							SELECT DISTINCT 
								outcome_concept_id 
							FROM 
								#scc_outcome_summary
						) o1
				) e2
			LEFT JOIN 
				#scc_outcome_summary o2
			ON 
					e2.exposure_concept_id = o2.exposure_concept_id
				AND 
					e2.outcome_concept_id = o2.outcome_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				e2.age_group_name,
				e2.index_year
		}
		{@stratify_by_age} ? {
		UNION
			SELECT e2.exposure_concept_id,
				e2.outcome_concept_id,
				CAST(e2.gender_concept_id AS VARCHAR(255)) AS gender_concept_id,
				'ALL' AS age_group_name,
				CAST(e2.index_year AS VARCHAR(255)) AS index_year,
				SUM(e2.num_persons) AS num_persons,
				SUM(e2.num_exposures) AS num_exposures,
				SUM(CASE WHEN o2.num_outcomes_exposed IS NULL THEN 0 ELSE o2.num_outcomes_exposed END) AS num_outcomes_exposed,
				SUM(CASE WHEN o2.num_outcomes_unexposed IS NULL THEN 0 ELSE o2.num_outcomes_unexposed END) AS num_outcomes_unexposed,
				SUM(e2.time_at_risk_exposed) AS time_at_risk_exposed,
				SUM(e2.time_at_risk_unexposed) AS time_at_risk_unexposed
			FROM 
				(
					SELECT 
						e1.*, 
						o1.outcome_concept_id
					FROM 
						#scc_exposure_summary e1,
						(
							SELECT DISTINCT 
								outcome_concept_id	
							FROM 
								#scc_outcome_summary
						) o1
				) e2
			LEFT JOIN 
					#scc_outcome_summary o2
			ON 
					e2.exposure_concept_id = o2.exposure_concept_id
				AND 
					e2.outcome_concept_id = o2.outcome_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				e2.gender_concept_id,
				e2.index_year
		}
		{@stratify_by_year} ? {
		UNION
			SELECT 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				CAST(e2.gender_concept_id AS VARCHAR(255)) AS gender_concept_id,
				CAST(e2.age_group_name AS VARCHAR(255)) AS age_group_name,
				'ALL' AS index_year,
				SUM(e2.num_persons) AS num_persons,
				SUM(e2.num_exposures) AS num_exposures,
				SUM(CASE WHEN o2.num_outcomes_exposed IS NULL THEN 0 ELSE o2.num_outcomes_exposed END) AS num_outcomes_exposed,
				SUM(CASE WHEN o2.num_outcomes_unexposed IS NULL THEN 0 ELSE o2.num_outcomes_unexposed END) AS num_outcomes_unexposed,
				SUM(e2.time_at_risk_exposed) AS time_at_risk_exposed,
				SUM(e2.time_at_risk_unexposed) AS time_at_risk_unexposed
			FROM 
				(
					SELECT 
						e1.*, 
						o1.outcome_concept_id
					FROM 
						#scc_exposure_summary e1,
						(
							SELECT DISTINCT 
								outcome_concept_id 
							FROM 
								#scc_outcome_summary) o1
						) e2
					LEFT JOIN 
						#scc_outcome_summary o2
					ON 
							e2.exposure_concept_id = o2.exposure_concept_id
						AND 
							e2.outcome_concept_id = o2.outcome_concept_id
						AND 
							e2.gender_concept_id = o2.gender_concept_id
						AND 
							e2.age_group_name = o2.age_group_name
						AND 
							e2.index_year = o2.index_year
					GROUP BY 
						e2.exposure_concept_id,
						e2.outcome_concept_id,
						e2.gender_concept_id,
						e2.age_group_name
		}
		{@stratify_by_gender & @stratify_by_age} ? {
		UNION
			SELECT e2.exposure_concept_id,
				e2.outcome_concept_id,
				'ALL' AS gender_concept_id,
				'ALL' AS age_group_name,
				CAST(e2.index_year AS VARCHAR(255)) AS index_year,
				SUM(e2.num_persons) AS num_persons,
				SUM(e2.num_exposures) AS num_exposures,
				SUM(CASE WHEN o2.num_outcomes_exposed IS NULL THEN 0 ELSE o2.num_outcomes_exposed END) AS num_outcomes_exposed,
				SUM(CASE WHEN o2.num_outcomes_unexposed IS NULL THEN 0 ELSE o2.num_outcomes_unexposed END) AS num_outcomes_unexposed,
				SUM(e2.time_at_risk_exposed) AS time_at_risk_exposed,
				SUM(e2.time_at_risk_unexposed) AS time_at_risk_unexposed
			FROM 
				(
					SELECT 
						e1.*, 
						o1.outcome_concept_id
					FROM 
						#scc_exposure_summary e1,
						(
							SELECT DISTINCT 
								outcome_concept_id 
							FROM 
								#scc_outcome_summary
						) o1
				) e2
			LEFT JOIN 
				#scc_outcome_summary o2
			ON 
					e2.exposure_concept_id = o2.exposure_concept_id
				AND 
					e2.outcome_concept_id = o2.outcome_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				e2.index_year
		}
		{@stratify_by_gender & @stratify_by_year} ? {
		UNION
			SELECT 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				'ALL' AS gender_concept_id,
				CAST(e2.age_group_name AS VARCHAR(255)) AS age_group_name,
				'all' AS index_year,
				SUM(e2.num_persons) AS num_persons,
				SUM(e2.num_exposures) AS num_exposures,
				SUM(CASE WHEN o2.num_outcomes_exposed IS NULL THEN 0 ELSE o2.num_outcomes_exposed END) AS num_outcomes_exposed,
				SUM(CASE WHEN o2.num_outcomes_unexposed IS NULL THEN 0 ELSE o2.num_outcomes_unexposed END) AS num_outcomes_unexposed,
				SUM(e2.time_at_risk_exposed) AS time_at_risk_exposed,
				SUM(e2.time_at_risk_unexposed) AS time_at_risk_unexposed
			FROM 
				(
					SELECT 
						e1.*, 
						o1.outcome_concept_id
					 FROM 
						#scc_exposure_summary e1,
						(
							SELECT DISTINCT 
								outcome_concept_id 
							FROM 
								#scc_outcome_summary
						) o1
				) e2
			LEFT JOIN 
				#scc_outcome_summary o2
			ON 
					e2.exposure_concept_id = o2.exposure_concept_id
				AND 
					e2.outcome_concept_id = o2.outcome_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				e2.age_group_name
		}
		{@stratify_by_age & @stratify_by_year} ? {
		UNION
			SELECT 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				CAST(e2.gender_concept_id AS VARCHAR(255)) AS gender_concept_id,
				'ALL' AS age_group_name,
				'ALL' AS index_year,
				SUM(e2.num_persons) AS num_persons,
				SUM(e2.num_exposures) AS num_exposures,
				SUM(CASE WHEN o2.num_outcomes_exposed IS NULL THEN 0 ELSE o2.num_outcomes_exposed END) AS num_outcomes_exposed,
				SUM(CASE WHEN o2.num_outcomes_unexposed IS NULL THEN 0 ELSE o2.num_outcomes_unexposed END) AS num_outcomes_unexposed,
				SUM(e2.time_at_risk_exposed) AS time_at_risk_exposed,
				SUM(e2.time_at_risk_unexposed) AS time_at_risk_unexposed
			FROM 
				(
					SELECT 
						e1.*, o1.outcome_concept_id
					FROM 
						#scc_exposure_summary e1,
						(
							SELECT DISTINCT 
								outcome_concept_id
							FROM
								#scc_outcome_summary
						) o1
				) e2
			LEFT JOIN 
				#scc_outcome_summary o2
			ON 
					e2.exposure_concept_id = o2.exposure_concept_id
				AND 
					e2.outcome_concept_id = o2.outcome_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				e2.gender_concept_id
		}
		{@stratify_by_gender & @stratify_by_age & @stratify_by_year} ? {
		UNION
			SELECT 
				e2.exposure_concept_id,
				e2.outcome_concept_id,
				'ALL' AS gender_concept_id,
				'ALL' AS age_group_name,
				'ALL' AS index_year,
				SUM(e2.num_persons) AS num_persons,
				SUM(e2.num_exposures) AS num_exposures,
				SUM(CASE WHEN o2.num_outcomes_exposed IS NULL THEN 0 ELSE o2.num_outcomes_exposed END) AS num_outcomes_exposed,
				SUM(CASE WHEN o2.num_outcomes_unexposed IS NULL THEN 0 ELSE o2.num_outcomes_unexposed END) AS num_outcomes_unexposed,
				SUM(e2.time_at_risk_exposed) AS time_at_risk_exposed,
				SUM(e2.time_at_risk_unexposed) AS time_at_risk_unexposed
			FROM 
				(
					SELECT 
						e1.*, 
						o1.outcome_concept_id
					FROM 
						#scc_exposure_summary e1,
						(
							SELECT DISTINCT 
								outcome_concept_id 
							FROM
								#scc_outcome_summary) o1
				) e2
			LEFT JOIN 
				#scc_outcome_summary o2
			ON 
					e2.exposure_concept_id = o2.exposure_concept_id
				AND 
					e2.outcome_concept_id = o2.outcome_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.exposure_concept_id,
				e2.outcome_concept_id
		}

	) T1
;

TRUNCATE TABLE #age_group;
TRUNCATE TABLE #scc_exposure_summary;
TRUNCATE TABLE #scc_outcome_summary;

DROP TABLE #age_group;
DROP TABLE #scc_exposure_summary;
DROP TABLE #scc_outcome_summary;
