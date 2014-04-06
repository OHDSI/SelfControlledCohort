/*****************

Self-controlled cohort design
Parameterized TSQL for Jackalope

Patrick Ryan
15 July 2013


current assumptions:
connected to valid server with schema that contains CDMv4 instance
all tables in CDMv4 exist

if tables for DRUG_ERA, CONDITION_ERA, PERSON, and/or OBSERVATION are provides as temp tables,
	full paths is needed and all variables need to be the same

******************/

{DEFAULT @table_drug_era = 'drug_era'} --name of table where contains in format of DRUG_ERA live (could be temp table if pre-processing was necessary)
{DEFAULT @table_condition_era = 'condition_era'} --name of table where contains in format of CONDITION_ERA live (could be temp table if pre-processing was necessary)
{DEFAULT @table_person = 'person'} --name of table where contains in format of PERSON live (could be temp table if pre-processing was necessary)
{DEFAULT @table_observation_period = 'observation_period'} --name of table where contains in format of OBSERVATION_PERIOD live (could be temp table if pre-processing was necessary)
{DEFAULT @first_occurrence_drug_only = '1'} --if 1, only use first occurrence of each drug concept id for each person in DRUG_ERA table
{DEFAULT @first_occurrence_condition_only = '1'} --if 1, only use first occurrence of each condition concept id for each person in CONDITION_ERA table
{DEFAULT @drug_type_concept_id_list = '38000182'} --which DRUG_TYPE to use:  generally only use 1 value (ex:  30d era)
{DEFAULT @condition_type_concept_id_list = '38000247'} --which CONDITION_TYPE to use:  generally only use 1 value (ex:  30d era)
{DEFAULT @drug_concept_id_list = '1125315, 1713332, 19011773, 1174888'} --list of DRUG_CONCEPT_IDs to study, if NULL, then all DRUG_CONCEPT_IDs will be used
{DEFAULT @condition_concept_id_list = '444382, 79106, 138825, 77670'} --list of CONDITION_CONCET_IDs to study, if NULL, all CONDITIONS considered as potential outcomes
{DEFAULT @gender_concept_id_list = '8507,8532'} --list of GENDER_CONCEPT_IDs, generally use MALE (8507) and FEMALE (8532)
{DEFAULT @min_age = '0'} --integer for minimum allowable age
{DEFAULT @max_age = '100'} --integer for maximum allowable age
{DEFAULT @min_index = '1/1/2000'} --date for minimum allowable data for index exposure
{DEFAULT @max_index = '1/1/2013'} --date for maximum allowable data for index exposure
{DEFAULT @stratify_gender = '1'} --if 1, analysis will be calculated overall, and stratified across all gender groups
{DEFAULT @stratify_age = '1'} --if 1, analysis will be calculated overall, and stratified across all age groups  (using AGE_GROUP table below)
{DEFAULT @stratify_index = '1'} --if 1, analysis will be calculated overall, and stratified across all years of the index dates
{DEFAULT @use_length_of_exposure_exposed = '1'} --if 1, use the duration from drug_era_start -> drug_era_end as part of time_at_risk
{DEFAULT @time_at_risk_exposed_start = '1'} --integer of days to add to drug_era_start for start of time_at_risk (0 to include index date, 1 to start the day after)
{DEFAULT @surveillance_exposed = '30'} --additional window to add to end of exposure period (if @USE_LENGTH_OF_EXPOSURE_EXPOSED = 1, then add to DRUG_ERA_END, else add to DRUG_ERA_START)
{DEFAULT @use_length_of_exposure_unexposed = '1'} --if 1, use the duration from drug_era_start -> drug_era_end as part of time_at_risk looking back before drug_era_start
{DEFAULT @time_at_risk_unexposed_start = '-1'} --integer of days to add to drug_era_start for start of time_at_risk (0 to include index date, -1 to start the day before)
{DEFAULT @surveillance_unexposed = '-30'} --additional window to add to end of exposure period (if @USE_LENGTH_OF_EXPOSURE_UNEXPOSED = 1, then add to DRUG_ERA_END, else add to DRUG_ERA_START)
{DEFAULT @has_full_time_at_risk = '0'} --if 1, restrict to people who have full time-at-risk exposed and unexposed
{DEFAULT @washout_period_length = '0'} --integer to define required time observed before exposure start
{DEFAULT @followup_period_length = '0'} --integer to define required time observed after exposure start
{DEFAULT @shrinkage = '0.0001'} --shrinkage used in IRR calculations, required >0 to deal with 0 case counts, but larger number means more shrinkage

--Delete these:
drop table #age_group;
drop table #self_controlled_cohort_exposure_summary;
drop table #self_controlled_cohort_outcome_summary;

IF OBJECT_ID('scratch.dbo.self_controlled_cohort_strata_summary', 'U') IS NOT NULL 
	DROP TABLE scratch.dbo.self_controlled_cohort_strata_summary;

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
	d1.drug_concept_id,
	{@stratify_gender} ? {gender_concept_id,} : {'ALL' AS gender_concept_id,}
	{@stratify_age} ? {ag1.age_group_name,} : {'ALL' AS age_group_name,}
	{@stratify_index} ? {YEAR(d1.drug_era_start_date) AS index_year,} : {'ALL' AS index_year,}
	COUNT(DISTINCT d1.person_id) AS num_persons,
	COUNT(d1.person_id) AS num_exposures,
	SUM(
		--need to account for potential censoring due to observation period length	
		CASE WHEN 
			DATEADD(
				dd,
				{@use_length_of_exposure_exposed} ? {DATEDIFF(dd,d1.drug_era_start_date,d1.drug_era_end_date)} : {0} + @surveillance_exposed,
				drug_era_start_date
			) <= op1.observation_period_end_date
		THEN 
			DATEDIFF(
				dd,
				DATEADD(dd, @time_at_risk_exposed_start, drug_era_start_date),	
				DATEADD(dd, {@use_length_of_exposure_exposed} ? {DATEDIFF(dd,d1.drug_era_start_date,d1.drug_era_end_date)} : {0} + @surveillance_exposed,
				drug_era_start_date)
			)
		ELSE
			DATEDIFF(
				dd, 
				DATEADD(dd, @time_at_risk_exposed_start, drug_era_start_date), 
				op1.observation_period_end_date
			)
		END
	) / 365.25 AS time_at_risk_exposed,		
	SUM(
		CASE WHEN 
			DATEADD(
				dd,
				{@use_length_of_exposure_unexposed} ? {-1*DATEDIFF(dd,d1.drug_era_start_date,d1.drug_era_end_date)} : {0} + @surveillance_unexposed,
				drug_era_start_date
			) >= op1.observation_period_start_date
		THEN 		
			DATEDIFF(
				dd,
				DATEADD(dd, {@use_length_of_exposure_unexposed} ? {-1*DATEDIFF(dd, d1.drug_era_start_date, d1.drug_era_end_date)} : {0} + @surveillance_unexposed, drug_era_start_date), 
				DATEADD(dd, @time_at_risk_unexposed_start, drug_era_start_date)
			)
		ELSE  
			DATEDIFF(
				dd, 
				op1.observation_period_start_date, 
				DATEADD(dd, @time_at_risk_unexposed_start, drug_era_start_date)
			)
		END
	) / 365.25 AS time_at_risk_unexposed
INTO 
	#self_controlled_cohort_exposure_summary
FROM
	(
		SELECT 
			person_id, 
			drug_concept_id, 
			drug_era_start_date, 
			drug_era_end_date
		FROM
			(
				SELECT 
					person_id, 
					drug_concept_id, 
					drug_era_start_date, 
					drug_era_end_date
					{@first_occurrence_drug_only} ? {,ROW_NUMBER() OVER (PARTITION BY person_id, drug_concept_id, drug_type_concept_id ORDER BY drug_era_start_date) AS rn1} 
				FROM 
					@table_drug_era 
				WHERE 
					1=1
					{@drug_concept_id_list != ''} ? {AND drug_concept_id IN (@drug_concept_id_list)}
					{@drug_type_concept_id_list != ''} ? {AND drug_type_concept_id IN (@drug_type_concept_id_list)}
			) t1
		{@first_occurrence_drug_only} ? {WHERE rn1 = 1}
	) d1
	INNER JOIN
		@table_person p1
	ON 
		d1.person_id = p1.person_id
	INNER JOIN
		@table_observation_period op1
	ON 
		d1.person_id = op1.person_id,
	#age_group ag1
WHERE 
		d1.drug_era_start_date >= op1.observation_period_start_date
	AND 
		d1.drug_era_start_date <= op1.observation_period_end_date
	AND 
		YEAR(d1.drug_era_start_date) - year_of_birth >= ag1.age_group_min
	AND 
		YEAR(d1.drug_era_start_date) - year_of_birth <= ag1.age_group_max
	{@min_index != ''} ? {AND drug_era_start_date >= '@min_index'}
	{@max_index != ''} ? {AND drug_era_start_date <= '@max_index'}
	{@min_age != ''} ? {AND YEAR(d1.drug_era_start_date) - p1.year_of_birth  >= @min_age}
	{@max_age != ''} ? {AND YEAR(d1.drug_era_start_date) - p1.year_of_birth  <= @max_age}
	{@gender_concept_id_list != ''} ? {AND p1.gender_concept_id IN (@gender_concept_id_list)}
	{@has_full_time_at_risk}  ? {
		AND 
			op1.observation_period_end_date >= DATEADD(dd, {@use_length_of_exposure_exposed} ? {DATEDIFF(dd,d1.drug_era_start_date,d1.drug_era_end_date)} : {0} + @surveillance_exposed, drug_era_start_date)
		AND 
			op1.observation_period_start_date <= DATEADD(dd, {@use_length_of_exposure_exposed} ? {DATEDIFF(dd,d1.drug_era_start_date,d1.drug_era_end_date)} : {0} + @surveillance_unexposed, drug_era_start_date)	
	}
	{@washout_period_length != ''} ? {AND DATEDIFF(dd, op1.observation_period_start_date, d1.drug_era_start_date) >= @washout_period_length}
	{@followup_period_length != ''} ? {AND DATEDIFF(dd,d1.drug_era_start_date, op1.observation_period_end_date) >= @followup_period_length}
GROUP BY 
	drug_concept_id
	{@stratify_gender} ? {, gender_concept_id}
	{@stratify_age} ? {, ag1.age_group_name}
	{@stratify_index} ? {, YEAR(d1.drug_era_start_date)}
;


--query 2:  number of events in each time window
SELECT 
	d1.drug_concept_id,
	c1.condition_concept_id,
	{@stratify_gender} ? {gender_concept_id,} : {'ALL' AS gender_concept_id,}
	{@stratify_age} ? {ag1.age_group_name,} : {'ALL' AS age_group_name,}
	{@stratify_index} ? {YEAR(d1.drug_era_start_date) AS index_year,} : {'ALL' AS index_year,}
	SUM(
		CASE WHEN 
				c1.condition_era_start_date >= DATEADD(dd,@time_at_risk_exposed_start,drug_era_start_date)
			AND 
				c1.condition_era_start_date <= DATEADD(dd, {@use_length_of_exposure_exposed}?{DATEDIFF(dd,d1.drug_era_start_date,d1.drug_era_end_date)}:{0} + @surveillance_exposed ,drug_era_start_date)
			AND 
				c1.condition_era_start_date <= op1.observation_period_end_date
		THEN 
			1 
		ELSE 
			0 
		END
	) AS num_outcomes_exposed,
	SUM(
		CASE WHEN 
			c1.condition_era_start_date <= DATEADD(dd,@time_at_risk_unexposed_start, drug_era_start_date)
		AND 
			c1.condition_era_start_date >= DATEADD(dd,{@use_length_of_exposure_unexposed} ? {-1*DATEDIFF(dd,d1.drug_era_start_date,d1.drug_era_end_date)} : {0} + @surveillance_unexposed ,drug_era_start_date)	
		AND 
			c1.condition_era_start_date >= op1.observation_period_start_date
		THEN 
			1 
		ELSE 
			0 
		END
	) AS num_outcomes_unexposed
	--no additional time censored to be calculated, because doing so would violate assumption of Poisson rates and cause problems with patients with events in UNEXPOSED-time not counting EXPOSED-exposure time
INTO 
	#self_controlled_cohort_outcome_summary
FROM
	(
		SELECT 
			person_id, 
			drug_concept_id, 
			drug_era_start_date, 
			drug_era_end_date
		FROM
			(
				SELECT 
					person_id, 
					drug_concept_id, 
					drug_era_start_date, 
					drug_era_end_date
					{@first_occurrence_drug_only}?{,ROW_NUMBER() OVER (PARTITION BY person_id, drug_concept_id, drug_type_concept_id ORDER BY drug_era_start_date) AS rn1}
				FROM 
					@table_drug_era 
				WHERE 
						1=1
					{@drug_concept_id_list != ''} ? {AND drug_concept_id IN (@drug_concept_id_list)}
					{@drug_type_concept_id_list != ''} ? {AND drug_type_concept_id IN (@drug_type_concept_id_list) }
	
			) T1
		{@first_occurrence_drug_only} ? {WHERE rn1 = 1}
	) D1
INNER JOIN
	@table_person p1
ON 
	d1.person_id = p1.person_id
INNER JOIN
	(
		SELECT 
			person_id, 
			condition_concept_id, 
			condition_era_start_date, 
			condition_era_end_date
		FROM
			(
				SELECT 
					person_id, 
					condition_concept_id, 
					condition_era_start_date, 
					condition_era_end_date
					{@first_occurrence_condition_only} ? {,ROW_NUMBER() OVER (PARTITION BY person_id, condition_concept_id, condition_type_concept_id ORDER BY condition_era_start_date) AS rn1}
				FROM 
					@table_condition_era 
				WHERE 
						1=1
					{@condition_concept_id_list != ''} ? {AND condition_concept_id IN (@condition_concept_id_list)}
					{@condition_type_concept_id_list} ? {AND condition_type_concept_id IN (@condition_type_concept_id_list)}
			) T1
		{@first_occurrence_drug_only} ? {WHERE rn1 = 1}
	) C1
ON 
	p1.person_id = c1.person_id
INNER JOIN
	@table_observation_period op1
ON 
	d1.person_id = op1.person_id,
	#age_group ag1
WHERE 
		d1.drug_era_start_date >= op1.observation_period_start_date
	AND 
		d1.drug_era_start_date <= op1.observation_period_end_date
	AND 
		YEAR(d1.drug_era_start_date) - year_of_birth >= ag1.age_group_min
	AND 
		YEAR(d1.drug_era_start_date) - year_of_birth <= ag1.age_group_max
	{@min_index != ''} ? {AND drug_era_start_date >= '@min_index'}
	{@max_index != ''} ? {AND drug_era_start_date <= '@max_index'}
	{@min_age != ''} ? {AND YEAR(d1.drug_era_start_date) - p1.year_of_birth >= @min_age}
	{@max_age != ''} ? {AND YEAR(d1.drug_era_start_date) - p1.year_of_birth <= @max_age}
	{@gender_concept_id_list != ''} ? {AND p1.gender_concept_id IN (@gender_concept_id_list)}
	{@has_full_time_at_risk} ? {
		AND 
			op1.observation_period_end_date >= DATEADD(
				dd, 
				{@use_length_of_exposure_exposed = 1} ? {DATEDIFF(dd,d1.drug_era_start_date,d1.drug_era_end_date)} : {0} + @surveillance_exposed,
				drug_era_start_date)		
		AND 
			op1.observation_period_start_date <= DATEADD(
				dd,
				{@use_length_of_exposure_exposed} ? {DATEDIFF(DD,d1.drug_era_start_date,d1.drug_era_end_date)} : {0} + @surveillance_unexposed,
				drug_era_start_date)	
	}
	{@washout_period_length != 0} ? {AND DATEDIFF(DD,op1.observation_period_start_date,d1.drug_era_start_date) >= @washout_period_length}
	{@followup_period_length != 0} ? {AND DATEDIFF(DD,d1.drug_era_start_date, op1.observation_period_end_date) >= @followup_period_length}
GROUP BY 
	drug_concept_id, 
	condition_concept_id
	{@stratify_gender} ? {, gender_concept_id}
	{@stratify_age} ? {, ag1.age_group_name}
	{@stratify_index} ? {, YEAR(d1.drug_era_start_date)}
;

--now create final summary table
SELECT 
	drug_concept_id,
	condition_concept_id,
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
	END AS irr,
	
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
	) AS irrlb95,

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
	) AS irrub95,	
	
	--calcuating SElogIRR
	SQRT( (1.0/ CASE WHEN t1.num_outcomes_exposed = 0 THEN 0.5 ELSE t1.num_outcomes_exposed END) + (1.0/ CASE WHEN t1.num_outcomes_unexposed = 0 THEN 0.5 ELSE t1.num_outcomes_unexposed END) ) AS selogirr
	
INTO 
	scratch.dbo.self_controlled_cohort_strata_summary
FROM
	(
		SELECT 
			e2.drug_concept_id,
			e2.condition_concept_id,
			CAST(e2.gender_concept_id AS VARCHAR) AS gender_concept_id,
			CAST(e2.age_group_name AS VARCHAR) AS age_group_name,
			CAST(e2.index_year AS VARCHAR) AS index_year,
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
					o1.condition_concept_id
				FROM 
					#self_controlled_cohort_exposure_summary e1,
					(
						SELECT DISTINCT 
							condition_concept_id 
						FROM 
							#self_controlled_cohort_outcome_summary
					) o1
			) e2
		LEFT JOIN 
			#self_controlled_cohort_outcome_summary o2
		ON 
				e2.drug_concept_id = o2.drug_concept_id
			AND 
				e2.condition_concept_id = o2.condition_concept_id
			AND 
				e2.gender_concept_id = o2.gender_concept_id
			AND 
				e2.age_group_name = o2.age_group_name
			AND 
				e2.index_year = o2.index_year

		{@stratify_gender} ? {
		UNION
			SELECT 
				e2.drug_concept_id,
				e2.condition_concept_id,
				'ALL' AS gender_concept_id,
				CAST(e2.age_group_name AS VARCHAR) AS age_group_name,
				CAST(e2.index_year AS VARCHAR) AS index_year,
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
						o1.condition_concept_id
					FROM 
						#self_controlled_cohort_exposure_summary e1,
						(
							SELECT DISTINCT 
								condition_concept_id 
							FROM 
								#self_controlled_cohort_outcome_summary
						) o1
				) e2
			LEFT JOIN 
				#self_controlled_cohort_outcome_summary o2
			ON 
					e2.drug_concept_id = o2.drug_concept_id
				AND 
					e2.condition_concept_id = o2.condition_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.drug_concept_id,
				e2.condition_concept_id,
				e2.age_group_name,
				e2.index_year
		}
		{@stratify_age} ? {
		UNION
			SELECT e2.drug_concept_id,
				e2.condition_concept_id,
				CAST(e2.gender_concept_id AS VARCHAR) AS gender_concept_id,
				'ALL' AS age_group_name,
				CAST(e2.index_year AS VARCHAR) AS index_year,
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
						o1.condition_concept_id
					FROM 
						#self_controlled_cohort_exposure_summary e1,
						(
							SELECT DISTINCT 
								condition_concept_id	
							FROM 
								#self_controlled_cohort_outcome_summary
						) o1
				) e2
			LEFT JOIN 
					#self_controlled_cohort_outcome_summary o2
			ON 
					e2.drug_concept_id = o2.drug_concept_id
				AND 
					e2.condition_concept_id = o2.condition_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.drug_concept_id,
				e2.condition_concept_id,
				e2.gender_concept_id,
				e2.index_year
		}
		{@stratify_index} ? {
		UNION
			SELECT 
				e2.drug_concept_id,
				e2.condition_concept_id,
				CAST(e2.gender_concept_id AS VARCHAR) AS gender_concept_id,
				CAST(e2.age_group_name AS VARCHAR) AS age_group_name,
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
						o1.condition_concept_id
					FROM 
						#self_controlled_cohort_exposure_summary e1,
						(
							SELECT DISTINCT 
								condition_concept_id 
							FROM 
								#self_controlled_cohort_outcome_summary) o1
						) e2
					LEFT JOIN 
						#self_controlled_cohort_outcome_summary o2
					ON 
							e2.drug_concept_id = o2.drug_concept_id
						AND 
							e2.condition_concept_id = o2.condition_concept_id
						AND 
							e2.gender_concept_id = o2.gender_concept_id
						AND 
							e2.age_group_name = o2.age_group_name
						AND 
							e2.index_year = o2.index_year
					GROUP BY 
						e2.drug_concept_id,
						e2.condition_concept_id,
						e2.gender_concept_id,
						e2.age_group_name
		}
		{@stratify_gender & @stratify_age} ? {
		UNION
			SELECT e2.drug_concept_id,
				e2.condition_concept_id,
				'ALL' AS gender_concept_id,
				'ALL' AS age_group_name,
				CAST(e2.index_year AS VARCHAR) AS index_year,
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
						o1.condition_concept_id
					FROM 
						#self_controlled_cohort_exposure_summary e1,
						(
							SELECT DISTINCT 
								condition_concept_id 
							FROM 
								#self_controlled_cohort_outcome_summary
						) o1
				) e2
			LEFT JOIN 
				#self_controlled_cohort_outcome_summary o2
			ON 
					e2.drug_concept_id = o2.drug_concept_id
				AND 
					e2.condition_concept_id = o2.condition_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.drug_concept_id,
				e2.condition_concept_id,
				e2.index_year
		}
		{@stratify_gender & @stratify_index} ? {
		UNION
			SELECT 
				e2.drug_concept_id,
				e2.condition_concept_id,
				'ALL' AS gender_concept_id,
				CAST(e2.age_group_name AS VARCHAR) AS age_group_name,
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
						o1.condition_concept_id
					 FROM 
						#self_controlled_cohort_exposure_summary e1,
						(
							SELECT DISTINCT 
								condition_concept_id 
							FROM 
								#self_controlled_cohort_outcome_summary
						) o1
				) e2
			LEFT JOIN 
				#self_controlled_cohort_outcome_summary o2
			ON 
					e2.drug_concept_id = o2.drug_concept_id
				AND 
					e2.condition_concept_id = o2.condition_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.drug_concept_id,
				e2.condition_concept_id,
				e2.age_group_name
		}
		{@stratify_age & @stratify_index} ? {
		UNION
			SELECT 
				E2.DRUG_CONCEPT_ID,
				e2.condition_concept_id,
				CAST(e2.gender_concept_id AS VARCHAR) AS gender_concept_id,
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
						e1.*, o1.condition_concept_id
					FROM 
						#self_controlled_cohort_exposure_summary e1,
						(
							SELECT DISTINCT 
								condition_concept_id
							FROM
								#self_controlled_cohort_outcome_summary
						) o1
				) e2
			LEFT JOIN 
				#self_controlled_cohort_outcome_summary o2
			ON 
					e2.drug_concept_id = o2.drug_concept_id
				AND 
					e2.condition_concept_id = o2.condition_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.drug_concept_id,
				e2.condition_concept_id,
				e2.gender_concept_id
		}
		{@stratify_gender & @stratify_age & @stratify_index} ? {
		UNION
			SELECT 
				e2.drug_concept_id,
				e2.condition_concept_id,
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
						o1.condition_concept_id
					FROM 
						#self_controlled_cohort_exposure_summary e1,
						(
							SELECT DISTINCT 
								condition_concept_id 
							FROM
								#self_controlled_cohort_outcome_summary) o1
				) e2
			LEFT JOIN 
				#self_controlled_cohort_outcome_summary o2
			ON 
					e2.drug_concept_id = o2.drug_concept_id
				AND 
					e2.condition_concept_id = o2.condition_concept_id
				AND 
					e2.gender_concept_id = o2.gender_concept_id
				AND 
					e2.age_group_name = o2.age_group_name
				AND 
					e2.index_year = o2.index_year
			GROUP BY 
				e2.drug_concept_id,
				e2.condition_concept_id
		}

	) T1
;



