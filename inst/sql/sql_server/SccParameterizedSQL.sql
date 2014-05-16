/*****************
Self-controlled cohort design

Patrick Ryan
Martijn Schuemie
******************/
{DEFAULT @analysisId = 1}
{DEFAULT @sourceName = ""}
{DEFAULT @exposureTable = 'drug_era'} --name of table where contains in format of DRUG_ERA live (could be temp table if pre-processing was necessary)
{DEFAULT @exposureStartDate = 'drug_era_start_date'} 
{DEFAULT @exposureEndDate = 'drug_era_end_date'} 
{DEFAULT @exposureConceptId = 'drug_concept_id'} 
{DEFAULT @exposurePersonId = 'person_id'} 
{DEFAULT @outcomeTable = 'condition_era'} --name of table where contains in format of CONDITION_ERA live (could be temp table if pre-processing was necessary)
{DEFAULT @outcomeStartDate = 'condition_era_start_date'} 
{DEFAULT @outcomeEndDate = 'condition_era_end_date'} 
{DEFAULT @outcomeConceptId = 'condition_concept_id'} 
{DEFAULT @outcomePersontId = 'person_id'} 
{DEFAULT @personTable = 'person'} --name of table where contains in format of PERSON live (could be temp table if pre-processing was necessary)
{DEFAULT @observationPeriodTable = 'observation_period'} --name of table where contains in format of OBSERVATION_PERIOD live (could be temp table if pre-processing was necessary)
{DEFAULT @firstOccurrenceDrugOnly = '1'} --if 1, only use first occurrence of each drug concept id for each person in DRUG_ERA table
{DEFAULT @firstOccurrenceConditionOnly = '1'} --if 1, only use first occurrence of each condition concept id for each person in CONDITION_ERA table
{DEFAULT @drugTypeConceptIdList = '38000182'} --which DRUG_TYPE to use:  generally only use 1 value (ex:  30d era)
{DEFAULT @conditionTypeConceptIdList = '38000247'} --which CONDITION_TYPE to use:  generally only use 1 value (ex:  30d era)
{DEFAULT @exposuresOfInterest = '1125315, 1713332, 19011773, 1174888'} --list of DRUG_CONCEPT_IDs to study, if NULL, then all DRUG_CONCEPT_IDs will be used
{DEFAULT @outcomesOfInterest = '444382, 79106, 138825, 77670'} --list of CONDITION_CONCET_IDs to study, if NULL, all CONDITIONS considered as potential outcomes
{DEFAULT @genderConceptIdList = '8507,8532'} --list of GENDER_CONCEPT_IDs, generally use MALE (8507) and FEMALE (8532)
{DEFAULT @minAge = '0'} --integer for minimum allowable age
{DEFAULT @maxAge = '100'} --integer for maximum allowable age
{DEFAULT @minIndex = '1/1/2000'} --date for minimum allowable data for index exposure
{DEFAULT @maxIndex = '1/1/2013'} --date for maximum allowable data for index exposure
{DEFAULT @stratifyGender = '1'} --if 1, analysis will be calculated overall, and stratified across all gender groups
{DEFAULT @stratifyAge = '1'} --if 1, analysis will be calculated overall, and stratified across all age groups  (using AGE_GROUP table below)
{DEFAULT @stratifyIndex = '1'} --if 1, analysis will be calculated overall, and stratified across all years of the index dates
{DEFAULT @useLengthOfExposureExposed = '1'} --if 1, use the duration from drug_era_start -> drug_era_end as part of time_at_risk
{DEFAULT @timeAtRiskExposedStart = '1'} --integer of days to add to drug_era_start for start of time_at_risk (0 to include index date, 1 to start the day after)
{DEFAULT @surveillanceExposed = '30'} --additional window to add to end of exposure period (if @useLengthOfExposureExposed = 1, then add to DRUG_ERA_END, else add to DRUG_ERA_START)
{DEFAULT @useLengthOfExposureUnexposed = '1'} --if 1, use the duration from drug_era_start -> drug_era_end as part of time_at_risk looking back before drug_era_start
{DEFAULT @timeAtRiskUnexposedStart = '-1'} --integer of days to add to drug_era_start for start of time_at_risk (0 to include index date, -1 to start the day before)
{DEFAULT @surveillanceUnexposed = '-30'} --additional window to add to end of exposure period (if @useLengthOfExposureUnexposed = 1, then add to DRUG_ERA_END, else add to DRUG_ERA_START)
{DEFAULT @hasFullTimeAtRisk = '0'} --if 1, restrict to people who have full time-at-risk exposed and unexposed
{DEFAULT @washoutPeriodLength = '0'} --integer to define required time observed before exposure start
{DEFAULT @followupPeriodLength = '0'} --integer to define required time observed after exposure start
{DEFAULT @shrinkage = '0.0001'} --shrinkage used in IRR calculations, required >0 to deal with 0 case counts, but larger number means more shrinkage

--Delete these :
--drop table #age_group;
--drop table #scc_exposure_summary;
--drop table #scc_outcome_summary;

{@createResultsTable} ? {
IF OBJECT_ID('@resultsSchema.dbo.@resultsTablePrefix_results', 'U') IS NOT NULL 
  DROP TABLE @resultsSchema.dbo.@resultsTablePrefix_results;

IF OBJECT_ID('@resultsSchema.dbo.@resultsTablePrefix_analysis', 'U') IS NOT NULL 
	DROP TABLE @resultsSchema.dbo.@resultsTablePrefix_analysis;

CREATE TABLE @resultsSchema.dbo.@resultsTablePrefix_analysis (
  analysisId INT NOT NULL,
	firstOccurrenceDrugOnly VARCHAR(5),
	firstOccurrenceConditionOnly VARCHAR(5),
	drugTypeConceptIdList VARCHAR(255),
	conditionTypeConceptIdList VARCHAR(255),
	genderConceptIdList VARCHAR(255),
	minAge VARCHAR(10),
	maxAge VARCHAR(10),
	minIndex VARCHAR(4),
	maxIndex VARCHAR(4),
	stratifyGender VARCHAR(5),
	stratifyAge VARCHAR(5),
	stratifyIndex VARCHAR(5),
	useLengthOfExposureExposed VARCHAR(5),
	timeAtRiskExposedStart INT,
	surveillanceExposed INT,
	useLengthOfExposureUnexposed VARCHAR(5),
	timeAtRiskUnexposedStart INT,
	surveillanceUnexposed INT,
	hasFullTimeAtRisk VARCHAR(5),
	washoutPeriodLength INT,
	followupPeriodLength INT,
	shrinkage FLOAT
);

} 
INSERT INTO
	@resultsSchema.dbo.@resultsTablePrefix_analysis
SELECT
	@analysisId AS analysisId,
	'@firstOccurrenceDrugOnly' AS firstOccurrenceDrugOnly,
	'@firstOccurrenceConditionOnly' AS firstOccurrenceConditionOnly,
	'@drugTypeConceptIdList' AS drugTypeConceptIdList,
	'@conditionTypeConceptIdList' AS conditionTypeConceptIdList,
	'@genderConceptIdList' AS genderConceptIdList,
	'@minAge' AS minAge,
	'@maxAge' AS maxAge,
	'@minIndex' AS minIndex,
	'@maxIndex' AS maxIndex,
	'@stratifyGender' AS stratifyGender,
	'@stratifyAge' AS stratifyAge,
	'@stratifyIndex' AS stratifyIndex,
	'@useLengthOfExposureExposed' AS useLengthOfExposureExposed,
	@timeAtRiskExposedStart AS timeAtRiskExposedStart,
	@surveillanceExposed AS surveillanceExposed,
	'@useLengthOfExposureUnexposed' AS useLengthOfExposureUnexposed,
	@timeAtRiskUnexposedStart AS timeAtRiskUnexposedStart,
	@surveillanceUnexposed AS surveillanceUnexposed,
	'@hasFullTimeAtRisk' AS hasFullTimeAtRisk,
	@washoutPeriodLength AS washoutPeriodLength,
	@followupPeriodLength AS followupPeriodLength,
	@shrinkage AS shrinkage
WHERE
  NOT EXISTS (
    SELECT 
      *
    FROM
      @resultsSchema.dbo.@resultsTablePrefix_analysis
    WHERE
      analysisId = @analysisId
  )
;


USE @cdmSchema; 

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
	d1.@exposureConceptId AS exposure_concept_id,
	{@stratifyGender} ? {gender_concept_id,} : {'ALL' AS gender_concept_id,}
	{@stratifyAge} ? {ag1.age_group_name,} : {'ALL' AS age_group_name,}
	{@stratifyIndex} ? {YEAR(d1.@exposureStartDate) AS index_year,} : {'ALL' AS index_year,}
	COUNT(DISTINCT d1.@exposurePersonId) AS num_persons,
	COUNT(d1.@exposurePersonId) AS num_exposures,
	SUM(
		--need to account for potential censoring due to observation period length	
		CASE WHEN 
			DATEADD(
				dd,
				{@useLengthOfExposureExposed} ? {DATEDIFF(dd,d1.@exposureStartDate,d1.@exposureEndDate)} : {0} + @surveillanceExposed,
				@exposureStartDate
			) <= op1.observation_period_end_date
		THEN 
			DATEDIFF(
				dd,
				DATEADD(dd, @timeAtRiskExposedStart, @exposureStartDate),	
				DATEADD(dd, {@useLengthOfExposureExposed} ? {DATEDIFF(dd,d1.@exposureStartDate,d1.@exposureEndDate)} : {0} + @surveillanceExposed,
				@exposureStartDate)
			)
		ELSE
			DATEDIFF(
				dd, 
				DATEADD(dd, @timeAtRiskExposedStart, @exposureStartDate), 
				op1.observation_period_end_date
			)
		END
	) / 365.25 AS time_at_risk_exposed,		
	SUM(
		CASE WHEN 
			DATEADD(
				dd,
				{@useLengthOfExposureUnexposed} ? {-1*DATEDIFF(dd,d1.@exposureStartDate,d1.@exposureEndDate)} : {0} + @surveillanceUnexposed,
				@exposureStartDate
			) >= op1.observation_period_start_date
		THEN 		
			DATEDIFF(
				dd,
				DATEADD(dd, {@useLengthOfExposureUnexposed} ? {-1*DATEDIFF(dd, d1.@exposureStartDate, d1.@exposureEndDate)} : {0} + @surveillanceUnexposed, @exposureStartDate), 
				DATEADD(dd, @timeAtRiskUnexposedStart, @exposureStartDate)
			)
		ELSE  
			DATEDIFF(
				dd, 
				op1.observation_period_start_date, 
				DATEADD(dd, @timeAtRiskUnexposedStart, @exposureStartDate)
			)
		END
	) / 365.25 AS time_at_risk_unexposed
INTO 
	#scc_exposure_summary
FROM
	(
		SELECT 
			@exposurePersonId, 
			@exposureConceptId, 
			@exposureStartDate, 
			@exposureEndDate
		FROM
			(
				SELECT 
					@exposurePersonId, 
					@exposureConceptId, 
					@exposureStartDate, 
					@exposureEndDate
					{@firstOccurrenceDrugOnly} ? {,ROW_NUMBER() OVER (PARTITION BY @exposurePersonId, @exposureConceptId, drug_type_concept_id ORDER BY @exposureStartDate) AS rn1} 
				FROM 
					@exposureTable 
				WHERE 
					1=1
					{@exposuresOfInterest != ''} ? {AND @exposureConceptId IN (@exposuresOfInterest)}
					{@drugTypeConceptIdList != '' & exposure_table != 'cohort'} ? {AND drug_type_concept_id IN (@drugTypeConceptIdList)}
			) t1
		{@firstOccurrenceDrugOnly} ? {WHERE rn1 = 1}
	) d1
	INNER JOIN
		@personTable p1
	ON 
		d1.@exposurePersonId = p1.person_id
	INNER JOIN
		@observationPeriodTable op1
	ON 
		d1.@exposurePersonId = op1.person_id,
	#age_group ag1
WHERE 
		d1.@exposureStartDate >= op1.observation_period_start_date
	AND 
		d1.@exposureStartDate <= op1.observation_period_end_date
	AND 
		YEAR(d1.@exposureStartDate) - year_of_birth >= ag1.age_group_min
	AND 
		YEAR(d1.@exposureStartDate) - year_of_birth <= ag1.age_group_max
	{@minIndex != ''} ? {AND @exposureStartDate >= '@minIndex'}
	{@maxIndex != ''} ? {AND @exposureStartDate <= '@maxIndex'}
	{@minAge != ''} ? {AND YEAR(d1.@exposureStartDate) - p1.year_of_birth  >= @minAge}
	{@maxAge != ''} ? {AND YEAR(d1.@exposureStartDate) - p1.year_of_birth  <= @maxAge}
	{@genderConceptIdList != ''} ? {AND p1.gender_concept_id IN (@genderConceptIdList)}
	{@hasFullTimeAtRisk}  ? {
		AND 
			op1.observation_period_end_date >= DATEADD(dd, {@useLengthOfExposureExposed} ? {DATEDIFF(dd,d1.@exposureStartDate,d1.@exposureEndDate)} : {0} + @surveillanceExposed, @exposureStartDate)
		AND 
			op1.observation_period_start_date <= DATEADD(dd, {@useLengthOfExposureExposed} ? {DATEDIFF(dd,d1.@exposureStartDate,d1.@exposureEndDate)} : {0} + @surveillanceUnexposed, @exposureStartDate)	
	}
	{@washoutPeriodLength != ''} ? {AND DATEDIFF(dd, op1.observation_period_start_date, d1.@exposureStartDate) >= @washoutPeriodLength}
	{@followupPeriodLength != ''} ? {AND DATEDIFF(dd,d1.@exposureStartDate, op1.observation_period_end_date) >= @followupPeriodLength}
GROUP BY 
	@exposureConceptId
	{@stratifyGender} ? {, gender_concept_id}
	{@stratifyAge} ? {, ag1.age_group_name}
	{@stratifyIndex} ? {, YEAR(d1.@exposureStartDate)}
;


--query 2:  number of events in each time window
SELECT 
	d1.@exposureConceptId AS exposure_concept_id,
	c1.@outcomeConceptId AS outcome_concept_id,
	{@stratifyGender} ? {gender_concept_id,} : {'ALL' AS gender_concept_id,}
	{@stratifyAge} ? {ag1.age_group_name,} : {'ALL' AS age_group_name,}
	{@stratifyIndex} ? {YEAR(d1.@exposureStartDate) AS index_year,} : {'ALL' AS index_year,}
	SUM(
		CASE WHEN 
				c1.@outcomeStartDate >= DATEADD(dd,@timeAtRiskExposedStart,@exposureStartDate)
			AND 
				c1.@outcomeStartDate <= DATEADD(dd, {@useLengthOfExposureExposed}?{DATEDIFF(dd,d1.@exposureStartDate,d1.@exposureEndDate)}:{0} + @surveillanceExposed ,@exposureStartDate)
			AND 
				c1.@outcomeStartDate <= op1.observation_period_end_date
		THEN 
			1 
		ELSE 
			0 
		END
	) AS num_outcomes_exposed,
	SUM(
		CASE WHEN 
			c1.@outcomeStartDate <= DATEADD(dd,@timeAtRiskUnexposedStart, @exposureStartDate)
		AND 
			c1.@outcomeStartDate >= DATEADD(dd,{@useLengthOfExposureUnexposed} ? {-1*DATEDIFF(dd,d1.@exposureStartDate,d1.@exposureEndDate)} : {0} + @surveillanceUnexposed ,@exposureStartDate)	
		AND 
			c1.@outcomeStartDate >= op1.observation_period_start_date
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
			@exposurePersonId, 
			@exposureConceptId, 
			@exposureStartDate, 
			@exposureEndDate
		FROM
			(
				SELECT 
					@exposurePersonId, 
					@exposureConceptId, 
					@exposureStartDate, 
					@exposureEndDate
					{@firstOccurrenceDrugOnly}?{,ROW_NUMBER() OVER (PARTITION BY @exposurePersonId, @exposureConceptId, drug_type_concept_id ORDER BY @exposureStartDate) AS rn1}
				FROM 
					@exposureTable 
				WHERE 
						1=1
					{@exposuresOfInterest != ''} ? {AND @exposureConceptId IN (@exposuresOfInterest)}
					{@drugTypeConceptIdList != '' & exposure_table != 'cohort'} ? {AND drug_type_concept_id IN (@drugTypeConceptIdList) }
	
			) T1
		{@firstOccurrenceDrugOnly} ? {WHERE rn1 = 1}
	) D1
INNER JOIN
	@personTable p1
ON 
	d1.@exposurePersonId = p1.person_id
INNER JOIN
	(
		SELECT 
			@outcomePersontId, 
			@outcomeConceptId, 
			@outcomeStartDate, 
			@outcomeEndDate
		FROM
			(
				SELECT 
					@outcomePersontId, 
					@outcomeConceptId, 
					@outcomeStartDate, 
					@outcomeEndDate
					{@firstOccurrenceConditionOnly} ? {,ROW_NUMBER() OVER (PARTITION BY @outcomePersontId, @outcomeConceptId, condition_type_concept_id ORDER BY @outcomeStartDate) AS rn1}
				FROM 
					@outcomeTable 
				WHERE 
						1=1
					{@outcomesOfInterest != ''} ? {AND @outcomeConceptId IN (@outcomesOfInterest)}
					{@conditionTypeConceptIdList & outcome_table != 'cohort'} ? {AND condition_type_concept_id IN (@conditionTypeConceptIdList)}
			) T1
		{@firstOccurrenceConditionOnly} ? {WHERE rn1 = 1}
	) c1
ON 
	p1.person_id = c1.@outcomePersontId
INNER JOIN
	@observationPeriodTable op1
ON 
	d1.@exposurePersonId = op1.person_id,
	#age_group ag1
WHERE 
		d1.@exposureStartDate >= op1.observation_period_start_date
	AND 
		d1.@exposureStartDate <= op1.observation_period_end_date
	AND 
		YEAR(d1.@exposureStartDate) - year_of_birth >= ag1.age_group_min
	AND 
		YEAR(d1.@exposureStartDate) - year_of_birth <= ag1.age_group_max
	{@minIndex != ''} ? {AND @exposureStartDate >= '@minIndex'}
	{@maxIndex != ''} ? {AND @exposureStartDate <= '@maxIndex'}
	{@minAge != ''} ? {AND YEAR(d1.@exposureStartDate) - p1.year_of_birth >= @minAge}
	{@maxAge != ''} ? {AND YEAR(d1.@exposureStartDate) - p1.year_of_birth <= @maxAge}
	{@genderConceptIdList != ''} ? {AND p1.gender_concept_id IN (@genderConceptIdList)}
	{@hasFullTimeAtRisk} ? {
		AND 
			op1.observation_period_end_date >= DATEADD(
				dd, 
				{@useLengthOfExposureExposed = 1} ? {DATEDIFF(dd,d1.@exposureStartDate,d1.@exposureEndDate)} : {0} + @surveillanceExposed,
				@exposureStartDate)		
		AND 
			op1.observation_period_start_date <= DATEADD(
				dd,
				{@useLengthOfExposureExposed} ? {DATEDIFF(DD,d1.@exposureStartDate,d1.@exposureEndDate)} : {0} + @surveillanceUnexposed,
				@exposureStartDate)	
	}
	{@washoutPeriodLength != 0} ? {AND DATEDIFF(DD,op1.observation_period_start_date,d1.@exposureStartDate) >= @washoutPeriodLength}
	{@followupPeriodLength != 0} ? {AND DATEDIFF(DD,d1.@exposureStartDate, op1.observation_period_end_date) >= @followupPeriodLength}
GROUP BY 
	@exposureConceptId, 
	@outcomeConceptId
	{@stratifyGender} ? {, gender_concept_id}
	{@stratifyAge} ? {, ag1.age_group_name}
	{@stratifyIndex} ? {, YEAR(d1.@exposureStartDate)}
;

--now create final summary table
{!@createResultsTable} ? {
INSERT INTO 
	@resultsSchema.dbo.@resultsTablePrefix_results
}
SELECT 
  '@sourceName' AS sourceName,
	@analysisId AS analysisId,
	exposure_concept_id AS exposureConceptId,
	outcome_concept_id AS outcomeConceptId,
	gender_concept_id AS genderConceptId,
	age_group_name AS ageGroupName,
	index_year AS indexYear,
	num_persons AS numPersons,
	num_exposures AS numExposures,
	num_outcomes_exposed AS numOutcomesExposed,
	num_outcomes_unexposed AS numOutcomesUnexposed,
	time_at_risk_exposed AS timeAtRiskExposed,
	time_at_risk_unexposed AS timeAtRiskUnexposed,
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
	) AS irrLb95,

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
	) AS irrUb95,	
	
	--calcuating SElogIRR
	SQRT( (1.0/ CASE WHEN t1.num_outcomes_exposed = 0 THEN 0.5 ELSE t1.num_outcomes_exposed END) + (1.0/ CASE WHEN t1.num_outcomes_unexposed = 0 THEN 0.5 ELSE t1.num_outcomes_unexposed END) ) AS seLogIrr
{@createResultsTable} ? {	
INTO 
	@resultsSchema.dbo.@resultsTablePrefix_results
}
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

		{@stratifyGender} ? {
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
		{@stratifyAge} ? {
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
		{@stratifyIndex} ? {
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
		{@stratifyGender & @stratifyAge} ? {
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
		{@stratifyGender & @stratifyIndex} ? {
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
		{@stratifyAge & @stratifyIndex} ? {
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
		{@stratifyGender & @stratifyAge & @stratifyIndex} ? {
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
