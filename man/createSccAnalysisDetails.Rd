\name{createSccAnalysisDetails}
\alias{createSccAnalysisDetails}
\title{createSccAnalysisDetails}
\usage{
createSccAnalysisDetails(analysisId = 1, firstOccurrenceDrugOnly = TRUE,
  firstOccurrenceConditionOnly = TRUE, drugTypeConceptIdList = c(38000182),
  conditionTypeConceptIdList = c(38000247), genderConceptIdList = c(8507,
  8532), minAge = "", maxAge = "", minIndex = "", maxIndex = "",
  stratifyGender = FALSE, stratifyAge = FALSE, stratifyIndex = FALSE,
  useLengthOfExposureExposed = TRUE, timeAtRiskExposedStart = 1,
  surveillanceExposed = 30, useLengthOfExposureUnexposed = TRUE,
  timeAtRiskUnexposedStart = -1, surveillanceUnexposed = -30,
  hasFullTimeAtRisk = FALSE, washoutPeriodLength = 0,
  followupPeriodLength = 0, shrinkage = 1e-04)
}
\arguments{
  \item{analysisId}{A unique identifier that can later be
  used to identify the results of this analysis}

  \item{firstOccurrenceDrugOnly}{if TRUE, only use first
  occurrence of each drug concept id for each person}

  \item{firstOccurrenceConditionOnly}{if TRUE, only use
  first occurrence of each condition concept id for each
  person}

  \item{drugTypeConceptIdList}{which drug_type to use:
  generally only use 1 value (ex: 30d era)}

  \item{conditionTypeConceptIdList}{which condition_type to
  use: generally only use 1 value (ex: 30d era)}

  \item{genderConceptIdList}{list of gender_concept_id,
  generally use MALE (8507) and FEMALE (8532)}

  \item{minAge}{integer for minimum allowable age}

  \item{maxAge}{integer for maximum allowable age}

  \item{minIndex}{date for minimum allowable data for index
  exposure}

  \item{maxIndex}{date for maximum allowable data for index
  exposure}

  \item{stratifyGender}{if TRUE, analysis will be
  calculated overall, and stratified across all gender
  groups}

  \item{stratifyAge}{if TRUE, analysis will be calculated
  overall, and stratified across all age groups (using
  AGE_GROUP table below)}

  \item{stratifyIndex}{if TRUE, analysis will be calculated
  overall, and stratified across all years of the index
  dates}

  \item{useLengthOfExposureExposed}{if TRUE, use the
  duration from drugEraStart -> drugEraEnd as part of
  timeAtRisk}

  \item{timeAtRiskExposedStart}{integer of days to add to
  drugEraStart for start of timeAtRisk (0 to include index
  date, 1 to start the day after)}

  \item{surveillanceExposed}{additional window to add to
  end of exposure period (if useLengthOfExposureExposed =
  TRUE, then add to exposure end date, else add to exposure
  start date)}

  \item{useLengthOfExposureUnexposed}{if TRUE, use the
  duration from exposure start -> exposure end as part of
  timeAtRisk looking back before exposure start}

  \item{timeAtRiskUnexposedStart}{integer of days to add to
  exposure start for start of timeAtRisk (0 to include
  index date, -1 to start the day before)}

  \item{surveillanceUnexposed}{additional window to add to
  end of exposure period (if useLengthOfExposureUnexposed =
  TRUE, then add to exposure end date, else add to exposure
  start date)}

  \item{hasFullTimeAtRisk}{if TRUE, restrict to people who
  have full time-at-risk exposed and unexposed}

  \item{washoutPeriodLength}{integer to define required
  time observed before exposure start}

  \item{followupPeriodLength}{integer to define required
  time observed after exposure start}

  \item{shrinkage}{shrinkage used in IRR calculations,
  required >0 to deal with 0 case counts, but larger number
  means more shrinkage}
}
\value{
An object of type \code{sccAnalysisDetails} containing the
details of the analysis to be run
}
\description{
\code{createSccAnalysisDetails} generates an object
specifying one set of analysis choices for the
self-controlled cohort method.
}
\examples{
\dontrun{
  connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  analysesDetails <- NULL
  analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 1,firstOccurrenceDrugOnly=TRUE),analysesDetails)
  analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 2,firstOccurrenceDrugOnly=FALSE),analysesDetails)
  sccResult <- selfControlledCohort(analysesDetails, connectionDetails, "cdm_truven_mdcr", "scratch", sourceName = "cdm_truven_mdcr", exposuresOfInterest = c(767410,1314924,907879), outcomesOfInterest = c(444382, 79106, 138825), outcomeTable = "condition_era")
  plot(sccResult)
}
}

