\name{selfControlledCohort}
\alias{selfControlledCohort}
\title{selfControlledCohort}
\usage{
selfControlledCohort(connectionDetails, cdmSchema, resultsSchema, sourceName,
  analysisIds)
}
\arguments{
  \item{connectionDetails}{An R object of type
  ConnectionDetail (details for the function that contains
  server info, database type, optionally username/password,
  port)}

  \item{cdmSchema}{string name of databsae schema that
  contains OMOP CDM and vocabulary}

  \item{resultsSchema}{string name of database schema that
  we can write results to}

  \item{sourceName}{string name of the database, as
  recorded in results}

  \item{analysisIds}{(optional) a vector containing the set
  of OSCAR analysisIds for which results will be generated.
  If not specified, all analyses will be executed.}
}
\value{
An object of type \code{sccResults} containing details for
connecting to the database containing the results
}
\description{
\code{selfControlledCohort} generates population-level
estimation from OMOP CDMv4 instance by comparing exposed
and unexposed time among exposed cohort.
}
\details{
PATRICK HOMEWORK: complete details
}
\examples{
\dontrun{
  connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  oscarResults <- oscar(connectionDetails, "cdm4_sim", "scratch", "TestDB")
  plot(oscarResults, "population")
}
}

