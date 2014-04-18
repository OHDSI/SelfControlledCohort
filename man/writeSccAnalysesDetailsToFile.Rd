\name{writeSccAnalysesDetailsToFile}
\alias{writeSccAnalysesDetailsToFile}
\title{writeSccAnalysesDetailsToFile}
\usage{
writeSccAnalysesDetailsToFile(sccAnalysesDetails, file)
}
\arguments{
  \item{sccAnalysesDetails}{the \code{sccAnalysesDetails}
  to be written to file}

  \item{file}{the name of the file where the results will
  be written}
}
\description{
\code{writeSccAnalysesDetailsToFile} writes an object of
type \code{analysesDetails} to a CSV file
}
\examples{
\dontrun{
  analysesDetails <- NULL
  analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 1,firstOccurrenceDrugOnly=TRUE),analysesDetails)
  analysesDetails <- appendToSccAnalysesDetails(createSccAnalysisDetails(analysisId = 2,firstOccurrenceDrugOnly=FALSE),analysesDetails)
  writeSccAnalysesDetailsToFile(analysesDetails,"c:/temp/test.csv")
}
}

