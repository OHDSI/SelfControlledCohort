SelfControlledCohort
====================

Introduction
============
This package provides a method to estimate risk by comparing time exposed with time unexposed among the exposed cohort.

Features
========
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Supports stratification by age, gender, and index year.

Example
========
```r
library(SelfControlledCohort)

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             user = "joe",
                                             password = "secret",
                                             server = "myserver")
                                             
sccResults <- runSelfControlledCohort(connectionDetails,
                                     cdmDatabaseSchema = "cdm_data",
                                     exposureIds = c(767410, 1314924, 907879),
                                     outcomeIds = 444382,
                                     outcomeTable = "condition_era")

summary(sccResults)
```

Technology
============
SelfControlledCohort is an R package.

System Requirements
============
Requires R. Libraries used in SelfControlledCohort require Java.

Dependencies
============
 * DatabaseConnector
 * SqlRender
 * OhdsiRTools

Getting Started
===============
1. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
2. In R, use the following commands to download and install CohortMethod:

  ```r
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/OhdsiRTools") 
  install_github("ohdsi/SqlRender")
  install_github("ohdsi/DatabaseConnector")
  install_github("ohdsi/SelfControlledCohort")
  ```

Getting Involved
================
* Package manual: [SelfControlledCohort.pdf](https://raw.githubusercontent.com/OHDSI/SelfControlledCohort/master/extras/SelfControlledCohort.pdf)
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
SelfControlledCohort is licensed under Apache License 2.0

Development
===========
SelfControlledCohort is being developed in R Studio.

### Development status
[![Build Status](https://travis-ci.org/OHDSI/SelfControlledCohort.svg?branch=master)](https://travis-ci.org/OHDSI/SelfControlledCohort)
[![codecov.io](https://codecov.io/github/OHDSI/SelfControlledCohort/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/SelfControlledCohort?branch=master)

Beta
