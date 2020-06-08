SelfControlledCohort
====================

[![Build Status](https://travis-ci.org/OHDSI/SelfControlledCohort.svg?branch=master)](https://travis-ci.org/OHDSI/SelfControlledCohort)
[![codecov.io](https://codecov.io/github/OHDSI/SelfControlledCohort/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/SelfControlledCohort?branch=master)

SelfControlledCohort is part of [HADES](https://ohdsi.github.io/Hades).

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

Getting Started
===============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2. In R, use the following commands to download and install SelfControlledCohort:

  ```r
  install.packages("drat")
  drat::addRepo("OHDSI")
  install.packages("SelfControlledCohort")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/SelfControlledCohort).

PDF versions of the documentation are also available:
* Package manual: [SelfControlledCohort.pdf](https://raw.githubusercontent.com/OHDSI/SelfControlledCohort/master/extras/SelfControlledCohort.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/SelfControlledCohort/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.


License
=======
SelfControlledCohort is licensed under Apache License 2.0

Development
===========
SelfControlledCohort is being developed in R Studio.

### Development status

Beta
