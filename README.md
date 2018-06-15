# CoDa (beta version)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mpascariu/CoDa?branch=master&svg=true)](https://ci.appveyor.com/project/mpascariu/CoDa)
[![Linux Build Status](https://travis-ci.org/mpascariu/CoDa.svg?branch=master)](https://travis-ci.org/mpascariu/CoDa)
[![codecov](https://codecov.io/github/mpascariu/CoDa/branch/master/graphs/badge.svg)](https://codecov.io/github/mpascariu/CoDa)
[![license](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://github.com/mpascariu/CoDa/blob/master/LICENSE)
This repository contains the source code for the CoDa model for forecasting mortality.

Installation
============

1. Make sure you have installed the most recent version of R ( https://www.r-project.org )
2. Install the package in R using **devtools** by running the following code in your R console:

```r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("mpascariu/CoDa")
```

Help
===============
All functions are documented in the standard way, which means that 
once you load the package using ```library(CoDa)```
you can just type ```?CoDa``` to see the help file. 


