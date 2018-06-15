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


## References
Find out more about CoDa method and possible applications here:

Bergeron-Boucher, M-P., Canudas-Romo, V., Oeppen, J. and Vaupel, W.J. 2017. [Coherent forecasts of mortality with compositional data analysis.](http://doi.org/10.4054/DemRes.2017.37.17) Demographic Research, Volume 17, Article 17, Pages 527--566.

Aitchison, J. 1986. [The Statistical Analysis of Compositional Data.](http://www.leg.ufpr.br/lib/exe/fetch.php/pessoais:abtmartins:a_concise_guide_to_compositional_data_analysis.pdf) London: Chapman and Hall. 2015.

Wikipedia. [Compositional data](https://en.wikipedia.org/wiki/Compositional_data) 
