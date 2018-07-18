# Compositional Data Mortality Model (CoDa) - R Package
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mpascariu/CoDa?branch=master&svg=true)](https://ci.appveyor.com/project/mpascariu/CoDa)
[![Linux Build Status](https://travis-ci.org/mpascariu/CoDa.svg?branch=master)](https://travis-ci.org/mpascariu/CoDa)
[![codecov](https://codecov.io/github/mpascariu/CoDa/branch/master/graphs/badge.svg)](https://codecov.io/github/mpascariu/CoDa)
[![license](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://github.com/mpascariu/CoDa/blob/master/LICENSE)

## Description

This repository contains the implementation of the Compositional Data Mortality 
Model (CoDa). This is a Lee-Carter (1992) type method that is used to modelling and 
forecasting the life table distribution of deaths (dx) using Principal Component 
Analysis. In the context of mortality forecasting the CoDa method was fist used 
in Bergeron-Boucher et al. (2017). The package includes functions for fitting 
the model, analysing it's goodness-of-fit and performing mortality projections.

## Installation

1. Make sure you have the most recent version of R
2. Run the following code in your R console 

```R
install.packages("CoDa")
```

## Updating to the latest version of the package

You can track and contribute to the development of `CoDa` on [GitHub](https://github.com/mpascariu/MortalityLaws). To install it:

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. Make sure you have a working development environment.
    * **Windows**: Install [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/).
    * **Mac**: Install `Xcode` from the Mac App Store.
    * **Linux**: Install a compiler and various development libraries (details vary across different flavors of Linux).

3. Install the development version of `CoDa`.

   ```R
   devtools::install_github("mpascariu/CoDa")
   ```

## Help
All functions are documented in the standard way, which means that 
once you load the package using ```library(CoDa)```
you can just type ```?coda``` to see the help file. 


## References

Bergeron-Boucher, M-P., Canudas-Romo, V., Oeppen, J. and Vaupel, W.J. 2017. [Coherent forecasts of mortality with compositional data analysis.](http://doi.org/10.4054/DemRes.2017.37.17) Demographic Research, Volume 17, Article 17, Pages 527--566.

Oeppen, J. 2008. Coherent forecasting of multiple-decrement life tables: A test using Japanese cause of death data. Paper presented at the European Population Conference 2008, Barcelona, Spain, July 9-12, 2008.

Aitchison, J. 1986. [The Statistical Analysis of Compositional Data.](http://www.leg.ufpr.br/lib/exe/fetch.php/pessoais:abtmartins:a_concise_guide_to_compositional_data_analysis.pdf) London: Chapman and Hall. 2015.

Ronald D. Lee and Lawrence R. Carter. 1992. [Modeling and Forecasting U.S. Mortality](http://doi.org/10.1080/01621459.1992.10475265), Journal of the American Statistical Association, 87:419, 659--671.

Wikipedia. [Compositional data](https://en.wikipedia.org/wiki/Compositional_data) 
