
rm(list = ls())
library(CoDa)

# Fit CoDa model
fit_CoDa <- CoDa(edd, x = 0:110, t = 1960:2014)
fit_CoDa

ls(fit_CoDa)
summary(fit_CoDa)


# Predict life expectancy 20 years in the future using CoDa model
pred_Coda <- predict(fit_CoDa, n = 20)

