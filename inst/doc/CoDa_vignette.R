## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  if (!requireNamespace("devtools")) install.packages("devtools")
#  devtools::install_github("mpascariu/CoDa")

## ------------------------------------------------------------------------
rm(list = ls())
library(CoDa)

model1 <- CoDa(edd, x = 0:110, t = 1960:2014)
model1

## ---- echo=FALSE---------------------------------------------------------
plot(edd[, '2010'], xlab = 'Age (x)', ylab = 'dx', ylim = c(0, 0.04), pch = 16)
lines(model1$fitted[, '2010'], col = 2)
legend('topleft', pch = c(16, NA), lty = c(NA, 1), lwd = 3, col = c(1, 2), bty = 'n',
       legend = c('Observed values (2010)', 'Fitted values (2010)'))

## ------------------------------------------------------------------------
ls(model1)

## ------------------------------------------------------------------------
summary(model1)

## ------------------------------------------------------------------------
fc_model1 <- predict(model1, n = 30, jumpchoice = 'actual')
fc_model1

## ---- echo=FALSE---------------------------------------------------------
plot(edd[, '2014'], xlab = 'Age (x)', ylab = 'dx', ylim = c(0, 0.045), pch = 16)
lines(fc_model1$predicted.values$mean[, '2044'], col = 3, lwd = 2)
legend('topleft', pch = c(16, NA), lty = c(NA, 1), lwd = 3, col = c(1, 3), bty = 'n',
       legend = c('Observed distribution in 2014', 'Forecast values in 2044'))

