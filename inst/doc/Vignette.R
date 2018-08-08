## ------------------------------------------------------------------------
library(CoDa)

## ------------------------------------------------------------------------
CoDa.data[1:5, 1:5]

## ---- message=FALSE------------------------------------------------------
M <- coda(dx = CoDa.data, x = 0:110, y = 1960:2014)
M

## ------------------------------------------------------------------------
ls(M)

## ------------------------------------------------------------------------
summary(M)

## ---- fig.asp=0.4, fig.width=10------------------------------------------
plot(M, plotType = "coef", ylab = "values")

## ---- fig.width=15, fig.height=6-----------------------------------------
plot(M, plotType = "data")

## ---- fig.asp=0.4, fig.width=10------------------------------------------
plot(resid(M), plotType = "scatter")

## ---- fig.asp = 0.5, fig.width=10----------------------------------------
plot(resid(M), plotType = "colourmap")

## ---- fig.asp = 0.5, fig.width=10----------------------------------------
plot(resid(M), plotType = "signplot")

## ------------------------------------------------------------------------
P <- predict(M, h = 30, jumpchoice = 'actual')
P
# list of objects in predict
ls(P)

# Predicted distribution of death
head(P$predicted.values, 3)

