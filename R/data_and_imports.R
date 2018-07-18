

#' Imports
#' @importFrom stats coef fitted
#' @importFrom compositions acomp geometricmeanCol clr clrInv
#' @importFrom forecast forecast Arima arimaorder auto.arima
#' @importFrom utils head tail
#' @importFrom graphics par plot abline image.default
#' @importFrom grDevices colorRampPalette grey.colors
#' @importFrom RColorBrewer brewer.pal
#' @importFrom fields image.plot
#' @importFrom reshape2 melt
#' @name foo_imports
#' @keywords internal
NULL


#' DATA in the package - for testing purposes
#'
#' Dataset containing empirical distribution of deaths
#' for US female population between 1960 and 2014. 
#' The data is provided in the package for testing purposes only.
#' By the time you are using it, it may be outdated. Download actual 
#' demographic data free of charge from Human Mortality Database. 
#' Once a username and a password is created on the 
#' \href{http://www.mortality.org}{website} the 
#' \href{https://CRAN.R-project.org/package=MortalityLaws}{MortalityLaws} 
#' R package can be used to extract data directly into your R console.
#' @source \href{http://www.mortality.org}{Human Mortality Database}
#' @seealso \code{\link{coda}}
"CoDa.data"
