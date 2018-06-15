

#' Imports
#' @importFrom stats coef
#' @importFrom compositions acomp geometricmeanCol clr clrInv
#' @importFrom forecast forecast Arima arimaorder auto.arima
#' @importFrom utils head tail
#' @name foo_imports
#' @keywords internal
NULL


#' Data - for testing purposes
#'
#' Dataset containing empirical distribution of deaths
#' for US female population between 1960 and 2014. 
#' This data is provided for testing purposes only.
#' Download the actual data free of charge from \url{http://www.mortality.org}.  
#'
#' @seealso \code{\link{CoDa}}
#' @source Human Mortality Database, \url{http://www.mortality.org}.
"CoDa.data"
