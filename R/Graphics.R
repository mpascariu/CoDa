

#' Plot fitted parameters from a stochastic CoDa mortality model
#' @param x An object of class \code{"coda"} with the fitted 
#' parameters of the mortality model.
#' @param type Type of plot to be drawn. See \code{\link[graphics]{plot}}.
#' @param ... Additional arguments to control graphical appearance.
#' @examples 
#' # Fit model
#' M <- coda(CoDa.data, x = 0:110, y = 1960:2014)
#' 
#' # Plot fitted parameters
#' plot(M)
#' plot(M, type = "p", pch = 19)
#' @export
#' 
plot.coda <- function(x, type = "l", ...){
  
  oldpar <- par(no.readonly = TRUE)
  age    <- x$x
  year   <- x$y
  C      <- coef(x)
  
  par(mfrow = c(1, 3))
  plot(age, C$ax, type = type, xlab = "age", ...)
  plot(age, C$bx, type = type, xlab = "age", ...)
  plot(year, C$kt, type = type, xlab = "year", ...)
  
  par(oldpar)
}


#' Plot the residuals of a CoDa Mortality Model
#' 
#' Plots the deviance residuals of a CoDa Mortality Model which are 
#' of class \code{"residuals.coda"}. Three types of plots
#' are available: scatter plot of residuals by age, period and cohort,
#' colour map (heatmap) of the residuals, and a black and white signplot 
#' of the residuals.
#' 
#' @param x An object of class \code{residuals.coda} with the residuals of a 
#' CoDa Mortality Model.
#' @param type The type of the plot. The alternatives are 
#' \code{"scatter"}(default), \code{"colourmap"}, and \code{"signplot"}.
#' @param reslim Optional numeric vector of length 2, giving the range of the 
#' residuals.
#' @param plotAge Logical value indicating if the age scatter plot should be 
#' produced. This is only used when \code{type = "scatter"}.
#' @param plotYear Logical value indicating if the calendar year scatter plot 
#' should be produced. This is only used when \code{type = "scatter"}.
#' @param plotCohort Logical value indicating if the cohort scatter plot 
#' should be produced. This is only used when \code{type = "scatter"}.
#' @param pch Optional symbol to use for the points in a scatterplot. 
#' This is only used when \code{type = "scatter"}. See 
#' \code{\link[graphics]{plot}}.
#' @param col Optional colours to use in plotting. If 
#' \code{type = "scatter"} this is a single colour to use in the points
#' in the scatter plots, while if \code{type = "colourmap"} this should
#' be a list of colours (see help in \code{\link[fields]{image.plot}} 
#' for details). This argument is ignored if \code{type = "signplot"}.
#' @param ... Other plotting parameters to be passed to the plotting 
#' functions. This can be used to control the appearance of the plots.
#'
#' @details
#' When \code{type = "scatter"} scatter plots of the residuals against age, 
#' calendar year and cohort (year of birth) are produced. 
#'
#' When \code{type = "colourmap"} a two dimensional colour map of the 
#' residuals is plotted. This is produced using function 
#' \code{\link[fields]{image.plot}}. See \code{\link[fields]{image.plot}} 
#' for further parameters that can be passed to this type of plots.
#'
#' When \code{type = "signplot"} a two dimensional black and white map of the
#'  residuals is plotted with dark grey representing negative residuals and 
#'  light grey representing positive residuals. This is produced using 
#'  function \code{\link[graphics]{image.default}}. 
#'   
#' @examples
#' # Fit model
#' M <- coda(CoDa.data, x = 0:110, y = 1960:2014)
#' 
#' # Plot residuals
#' res <- resid(M)
#' plot(res, type = "scatter")
#' plot(res, type = "colourmap")
#' plot(res, type = "signplot")
#' 
#' @source 
#' The code for producing the residual plots is inspired from the one published 
#' in \href{http://github.com/amvillegas/StMoMo}{\code{StMoMo}} R package. 
#' See \code{\link[StMoMo]{plot.resStMoMo}}. 
#' All the credit goes to it's authors: 
#' \href{https://github.com/amvillegas}{Andres Villegas}, Pietro Millossovich 
#' and Vladimir Kaishev.
#' @export 
#' 
plot.residuals.coda <- function(x, type = c("scatter", "colourmap", "signplot"), 
                                reslim = NULL, plotAge = TRUE, plotYear = TRUE, 
                                plotCohort  = TRUE, pch = 20, col = NULL, ...) {
  ages  <- as.numeric(rownames(x))
  years <- as.numeric(colnames(x))
  L     <- dim(x)
  res   <- as.data.frame(x[1:L[1], 1:L[2]])
  type  <- match.arg(type)
  oldpar <- par(no.readonly = TRUE)
  
  if (is.null(reslim)) {
    maxRes <- max(abs(res), na.rm = TRUE)
    reslim <- c(-maxRes, maxRes)
  }
  if (is.null(col) & type == "colourmap") {
    col <- colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(64)
  }
  if (is.null(col) & type == "scatter") {
    col <- "black"
  }
  
  switch(type, 
         scatter = scatterplotAPC(res, ages, years, 
                                  plotAge = plotAge, plotYear = plotYear, 
                                  plotCohort = plotCohort, pch = pch, 
                                  ylab = "residuals", ylim = reslim, 
                                  col = col, ...),
         colourmap = fields::image.plot(years, ages, t(res), 
                                        zlim = reslim, ylab = "age", 
                                        xlab = "calendar year", col = col, 
                                        ...),
         signplot = image.default(years, ages, t(res), 
                                  zlim = reslim, ylab = "age", 
                                  xlab = "calendar year", 
                                  breaks = c(-10e10, 0, 10e10), 
                                  col = grey.colors(2), ...)
  )
  par(oldpar)
}


#' Do a scatter plot of a matrix according to age-period-cohorts
#'
#' @param mat Matrix with the data to plot.
#' @param ages Ages corresponding to the rows in \code{mat}.
#' @param years Years corresponding to the columns in \code{mat}.  
#' @param plotAge Logical value indicating if the age scatter plot should be 
#' produced.
#' @param plotYear Logical value indicating if the calendar year scatter plot 
#' should be produced.
#' @param plotCohort Logical value indicating if the cohort scatter plot 
#' should be produced.
#' @param zeroLine Logical value indicating if a horizontal line at zero
#' should be plotted.
#' @param ... Other arguments to pass to the plot function.
#' @keywords internal
#' 
scatterplotAPC <- function(mat, ages, years, plotAge = TRUE, plotYear = TRUE, 
                           plotCohort  = TRUE, zeroLine  = TRUE, ...) {
  nAges <- length(ages)
  nYears <- length(years)  
  cohorts <- (years[1] - ages[nAges]):(years[nYears] - ages[1])
  nCohorts <- length(cohorts)
  
  mat <- as.matrix(mat)
  if ( nrow(mat) != nAges ||  ncol(mat) != nYears) {
    stop( "Mismatch between the dimensions of the plottin data and the 
          number of years or ages")
  }
  rownames(mat) <- ages
  colnames(mat) <- years
  data <- (reshape2::melt(mat, value.name = "y", varnames = c("x", "t")))
  x    <- NULL #hack to remove note in CRAN check
  data <- transform(data, c = t - x) 
  
  N <- plotAge + plotYear + plotCohort
  if (N > 0) par(mfrow = c(1, N))
  
  if (plotAge) {
    plot(data$x, data$y, type = "p", xlab = "age", ...)
    if (zeroLine) 
      abline(h = 0)
  }
  if (plotYear) {
    plot(data$t, data$y, type = "p", xlab = "calendar year", ...)
    if (zeroLine) 
      abline(h = 0)
  }
  
  #cohort 
  if (plotCohort) {
    plot(data$c, data$y, type = "p", xlab = "year of birth", ...)
    if (zeroLine) 
      abline(h = 0)    
  }   
}
