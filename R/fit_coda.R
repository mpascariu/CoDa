
#' Fit CoDa Mortality Model
#' 
#' Fit Compositional Data (CoDa) model for forecasting the life table 
#' distribution of deaths. CoDa is a Lee-Carter type model. A key difference 
#' between the Lee-Carter (1992) method and the Compositional Data (CoDa) model is that the 
#' former fits and forecasts the death rates (mx) while the latter is based on the 
#' life table death distribution (dx). See Bergeron-Boucher et al. (2017) for a 
#' detail description and mathematical formulation.
#' 
#' @param dx Matrix containing mortality data (dx) with ages as row and time as column.
#' @param x Vector of input ages (optional). Used to label the output objects and plots. 
#' @param y Vector of input years (optional). Used to label the output objects and plots. 
#' @return An object of class \code{coda}
#' @seealso \code{\link{predict.coda}}
#' @references 
#' \enumerate{
#' \item{Bergeron-Boucher M-P., Canudas-Romo V., Oeppen J. and Vaupel W.J. 2017. 
#' \href{http://doi.org/10.4054/DemRes.2017.37.17}{
#' Coherent forecasts of mortality with compositional data analysis.}
#' Demographic Research, Volume 17, Article 17, Pages 527--566.}
#' \item{Oeppen, J. 2008. Coherent forecasting of multiple-decrement life tables: 
#' A test using Japanese cause of death data. Paper presented at the 
#' European Population Conference 2008, Barcelona, Spain, July 9-12, 2008.}
#' \item{Aitchison, J. 1986. 
#' \href{http://www.leg.ufpr.br/lib/exe/fetch.php/pessoais:abtmartins:a_concise_guide_to_compositional_data_analysis.pdf}{
#' The Statistical Analysis of Compositional Data.} 
#' London: Chapman and Hall. 2015.}
#' \item{Ronald D. Lee and Lawrence R. Carter. 1992. 
#' \href{http://doi.org/10.1080/01621459.1992.10475265}{Modeling and Forecasting U.S. Mortality}, 
#' Journal of the American Statistical Association, 87:419, 659--671.}
#' }
#' @examples
#' # Fit CoDa model
#' M <- coda(CoDa.data)
#' summary(M)
#' 
#' # Forecast life expectancy
#' P <- predict(M, h = 20)
#' 
#' @export
#' 
coda <- function(dx, x = NULL, y = NULL){
  input <- c(as.list(environment()))
  coda.input.check(input)
  if (is.null(x)) x <- 1:nrow(dx)
  if (is.null(y)) y <- 1:ncol(dx)
  
  close.dx  <- unclass(acomp(t(dx))) # data close
  ax        <- geometricmeanCol(close.dx) # geometric mean
  close.ax  <- ax/sum(ax)
  dxc       <- sweep(close.dx, 2, close.ax, "/") # centering
  close.dxc <- dxc/rowSums(dxc)
  clr_dxc   <- clr(close.dxc) # clr
  
  # SVD: bx and kt
  par <- svd(clr_dxc, nu = 1, nv = 1)
  U   <- par$u
  V   <- par$v
  S   <- diag(par$d)
  bx  <- V[, 1]
  kt  <- S[1, 1] * U[, 1]
  
  var <- cumsum((par$d)^2/sum((par$d)^2)) # variability
  cf  <- list(ax = as.numeric(close.ax), bx = as.numeric(bx), kt = as.numeric(kt))
  clr.proj.fit <- matrix(kt, ncol = 1) %*% bx # projections
  BK.proj.fit  <- unclass(clrInv(clr.proj.fit)) # Inv clr
  proj.fit     <- sweep(BK.proj.fit, 2, close.ax, FUN = "*") # add geometric mean
  fit          <- t(proj.fit/rowSums(proj.fit))
  resid        <- dx - fit
  dimnames(fit) = dimnames(resid) = dimnames(dx) <- list(x, y)
  
  out <- list(fitted = fit, coefficients = cf, residuals = resid, 
              input = input, x = x, y = y)
  out <- structure(class = 'coda', out)
  out$call <- match.call()
  return(out)
}


#' Validate input values
#' 
#' @param X A list with input arguments provided in \code{\link{coda}} function
#' @keywords internal
#' 
coda.input.check <- function(X) {
  # Validate the other arguments
  with(X, {
    if (any(dx < 0)) {
      stop("'dx' contains negative values. ",
           "The compositions must always be positive or equal to zero.", 
           call. = F)
    }
    if (any(is.na(dx))) {
      stop("'dx' contains NA values. ",
           "coda() doesen't know how to deal with these yet.", 
           call. = F)
    }
    if (any(is.na(dx))) {
      stop("'x' contains NA values", call. = F)
    }
    if (any(is.na(y))) {
      stop("'y' contains NA values", call. = F)
    }
    if (any(is.na(x))) {
      stop("'x' contains NA values", call. = F)
    }
    if ((!is.null(x)) & dim(dx)[1] != length(x)) {
      stop("The length of 'x' is not equal to the number or rows in 'dx'.", call. = F)
    }
    if ((!is.null(y)) & dim(dx)[2] != length(y)) {
      stop("The length of 'y' is not equal to the number or columns in 'dx'.", call. = F)
    }
  })
}


# S3 ----------------------------------------------

#' Residuals of the CoDa Mortality Model
#' @inheritParams summary.coda
#' @keywords internal
#' @export
residuals.coda <- function(object, ...){
  out <- structure(class = 'residuals.coda', object$residuals)
  return(out)
}

#' Print coda
#' @param x An object of class \code{"coda"}
#' @param ... Further arguments passed to or from other methods.
#' @keywords internal
#' @export
#' 
print.coda <- function(x, ...) {
  cat('\nCompositional Data Model fit - CoDa (Oeppen 2008)')
  cat('\nModel with predictor: clr d[x] = a[x] + b[x]k[t]')
  cat('\nCall: '); print(x$call)
  cat('\nYears in fit: ', paste(range(x$y), collapse = ' - '))
  cat('\nAges in fit: ', paste(range(x$x), collapse = ' - '))
  cat('\n')
}

#' Summary coda
#' @param object An object of class \code{"coda"}
#' @inheritParams print.coda
#' @keywords internal
#' @export
#' 
summary.coda <- function(object, ...) {
  axbx <- data.frame(ax = object$coefficients$ax, 
                     bx = object$coefficients$bx,
                     row.names = object$x)
  kt <- data.frame(kt = object$coefficients$kt)
  out = structure(class = 'summary.coda', 
                  list(A = axbx, K = kt, call = object$call,
                       y = object$y, x_ = object$x))
  return(out)
}


#' Print summary.coda
#' @param x An object of class \code{"summary.coda"}
#' @inheritParams print.coda
#' @keywords internal
#' @export
#' 
print.summary.coda <- function(x, ...){
  cat('\nCompositional Data Model fit - CoDa (Oeppen 2008)')
  cat('\nModel with predictor: clr d[x] = a[x] + b[x]k[t]\n')
  cat('\nCoefficients:\n')
  A <- head_tail(x$A, digits = 5, hlength = 6, tlength = 6)
  K <- head_tail(data.frame(. = '|', y = as.integer(x$y), kt = x$K),
                 digits = 5, hlength = 6, tlength = 6)
  print(data.frame(A, K))
  cat('\n')
}


#' Summary function - display head and tail in a single data.frame
#' The original code for this function was first written for 'psych' R package
#' here we have modified it a bit
#' @param x A matrix or data frame or free text
#' @param hlength The number of lines at the beginning to show
#' @param tlength The number of lines at the end to show
#' @param digits Round off the data to digits
#' @param ellipsis separate the head and tail with dots
#' @keywords internal
#' @author William Revelle (\email{revelle@@northwestern.edu})
#' @keywords internal
#' 
head_tail <- function(x, hlength = 4, tlength = 4, 
                      digits = 2, ellipsis = TRUE) {
  if (is.data.frame(x) | is.matrix(x)) {
    if (is.matrix(x)) x = data.frame(unclass(x))
    nvar <- dim(x)[2]
    dots <- rep("...", nvar)
    h    <- data.frame(head(x, hlength))
    t    <- data.frame(tail(x, tlength))
    
    for (i in 1:nvar) {
      if (is.numeric(h[1, i])) {
        h[i] <- round(h[i], digits)
        t[i] <- round(t[i], digits)
      } else {
        dots[i] <- NA
      }
    }
    out <- if (ellipsis) rbind(h, ... = dots, t) else rbind(h, t)
  }
  else {
    h   <- head(x, hlength)
    t   <- tail(x, tlength)
    out <- if (ellipsis) rbind(h, "...       ...", t) else as.matrix(rbind(h, t))
  }
  return(out)
}

