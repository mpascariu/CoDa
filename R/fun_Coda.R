
#' Fit CoDa model
#' 
#' Fit Compositional Data Analysis (CoDa) model for forecasting the life table 
#' distribution of deaths.
#' 
#' @param dx Female death rates matrix with ages as row and time as column
#' @param x Vector of input ages (optional) 
#' @param y Vector of input years (optional)
#' @return An object of class \code{CoDa}
#' @seealso \code{\link{predict.CoDa}}
#' @references 
#' \enumerate{
#' \item{Bergeron-Boucher, M-P., Canudas-Romo, V., Oeppen, J. and Vaupel, W.J. 2017. 
#' \href{http://doi.org/10.4054/DemRes.2017.37.17}{
#' Coherent forecasts of mortality with compositional data analysis.}
#' Demographic Research, Volume 17, Article 17, Pages 527--566.}
#' \item{Aitchison, J. 1986. 
#' \href{http://www.leg.ufpr.br/lib/exe/fetch.php/pessoais:abtmartins:a_concise_guide_to_compositional_data_analysis.pdf}{
#' The Statistical Analysis of Compositional Data.} 
#' London: Chapman and Hall. 2015.}
#' }
#' @examples
#' # Fit CoDa model
#' fit_CoDa <- CoDa(CoDa.data, x = 0:110, y = 1960:2014)
#' ls(fit_CoDa)
#' summary(fit_CoDa)
#' 
#' # Predict life expectancy 20 years in the future using CoDa model
#' pred_Coda <- predict(fit_CoDa, n = 20)
#' pred_Coda
#' 
#' @export
#' 
CoDa <- function(dx, x = NULL, y = NULL){
  input <- c(as.list(environment()))
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
  out <- structure(class = 'CoDa', out)
  out$call <- match.call()
  return(out)
}


#' Predict empirical distribution of deaths using CoDa model
#' 
#' @param object CoDa object
#' @param n Number of years to be forecast in the future
#' @param order A specification of the non-seasonal part of the ARIMA model: 
#'  the three components (p, d, q) are the AR order, the degree of differencing, 
#'  and the MA order. If \code{order = NULL}, the ARIMA order will be estimated 
#'  automatically using the KPPS algorithm.
#' @param include.drift Logical. Should the ARIMA model include a linear drift term?
#'  If \code{include.drift = NULL}, the model will be estimated automatically.
#' @param method Fitting method: maximum likelihood or minimize conditional 
#'  sum-of-squares. Options to use:
#'  conditional-sum-of-squares (\code{"CSS-ML"}), maximum likelihood (\code{"ML"}) 
#'  and \code{"CSS"}.
#' @param ci Confidence level for prediction intervals.
#' @param ... Additional arguments to be passed to \code{\link{Arima}}
#' @param jumpchoice Method used for computation of jumpchoice. 
#'  Possibilities: \code{"actual"} (use actual rates from final year) 
#'  and \code{"fit"} (use fitted rates).
#' @return Results
#' @export
#' 
predict.CoDa <- function(object, n, order = NULL,
                         include.drift = NULL,
                         method = "ML", ci = c(80, 95), 
                         jumpchoice = c("actual", "fit"), ...){
  dx  <- t(object$input$dx)
  bop <- max(object$y) + 1
  eop <- bop + n - 1
  fcy <- bop:eop
  jc  <- jumpchoice[1]
  cf  <- coef(object)
  ax  <- cf$ax
  bx  <- cf$bx
  kt  <- cf$kt
  
  # forecast kt; ax and bx are time independent.
  ts_auto = auto.arima(kt)
  if (is.null(order)) order = arimaorder(ts_auto)
  if (is.null(include.drift)) include.drift = any(names(coef(ts_auto)) %in% "drift")
  
  tsm <- Arima(y = kt, order = order, include.drift = include.drift, method = method, ...)
  tsf <- forecast(tsm, h = n, level = ci)  # time series forecast
  fkt <- data.frame(tsf$mean, tsf$lower, tsf$upper) # forecast kt
  fdx <- compute_dx(input = dx, kt = fkt, ax = ax, bx = bx, # forecast dx
                    fit = t(fitted(object)), years = fcy, jumpchoice = jc)
  colnames(fkt) = names(fdx) <- c('mean', paste0('L', ci), paste0('U', ci))
  out <- list(predicted.values = fdx, y = fcy, kt = fkt, ts.model = tsm)
  out <- structure(class = 'predict.CoDa', out)
  return(out)
}

#' @keywords internal
#' 
compute_dx <- function(input, kt, ax, bx, fit, years, jumpchoice) {
  
  if (class(kt) == 'data.frame') {
    pred <- list()
    for (i in 1:ncol(kt)) {
      pred[[i]] <- compute_dx(input, kt = kt[, i], ax, bx, fit, years, jumpchoice)
      colnames(pred[[i]]) <- years
    }
    return(pred)
    
  } else {
    dx_nrow  <- nrow(input)
    close.dx <- acomp(input)
    jump_off <- as.numeric(close.dx[dx_nrow, ]/fit[dx_nrow, ])
    clr_proj <- matrix(kt, ncol = 1) %*% bx
    bk_      <- unclass(clrInv(clr_proj))
    dx_      <- sweep(bk_, 2, ax, FUN = "*")
    if (jumpchoice == 'actual') dx_ <- sweep(dx_, 2, jump_off, FUN = "*")
    dx_ <- t(dx_/rowSums(dx_))
    rownames(dx_) <- colnames(input)
    return(dx_)
  }
} 


