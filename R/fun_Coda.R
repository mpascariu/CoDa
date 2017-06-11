#' Function to fit death rates using the CoDa model
#' 
#' @param dx female death rates matrix with ages as row and time as column
#' @param x Vector of input ages (optional) 
#' @param t Vector of input years (optional)
#' @return An object of class \code{CoDa}
#' @seealso \code{\link{predict.CoDa}}
#' @examples
#' library(CoDa)
#' 
#' # Fit CoDa model
#' fit_CoDa <- CoDa(edd, x = 0:110, t = 1960:2014)
#' ls(fit_CoDa)
#' summary(fit_CoDa)
#' 
#' # Predict life expectancy 20 years in the future using CoDa model
#' pred_Coda <- predict(fit_CoDa, n = 20)
#' 
#' @importFrom compositions acomp geometricmeanCol clr clrInv
#' @export
#' 
CoDa <- function(dx, x = NULL, t = NULL){
  input <- c(as.list(environment()))
  dx_input <- t(dx)
  
  #data close
  close.dx <- acomp(dx_input)
  #geometric mean
  ax <- geometricmeanCol(close.dx)
  #centering
  dx.cent <- close.dx - ax
  #clr
  clr.cent <- clr(dx.cent)
  # SVD: bx and kt
  par <- svd(clr.cent, nu = 1, nv = 1)
  U   <- par$u
  V   <- par$v
  S   <- diag(par$d)
  bx  <- V[, 1]
  kt  <- S[1, 1] * U[, 1]
  variability <- cumsum((par$d)^2/sum((par$d)^2))
  coef <- list(ax = as.numeric(ax), 
               bx = as.numeric(bx), 
               kt = as.numeric(kt))
  
  #projections
  clr.proj.fit <- matrix(kt, ncol = 1) %*% bx
  #Inv clr
  BK.proj.fit <- clrInv(clr.proj.fit)
  #Add geometric mean
  fit <- t(unclass((BK.proj.fit + ax)))
  resid <- dx - fit
  dimnames(fit) = dimnames(resid) = dimnames(dx)
  
  out <- structure(class = 'CoDa', 
                   list(fitted = fit, coefficients = coef, 
                        residuals = resid, input = input, 
                        call = match.call()))
  return(out)
}



#' Predict empirical distribution of deaths using CoDa model
#' 
#' @param object CoDa object
#' @param n Number of years to be forcast in the future
#' @param order A specification of the non-seasonal part of the ARIMA model: 
#'  the three components (p, d, q) are the AR order, the degree of differencing, 
#'  and the MA order.
#' @param include.drift Logical. Should the ARIMA model include a linear drift term?
#'  Default: \code{TRUE}.
#' @param method Fitting method: maximum likelihood or minimize conditional 
#'  sum-of-squares. Options to use:
#'  conditional-sum-of-squares (\code{"CSS-ML"}), maximum likelihood (\code{"ML"}) 
#'  and \code{"CSS"}.
#' @param ci Confidence level for prediction intervals.
#' @param ... Additional arguments to be passed to \code{\link{Arima}}
#' @return Results
#' @importFrom forecast forecast Arima
#' @export
#' 
predict.CoDa <- function(object, n, order = c(0,1,0),
                         include.drift = TRUE,
                         method = "ML", ci = c(80, 95), ...){
  dx_input <- t(object$input$dx)
  bop <- max(as.numeric(rownames(dx_input))) + 1
  eop <- bop + n - 1
  fc_years = bop:eop
  
  ax = object$coefficients$ax
  bx = object$coefficients$bx
  kt = object$coefficients$kt
  
  # forecast kt
  ts_model_fit <- Arima(y = kt, order = order, 
                        include.drift = include.drift, 
                        method = method)
  tsf <- forecast(ts_model_fit, h = n, level = ci)  # tsf = time series forecast
  
  kt_forecast <- data.frame(tsf$mean, tsf$lower, tsf$upper)
  c_names <- c('mean', paste0('L', ci), paste0('U', ci))
  colnames(kt_forecast) <- c_names
  
  #projections
  clr.proj.fit <- matrix(kt, ncol = 1) %*% bx
  fitted_dx    <- acomp(ax + clrInv(clr.proj.fit))
  forecast_dx  <- compute_dx(input = dx_input, 
                             kt = kt_forecast, ax, bx,
                             fit = fitted_dx,
                             years = fc_years)
  names(forecast_dx) <- c_names
  
  out <- structure(class = 'predict.CoDa', 
                   list(predicted.values = forecast_dx, 
                        kt = kt, ts_model = ts_model_fit))
  return(out)
}

#' @keywords internal
#' 
compute_dx <- function(input, kt, ax, bx, fit, years) {
  
  if (class(kt) == 'data.frame') {
    pred <- list()
    for (i in 1:ncol(kt)) {
      pred[[i]] <- compute_dx(input, kt = kt[, i], ax, bx, fit, years)
      colnames(pred[[i]]) <- years
    }
    return(pred)
  } else {
    
    dx_nrow  = nrow(input)
    close.dx = acomp(input)
    jump_off = acomp(close.dx[dx_nrow, ]) - fit[dx_nrow, ]
    clr_proj = matrix(kt, ncol = 1) %*% bx
    dx_      = acomp(ax + clrInv(clr_proj)) + jump_off
    colnames(dx_) = colnames(input)
    out      = t(dx_)
    return(out)
  }
}

