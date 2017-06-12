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
#' pred_Coda
#' 
#' @importFrom compositions acomp geometricmeanCol clr clrInv
#' @export
#' 
CoDa <- function(dx, x = NULL, t = NULL){
  input <- c(as.list(environment()))
  
  #data close
  close.dx <- unclass(acomp(t(dx)))
  #geometric mean
  ax <- geometricmeanCol(close.dx)
  close.ax <- ax/sum(ax)
  #centering
  dx.cent <- sweep(close.dx, 2, close.ax, "/")
  close.dx.cent <- dx.cent/rowSums(dx.cent)
  #clr
  clr.cent <- clr(close.dx.cent)
  # SVD: bx and kt
  par <- svd(clr.cent, nu = 1, nv = 1)
  U   <- par$u
  V   <- par$v
  S   <- diag(par$d)
  bx  <- V[, 1]
  kt  <- S[1, 1] * U[, 1]
  variability <- cumsum((par$d)^2/sum((par$d)^2))
  coef <- list(ax = as.numeric(close.ax), 
               bx = as.numeric(bx), 
               kt = as.numeric(kt))
  
  #projections
  clr.proj.fit <- matrix(kt, ncol = 1) %*% bx
  #Inv clr
  BK.proj.fit <- unclass(clrInv(clr.proj.fit))
  #Add geometric mean
  proj.fit <- sweep(BK.proj.fit, 2, close.ax, FUN = "*")
  fit <- t(proj.fit/rowSums(proj.fit))
  resid <- dx - fit
  dimnames(fit) = dimnames(resid) = dimnames(dx)
  
  out <- structure(class = 'CoDa', 
                   list(fitted = fit, coefficients = coef,
                        residuals = resid, input = input, 
                        call = match.call()))
  return(out)
}

# jumpchoice = c("fit", "actual")

#' Predict empirical distribution of deaths using CoDa model
#' 
#' @param object CoDa object
#' @param n Number of years to be forcast in the future
#' @param order A specification of the non-seasonal part of the ARIMA model: 
#'  the three components (p, d, q) are the AR order, the degree of differencing, 
#'  and the MA order. If \code{order = NULL}, the arima order will be estimated 
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
#' @importFrom forecast forecast Arima arimaorder auto.arima
#' @export
#' 
predict.CoDa <- function(object, n, order = NULL,
                         include.drift = NULL,
                         method = "ML", ci = c(80, 95), 
                         jumpchoice = c("actual", "fit"), ...){
  dx_input <- t(object$input$dx)
  bop      <- max(as.numeric(rownames(dx_input))) + 1
  eop      <- bop + n - 1
  fc_years <- bop:eop
  jump_choice <- jumpchoice[1]
  
  ax = object$coefficients$ax
  bx = object$coefficients$bx
  kt = object$coefficients$kt
  
  # forecast kt; ax and bx are time independent.
  ts_auto = auto.arima(kt)
  if (is.null(order)) order = arimaorder(ts_auto)
  if (is.null(include.drift)) include.drift = any(names(ts_auto$coef) %in% "drift")
  
  ts_model_fit <- Arima(y = kt, order = order, 
                        include.drift = include.drift, 
                        method = method, ...)
  tsf <- forecast(ts_model_fit, h = n, level = ci)  # tsf = time series forecast
  
  forecast_kt <- data.frame(tsf$mean, tsf$lower, tsf$upper)
  c_names <- c('mean', paste0('L', ci), paste0('U', ci))
  colnames(forecast_kt) <- c_names
  
  forecast_dx  <- compute_dx(input = dx_input, 
                             kt = forecast_kt, ax, bx,
                             fit = t(object$fitted),
                             years = fc_years,
                             jumpchoice = jump_choice)
  names(forecast_dx) <- c_names
  
  out <- structure(class = 'predict.CoDa', 
                   list(predicted.values = forecast_dx, years = fc_years,
                        kt = forecast_kt, ts.model = ts_model_fit))
  return(out)
}

#' @keywords internal
#' 
compute_dx <- function(input, kt, ax, bx, fit, years, jumpchoice) {
  
  if (class(kt) == 'data.frame') {
    pred <- list()
    for (i in 1:ncol(kt)) {
      pred[[i]] <- compute_dx(input, kt = kt[, i], ax, bx, 
                              fit, years, jumpchoice)
      colnames(pred[[i]]) <- years
    }
    return(pred)
    
  } else {
    dx_nrow  = nrow(input)
    close.dx = acomp(input)
    jump_off = as.numeric(close.dx[dx_nrow, ]/fit[dx_nrow, ])
    clr_proj = matrix(kt, ncol = 1) %*% bx
    
    bk_ <- unclass(clrInv(clr_proj))
    dx_ <- sweep(bk_, 2, ax, FUN = "*")
    if (jumpchoice == 'actual') dx_ <- sweep(dx_, 2, jump_off, FUN = "*")
    dx_ <- t(dx_/rowSums(dx_))
    rownames(dx_) <- colnames(input)
    return(dx_)
  }
} 

