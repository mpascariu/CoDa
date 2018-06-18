
#' @keywords internal
#' @export
#' 
print.CoDa <- function(x, ...) {
  cat('\nCompositional Data Model fit - CoDa (Oeppen 2008)')
  cat('\nModel with predictor: clr d[x] = a[x] + b[x]k[t]')
  cat('\nCall: '); print(x$call)
  cat('\nYears in fit: ', paste(range(x$y), collapse = ' - '))
  cat('\nAges in fit: ', paste(range(x$x), collapse = ' - '))
  cat('\n')
}

#' @keywords internal
#' @export
#' 
summary.CoDa <- function(object, ...) {
    axbx <- data.frame(ax = object$coefficients$ax, 
                       bx = object$coefficients$bx,
                       row.names = object$x)
    kt <- data.frame(kt = object$coefficients$kt)
    out = structure(class = 'summary.CoDa', 
                    list(A = axbx, K = kt, call = object$call,
                         y = object$y, x_ = object$x))
    return(out)
}


#' @keywords internal
#' @export
#' 
print.summary.CoDa <- function(x, ...){
  cat('\nCompositional Data Model fit - CoDa (Oeppen 2008)')
  cat('\nModel with predictor: clr d[x] = a[x] + b[x]k[t]\n')
  
  cat('\nCoefficients:\n')
  A <- head_tail(x$A, digits = 5, hlength = 6, tlength = 6)
  K <- head_tail(data.frame(. = '|', y = as.integer(x$y), kt = x$K),
                  digits = 5, hlength = 6, tlength = 6)
  print(data.frame(A, K))
  cat('\n')
}

#' @keywords internal
#' @export
#' 
print.predict.CoDa <- function(x, ...) {
  cat('\nCompositional Data Model forecast')
  cat('\nAges in forecast: ', paste(range(x$years), collapse = ' - '))
  cat('\nTime series model (kt):', arima.string1(x$ts.model, padding = TRUE))
  cat('\n')
}

# ----------------------------------------------

#' Summary function - display head and tail in a single data.frame
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

#' @keywords internal
#' 
arima.string1 <- function(object, padding = FALSE) {
  order  <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  nc     <- names(coef(object))
  result <- paste0("ARIMA(", order[1], ",", order[2], ",", order[3], ")")
  
  if (order[7] > 1 & sum(order[4:6]) > 0) 
    result <- paste0(result, "(", order[4], ",", order[5], 
                    ",", order[6], ")[", order[7], "]")
  if (!is.null(object$xreg)) {
    if (NCOL(object$xreg) == 1 & is.element("drift", nc)) 
      result <- paste(result, "with drift        ")
    else result <- paste("Regression with", result, "errors")
  }
  else {
    if (is.element("constant", nc) | is.element("intercept", nc)) 
      result <- paste(result, "with non-zero mean")
    else if (order[2] == 0 & order[5] == 0) 
      result <- paste(result, "with zero mean    ")
    else result <- paste(result, "                  ")
  }
  if (!padding) 
    result <- gsub("[ ]*$", "", result)
  return(result)
}
