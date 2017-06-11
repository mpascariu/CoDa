
#' @keywords internal
#' @export
#' 
print.CoDa <- function(x, ...) {
  cat('\nCompositional Data Model fit - CoDa (Oeppen 2008)')
  cat('\nModel with predictor: clr d[x] = a[x] + b[x]k[t]')
  cat('\nCall: '); print(x$call)
  cat('\nYears in fit: ', paste(range(x$input$t), collapse = ' - '))
  cat('\nAges in fit: ', paste(range(x$input$x), collapse = ' - '))
}

#' @keywords internal
#' @export
#' 
summary.CoDa <- function(object, ...) {
    axbx <- data.frame(ax = object$coefficients$ax, 
                       bx = object$coefficients$bx,
                       row.names = object$input$x)
    kt <- data.frame(kt = object$coefficients$kt)
    out = structure(class = 'summary.CoDa', 
                    list(A = axbx, K = kt, call = object$call,
                         t = object$input$t, x_ = object$input$x))
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
  K <- head_tail(data.frame(. = '|', t = as.integer(x$t), kt = x$K),
                  digits = 5, hlength = 6, tlength = 6)
  print(data.frame(A, K))
}




#' Summary function - display head and tail in a single data.frame
#' The code for this function was first written for 'psych' R package
#' @importFrom utils head tail
#' @author William Revelle (\email{revelle@@northwestern.edu})
#' @keywords internal
#' 
head_tail <- function(x, hlength = 4, tlength = 4, 
                      digits = 2, ellipsis = TRUE) {
  if (is.data.frame(x) | is.matrix(x)) {
    if (is.matrix(x)) 
      x <- data.frame(unclass(x))
    nvar <- dim(x)[2]
    dots <- rep("...", nvar)
    h <- data.frame(head(x, hlength))
    t <- data.frame(tail(x, tlength))
    for (i in 1:nvar) {
      if (is.numeric(h[1, i])) {
        h[i] <- round(h[i], digits)
        t[i] <- round(t[i], digits)
      }
      else {
        dots[i] <- NA
      }
    }
    if (ellipsis) {
      head.tail <- rbind(h, ... = dots, t)
    }
    else {
      head.tail <- rbind(h, t)
    }
  }
  else {
    h <- head(x, hlength)
    t <- tail(x, tlength)
    if (ellipsis) {
      head.tail <- rbind(h, "...       ...", t)
    }
    else {
      head.tail <- rbind(h, t)
      head.tail <- as.matrix(head.tail)
    }
  }
  return(head.tail)
}
