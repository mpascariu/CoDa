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



#' Function to predict life expectancy using CoDa model
#' 
#' @param object CoDa object
#' @param n Number of years to be forcast in the future
#' @param ... ...
#' @return Results
#' @importFrom forecast forecast Arima
#' @export
#' 
predict.CoDa <- function(object, n, ...){
  with(object, 
       {
         dx_input <- t(input$dx)
         ax <- coefficients$ax
         bx <- coefficients$bx
         kt <- coefficients$kt
         nrow.dx <- nrow(dx_input)
         close.dx <- acomp(dx_input)
         
         # forecast kt
         order = c(0,1,0)
         test <- Arima(kt, order = order, include.drift = TRUE, method = "ML")
         fcst <- forecast(test, h = n)
         kt.fit <- fcst$fitted
         kt.for <- fcst$mean
         #kt.b <- replicate(5000, simulate(test, nsim=t, bootstrap=T), simplify = "array")
         #kt.for <- apply(kt.b, 1, function (kt.b) quantile(kt.b, prob=0.5, type=8))
         
         #projections
         clr.proj.fit <- matrix(kt, ncol = 1) %*% bx
         clr.proj.for <- matrix(c(kt, kt.for), ncol = 1) %*% bx
         
         #Inv clr
         BK.proj.fit <- clrInv(clr.proj.fit)
         BK.proj.for <- clrInv(clr.proj.for)
         BK <- t(BK.proj.for)
         
         #Add geometric mean
         proj.fit <- BK.proj.fit + ax
         proj.for <- BK.proj.for + ax
         
         #jump-off
         rows <- nrow.dx:nrow(proj.for)
         pert <- acomp(close.dx[nrow.dx, ]) - acomp(proj.for[nrow.dx, ])
         forecast.dx <- acomp(proj.for[rows,]) + pert
         forefit.dx <- proj.for
         forefit.dx[rows, ] <- forecast.dx
         
         #Life table
         LT <- LifeT_dx(unclass(forefit.dx))
         mx <- t(LT$mx)
         qx <- t(LT$qx)
         dx <- t(forefit.dx)
         lx <- t(LT$lx)
         ex <- t(LT$ex)
         
         bop <- min(as.numeric(rownames(dx_input)))
         eop <- max(as.numeric(rownames(dx_input))) + n
         d_names <- list(colnames(dx_input), bop:eop)
         dimnames(mx) = dimnames(qx) = dimnames(dx) = dimnames(lx) = 
           dimnames(ex) = dimnames(BK) <- d_names
         
         out <- list(mx = mx, qx = qx, dx = dx, lx = lx, 
                     ex = ex, BK = BK)
         
         return(out)
       })
}


