#' Life Table starting from dx
#' @keywords internal
#' 
LifeT_dx <- function(dx, radix = 1e+05){
  n  <- 1
  a  <- c(0.06, rep(0.5, ncol(dx) - 1)) 
  dx <- dx*radix
  qx = lx = Lx = Tx <- matrix(NA, nrow(dx), ncol(dx))
  
  for (j in 1:nrow(dx)) {
    lx[j, 2:ncol(dx)] <- radix - cumsum(dx[j, 1:(ncol(dx) - 1)])
    Lx[j, ] <- (lx[j, ]*n) - (dx[j, ]*(n - a))
    
    for (i in 1:ncol(dx)) {
      Tx[j, i] <- sum(Lx[j, i:ncol(Lx)], na.rm = TRUE)
    }
    Tx[Tx == 0] <- NA
  }
  mx <- dx/Lx
  
  for (j in 1:nrow(mx)) {
    qx[j, ] <- mx[j, ] * n / (1 + (n - a) * mx[j, ])
    qx[, ncol(qx)] <- 1
    qx[qx > 1] <- 1
  }
  ex <- Tx/lx
  
  dimnames(qx) = dimnames(lx) = dimnames(Lx) = 
    dimnames(ex) = dimnames(mx) <- dimnames(dx)
  return(list(mx = mx, qx = qx, dx = dx, lx = lx, 
              Lx = Lx, Tx = Tx, ex = ex))
}

#' Life Table starting from mx 
#' @keywords internal
#' 
LifeT_mx <- function(mx, radix = 1e+05){
  n  <- 1
  a  <- c(0.06, rep(0.5, ncol(mx) - 1))
  qx = lx = Lx = Tx <- matrix(NA, nrow(mx), ncol(mx))
  
  for (j in 1:nrow(mx)) {
    qx[j, ] <- mx[j, ] * n / (1 + (n - a) * mx[j, ])
    qx[, ncol(qx)] <- 1
    qx[qx > 1] <- 1
    px <- 1 - qx
    lx[j, ] <- radix * c(1, cumprod(px[j, ])[1:(length(lx[j, ]) - 1)])
    dx <- lx*qx
    Lx[j, ] <- (lx[j, ]*n) - (dx[j, ]*(n - a))
    
    for (i in 1:ncol(mx)) {
      Tx[j, i] <- sum(Lx[j, i:ncol(Lx)], na.rm = TRUE)
    }
    Tx[Tx == 0] <- NA
  }
  ex <- Tx/lx
  dimnames(qx) = dimnames(dx) = dimnames(lx) = dimnames(Lx) = 
    dimnames(Tx) = dimnames(ex) <- dimnames(mx)
  return(list(mx = mx, qx = qx, dx = dx, lx = lx, 
              Lx = Lx, Tx = Tx, ex = ex))
}

