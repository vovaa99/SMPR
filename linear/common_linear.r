library(MASS)

colors <- c("green", "blue")
# Для нормализации выборки
normalizeDataMiniMax <- function(x) {
  n <- dim(x)[1]
  m <- dim(x)[2]
  res <- matrix(NA, n, m)
  for (i in seq(m)) {
    minVal <- min(x[,i])
    maxVal <- max(x[,i])
    res[,i] <- (x[,i] - minVal)/(maxVal - minVal)
  }
  return(res)
}

normalizeDataZScaling <-function(xl){
  n <- dim(xl)[1]
  m <- dim(xl)[2]
  res <- matrix(NA, n, m)
  for(i in seq(m)) 
  {
    res[, i] <-(xl[, i] - mean(xl[, i])) / sd(xl[, i]) 
  }
  return (res)
}

drawLine <- function(w, xmin = -2, xmax = -2, ...) {
  x <- seq(xmin, xmax, len = 100)
  f <- function(x) {
    return( - x*w[1]/w[2] + w[3]/w[2] )
  }
  y <- f(x)
  lines(x, y, type="l", ...)
}

## Стохастический градиент
stgrad <- function(xl, eta = 1, lambda = 1/6, eps = 1e-5, loss, upd, ...) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- rep(0.5, n)
  
  Q <- 0
  Qprev <- Q
  
  # Начальное значение Q
  for (i in seq(l)) {
    xi <- xl[i, 1:n]
    yi <- xl[i, n+1]
    
    Q <- Q + loss(xi, yi, w)
  }
  
  iter <- 0
  repeat {
    #ограничение количества повторов
    iter <- iter + 1
    if (iter > 100) {
      break
    }
    
    mis <- array(dim = l)
    for (i in seq(l)) {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      mis[i] <- crossprod(w, xi) * yi
    }
    
    errorIndexes <- which(mis <= 0)
    if (length(errorIndexes) == 0) {
      break
    }
    
    i <- sample(errorIndexes, 1)
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    
    ex <- loss(xi, yi, w)
    
    w <- upd(xi, yi, w, eta)
    
    Q <- (1 - lambda) * Q + lambda * ex
    
    if (abs(Q - Qprev) < eps) {
      break
    }
    Qprev <- Q
    
    #drawLine(w, ...)
  }
  
  return(w)
}