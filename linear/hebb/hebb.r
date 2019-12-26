# Кусочно-линейную функцию потерь для Хебба
hebbLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  return (max(-mi, 0))
}
# правило Хебба для весов
hebbUpd <- function(xi, yi, w, eta) {
  nextW <- w + eta * yi * xi
  return (nextW)
}

getHEBBClassificator <- function(dat)
{
  resHEBB <- stgrad(dat, loss = hebbLoss, upd = hebbUpd, lwd = 1, col = 'lightgreen', xmin = plotxmin, xmax = plotxmax)
  drawLine(resHEBB, xmin = plotxmin, xmax = plotxmax, lwd = 2, col = 'red')
  getValue <- function(x) {
    sigmoid <- function(z) {
      return (1 / (1 + exp(-z)))
    }
    return ( sigmoid(c(crossprod(resHEBB, c(x[1], x[2], -1))) * -1) - sigmoid(c(crossprod(resHEBB, c(x[1], x[2], -1))) * 1) )
  }
  
  solvingFunc <- function(X)
  {
    pp <- getValue(X)
    if (pp > 0) 
    {
      return(1)
    }
    else 
    {
      return(2)
    }
  }
  return(solvingFunc)
}

n <- 100
m <- 100

sigma1 <- matrix(c(5, 0, 0, 5), 2, 2)
sigma2 <- matrix(c(1, 0, 0, 5), 2, 2)

mu1 <- c(3, 1)
mu2 <- c(8, 5)

#xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
#xc2 <- mvrnorm(n=m, mu = mu2, Sigma = sigma2)

dat <- rbind(xc1, xc2)
#dat <- normalizeDataMiniMax(dat)
dat <- normalizeDataZScaling(dat)
# random wj
dat <- cbind(dat, rep(-1, n+m))
# classes
dat <- cbind(dat, c(rep(-1, n), rep(1, m)))

plotxmin <- min(dat[,1], dat[,1]) - 0.3
plotxmax <- max(dat[,1], dat[,1]) + 0.3
plotymin <- min(dat[,2], dat[,2]) - 0.5
plotymax <- max(dat[,2], dat[,2]) + 0.5
plot(NULL, type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="HEBB")

points(dat, pch=21, col=colors[ifelse(dat[,4] == -1, 1, 2)], bg=colors[ifelse(dat[,4] == -1, 1, 2)])

HEBBClassificator <- getHEBBClassificator(dat)

library(plotrix)
for (i in seq(len=50, from = plotxmin, to = plotxmax)) {
for (j in seq(len=50, from = plotymin, to = plotymax)) {
classnum <- HEBBClassificator(c(i,j))
draw.circle(i, j, radius = 0.005, col = 0, border = colors[classnum])
}
}