library("MASS")

colors <- c("green", "red")
getFunc <- function(sigma1, mu1, mu2) {
  d1 <- det(sigma1)
  invs1 <- solve(sigma1)
  
  b <- invs1 %*% t(mu1 - mu2)
  
  D <- b[1, 1] # x
  E <- b[2, 1] # y
  mu <- (mu1 + mu2)
  G <- c(mu %*% b) / 2
  
  func <- function(x) {
    -x*D/E + G/E
  }
  
  return(func)
}

n <- 300
sigma1 <- matrix(c(5,0, 0, 5), 2, 2)

mu1 <- c(0, 2)
mu2 <- c(5, 20)

xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xc2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma1)

plotxmin <- min(xc1[,1], xc2[,1]) - 1
plotymin <- min(xc1[,2], xc2[,2]) - 1
plotxmax <- max(xc1[,1], xc2[,1]) + 1
plotymax <- max(xc1[,2], xc2[,2]) + 1
plot(NULL, type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="Линейный дискриминат Фишера")


points(xc1, pch=21, col=colors[1], bg=colors[1])
points(xc2, pch=21, col=colors[2], bg=colors[2])

m1 <- matrix(c(mean(xc1[,1]),mean(xc1[,2])),1,2)
m2 <- matrix(c(mean(xc2[,1]),mean(xc2[,2])),1,2)

sigma1 <- matrix(c(var(xc1[,1]),var(xc1[,2]),var(xc2[,1]),var(xc2[,2])),2,2)

func <- getFunc(sigma1, m1, m2)

x <- seq(plotxmin-5, plotxmax+5, len = 500)
lines(x, func(x), lwd = 2.5, type="l", col = 'red')
lines(c(mu1[1], mu2[1]), c(mu1[2], mu2[2]), col = 'gray', lwd = 2)
