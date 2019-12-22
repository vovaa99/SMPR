library("MASS")
options(digits=22)
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


getLDFClassificator <- function(Prob = c(1),Prior = c(1),means,vars)
{
  n <- dim(means)[1]
  funcs <- list()
  
  makefunc <- function(i) {
    force(i)
    function(X)
    { 
      res <- log(Prob[i] * Prior[i])
      l <- length(X)
      a <- 
        (-1/2)*(
          t(as.vector(means[,i])) %*% solve(vars) %*% (as.vector(means[,i]))
        
      )
      b <- 
        t(X) %*% solve(vars) %*%  (as.vector(means[,i]))
      #print(b)
        
      res <- res + a + b
      
      #res <- Prob[i] * Prior[i]
      #l <- length(X)
      #chisl <- exp(
      #  (-1/2)*(
      #    t(X-as.vector(means[,i])) %*% solve(vars) %*% (X-as.vector(means[,i]))
     #   )
      #)
      #res <- res * chisl/((2*pi) * det(vars)^(1/2))
      
      return(res)
    }
  }
  for(i in seq(n))
  {
    funcs[[i]] <- makefunc(i)
  }
  
  solvingFunc <- function(X)
  {
    results <- vector()
    for (i in seq(n))
    {
      results[i] <- funcs[[i]](X)
    }
    
    return(which.max(results))
  }
  return(solvingFunc)
}

getRisk <- function(mu1, mu2, sigma) {
  mah <- (mu1 - mu2) %*% solve(sigma) %*% t(mu1 - mu2)
  mah <- mah * -0.5
  res <- gausian(mah, 0, 1)
}

gausian <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}

n <- 300
sigma1 <- matrix(c(5,0, 0, 5), 2, 2)

mu1 <- c(5,20)
mu2 <- c(15, 15)

xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma1)

plotxmin <- min(xy1[,1], xy2[,1]) - 1
plotymin <- min(xy1[,2], xy2[,2]) - 1
plotxmax <- max(xy1[,1], xy2[,1]) + 1
plotymax <- max(xy1[,2], xy2[,2]) + 1
plot(NULL, type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="Линейный дискриминат Фишера")

points(xy1, pch=21, col=colors[1], bg=colors[1])
points(xy2, pch=21, col=colors[2], bg=colors[2])

m1 <- matrix(c(mean(xy1[,1]),mean(xy1[,2])),1,2)
m2 <- matrix(c(mean(xy2[,1]),mean(xy2[,2])),1,2)
means <- matrix(c(m1,m2),length(m1),2)
sigma1 <- matrix(c(var(xy1[,1]),var(xy1[,2]),var(xy2[,1]),var(xy2[,2])),2,2)

func <- getFunc(sigma1, m1, m2)

x <- seq(plotxmin-5, plotxmax+5, len = 500)
lines(x, func(x), lwd = 2.5, type="l", col = 'red')
lines(c(m1[1], m2[1]), c(m1[2], m2[2]), col = 'gray', lwd = 2)


LDFClassificator <- getLDFClassificator(rep(1,length(sigma1)),rep(2,length(sigma1)),means = means, vars = sigma1)

minx <- min(xy1[,1], xy2[,1]) - 1
miny <- min(xy1[,2], xy2[,2]) - 1
maxx <- max(xy1[,1], xy2[,1]) + 1
maxy <- max(xy1[,2], xy2[,2]) + 1
xstep <- (maxx-minx)/80
ystep <- (maxy-miny)/42
i <- minx
while(i <= maxx)
{ 
  j <- miny
  while(j <= maxy)
  {
    xy <- c(i,j)
    #points(xy[1],xy[2], col=colors[LDFClassificator(xy)])
    
    j <- j+ystep
  }
  i <- i+xstep
}

risk <- getRisk(m1, m2, sigma1)
text(plotxmin,plotymin-1, sprintf("risk = %s", risk), adj = c( 0, -1 ), col = "blue" )

