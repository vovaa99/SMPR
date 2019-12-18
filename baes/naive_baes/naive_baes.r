library("MASS")

getPyj <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}

getNaiveBayesFunc <- function(means,vars)
{
  n <- dim(means)[1]
  funcs <- list()
  Prob <- rep(0.5,n)
  Prior <- rep(0.5,n)
  makefunc <- function(i) {
    force(i)
    function(X)
    { 
      res <- log(Prob[i] * Prior[i])
      
      for (j in seq(n)) {
        p <- getPyj(X[j], means[j,i],vars[j,i])
        res <- res + log(p)
      }
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
    #print (results)
    return(which.max(results))
  }
  return(solvingFunc)
}


n <- 300
sigma1 <- matrix(c(2, 0, 0, 2), 2, 2)
sigma2 <- matrix(c(1, 0, 0, 1), 2, 2)

mu1 <- c(-2,-2)
mu2 <- c(4,4)

xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xc2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)

plotxmin <- min(xc1[,1], xc2[,1]) - 1
plotymin <- min(xc1[,2], xc2[,2]) - 1
plotxmax <- max(xc1[,1], xc2[,1]) + 1
plotymax <- max(xc1[,2], xc2[,2]) + 1
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="Наивный нормальный байесовский классификатор")

colors <- c("red", "green")
points(xc1, pch=21, col=colors[1], bg=colors[1])
points(xc2, pch=21, col=colors[2], bg=colors[2])

m1 <- c(mean(xc1[,1]),mean(xc1[,2]))
m2 <- c(mean(xc2[,1]),mean(xc2[,2]))

d1 <- c(var(xc1[,1]),var(xc1[,2]))
d2 <- c(var(xc2[,1]),var(xc2[,2]))

l <- max(plotxmax - plotxmin, plotymax - plotymin)
x <- seq(plotxmin, plotxmax, l/50)
y <- seq(plotymin, plotymax, l/50)

means<-matrix(c(m1,m2),length(m1),2)
vars<-matrix(c(d1,d2),length(d1),2)

bayesSolvingFunc <- getNaiveBayesFunc(means,vars)

for (i in x) {
  for (j in y) {
    points(i, j, pch = 21, col = colors[bayesSolvingFunc(c(i,j))])
  }
}

