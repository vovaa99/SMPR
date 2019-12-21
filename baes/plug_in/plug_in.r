library(MASS) # Generation of multidimensional normal distribution

getCurveFunc <- function(sigma1, mu1, sigma2, mu2) {
  d1 <- det(sigma1)
  d2 <- det(sigma2)
  invs1 <- solve(sigma1)
  invs2 <- solve(sigma2)
  
  a <- invs1 - invs2
  b <- invs1 %*% t(mu1) - invs2 %*% t(mu2)
  
  A <- a[1,1] # x^2
  B <- a[2,2] # y^2
  C <- 2 * a[1, 2] # xy
  D <- -2 * b[1, 1] # x
  E <- -2 * b[2, 1] # y
  G <- c(mu1 %*% invs1 %*% t(mu1) - mu2 %*% invs2 %*% t(mu2)) + log(abs(det(sigma1))) - log(abs(det(sigma2)))
  
  func <- function(x,y) {
    x^2 * A + y^2 * B + x*y*C + x*D + y*E + G
  }
  return(func)
}

getPlugInClassificator <- function(Prob = c(1),Prior = c(1),means,vars)
{
  n <- dim(means)[1]
  funcs <- list()
  
  makefunc <- function(i) {
    force(i)
    function(X)
    { 
      res <- log(Prob[i] * Prior[i])
      l <- length(X)
      chisl <- exp(
        (-1/2)*(
          t(X-as.vector(means[,i])) %*% solve(vars[[i]]) %*% (X-as.vector(means[,i]))
        )
      )
      res = chisl/((2*pi) * det(vars[[i]])^(1/2))
      
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

# Count of objects in each class
objects_count <- 300 

# Generation of test data
Sigma1 <- matrix(c(10, 0, 0, 15), 2, 2)
Sigma2 <- matrix(c(2, 0, 0, 2), 2, 2)
Mu1 <- c(0, 0)
Mu2 <- c(8, 0)
xy1 <- mvrnorm(n = objects_count, Mu1, Sigma1)
xy2 <- mvrnorm(n = objects_count, Mu2, Sigma2)

# Assembling 2 classes in one sample xl
xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))

# Drawing the training sample
colors <- c("green", "red")
plot(NULL, xlim=c(min(xl[ , 1]),max(xl[ , 1])), ylim=c(min(xl[ , 2]),max(xl[ , 2])), ylab="y", xlab="x")
points(xl[ , 1], xl[ , 2], pch = 21, bg = colors[xl[ ,3]], asp = 1, xlab = "x", ylab = "y")
# Evaluation
mu1 <- matrix(c(mean(xy1[,1]),mean(xy1[,2])),1,2)
mu2 <- matrix(c(mean(xy2[,1]),mean(xy2[,2])),1,2)

sigma1 <- var(xy1)
sigma2 <- var(xy2)


means<-matrix(c(mu1,mu2),length(mu1),2)
vars<-list()
vars[[1]]<-sigma1
vars[[2]]<-sigma2



plug_in_func <- getPlugInClassificator(rep(1,length(vars)),rep(1,length(vars)),means = means, vars = vars)

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
    points(xy[1],xy[2], col=colors[plug_in_func(xy)])
    
    j <- j+ystep
  }
  i <- i+xstep
}

curveFunc <- getCurveFunc(sigma1, mu1, sigma2, mu2)
x <- y <- seq(-20, 40, len = 500)
z <- outer(x, y, curveFunc)
contour(x, y, z, levels = 0, drawlabels = FALSE, lwd = 2.5, col = "red", add = TRUE)