library("plotrix")
install.packages("gpuR")

# Функции ядер
RectKer <- function(r) (abs(r) <= 1) * 0.5
TriaKer <- function(r) (abs(r) <= 1) * (1 - abs(r))
QuarKer <- function(r) (abs(r) <= 1) * (1 - r^2)^2
EpanKer <- function(r) (abs(r) <= 1) * (1 - r^2)
GausKer <- function(r) dnorm(r)
euclideanDistance <- function(u, v)
{

  sqrt(sum((u - v)^2))
}

# potfunc algo
potfunc <- function(dat, p, kernel, l, h) {
  datLength <- dim(dat)[1]
  classCount <- dim(table(dat$Species))
  
  classes <- rep(0, classCount)
  names(classes) <- levels(dat$Species)
  
  for (i in seq(datLength)) {
    e <- dat[i,]
    distance <- euclideanDistance(p, e[1:2])
    
    weight <- kernel(distance / h[i]) * l[i]
    classes[e$Species] <- classes[e$Species] + weight
  }
  
  if (max(classes) == 0) {
    return ("")
  }
  return (names(which.max(classes)))
}

# count errors
errVal <- function(dat, kernel, l, h) {
  datLength <- dim(dat)[1]
  err <- 0
  
  for (i in seq(datLength)) {
    e <- dat[i,]
    res <- potfunc(dat, e[1:2], kernel, l, h)
    
    err <- err + (res != e$Species)
  }
  
  cat("err: ", err, "\n")
  #cat("l: ", l, "\n")
  return(err)
}

# calculate l
calcL <- function(dat, kernel, h, maxErr = 10,l = rep(0, datLength)) {
  datLength <- dim(dat)[1]
  old_l <- l
  i <- 1
  err_cnt_new <- errVal(dat, kernel, l, h)
  err_cnt_old <- err_cnt_new
  while(err_cnt_new > maxErr) {
    e <- dat[i,]
    res <- potfunc(dat, e[1:2], kernel, l, h)
    
    if (res != e$Species)
    {
      l[i] <- l[i] + 1
    }
    
    #i <- ((40+sample(1:110,1)[1])%%m)+1
    i <- sample(seq(datLength), 1)
    cat("i: ", i, "\n")
    err_cnt_new <- errVal(dat, kernel, l, h)
    if(err_cnt_new > err_cnt_old)
    {
      l <- old_l 
    }
    else
    {
      old_l <-  l
    }
  }
  
  return(l)
}

# example of usage
PotentialCircles <- function(dat, l, h) {
  # plot source data it
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main="Изображение потенциалов")

  
  
  #points(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main=title)
  points(dat[1:2], pch = 21, col = "black", bg = colors[dat$Species], main=title)
  
  
  #draw potential circles
  coef <- 0.2
  m <- max(l)
  for (i in seq(length(l))) {
    e <- dat[i,]
    if (l[i] < 1) {
		next
    }
    c <- adjustcolor(colors[e$Species], l[i] / m * coef)
    draw.circle(e[,1], e[,2], h[i], col = c, border = c)
  }
}

# example of usage
PlotMap <- function(dat, l, h, kerF = GausKer) {
  # plot source data
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main="Карта классификации")
  
  # classification map
  for (i in seq(1.0, 7.0, 0.1)) {
    for (j in seq(0.1, 2.5, 0.1)) {
      cl <- potfunc(dat, c(i, j), kerF, l, h)
      points(i, j, pch = 21, col = colors[cl])
    }
  }
  
  points(dat[1:2], pch = 21, col = "black", bg = colors[dat$Species], main=title)
}

dat = iris
#dat <- rbind(iris[6:20,], iris[61:75,], iris[136:150,])
datLength <- dim(dat)[1]
h <- c(rep(1, datLength/3), rep(0.5, (datLength/3)), rep(0.5, (datLength/3)))

kerF <- RectKer
res <- calcL(dat[3:5], kerF, h, maxErr = 5);
print(res)
print(errVal(dat[3:5],kerF,res,h))
PotentialCircles(dat[3:5], res, h)
PlotMap(dat[3:5], res, h, kerF)
  