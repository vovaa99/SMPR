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

DistanceToDataFrame <- function(xl, z, metricFunction = euclideanDistance)
{
  lengthDat <- nrow(xl)
  n <- dim(xl)[2]-1
  for(i in 1:lengthDat)
  {
    xl$distance[i] <- metricFunction(xl[i,1:n],z)
  }

  return (xl);
}

parsen <- function(dat, z, h=c(0.35), kerF = RectKer) {
  n <- dim(dat)[2]
  m <- dim(dat)[1]
  datWithDist <- DistanceToDataFrame(dat, z)
  
  classifiedObjects <- rep("", length(h))
  
  for(i in seq(length(h)))
  {
    classes <- rep(0, length(names(table(dat[,n]))))
    names(classes) <- names(table(dat[,n]))
    
    for(j in 1:m){
      y <- datWithDist[j, n]
      w <- kerF(datWithDist[j,4]/h[i])
      classes[y] <- classes[y] + w
    }
    if(sum(classes) > 0) {
      class <- names(which.max(classes))
    } else {
      class <- ""
    }
    classifiedObjects[i] = class
  }
  return(classifiedObjects)
}


looCV <- function(dat, algo,...){
  l <- length(dat[[1]])
  correct <- rep(0, length(l))
  
  for (i in seq(l)) {
    trainData <- dat[-i, ]
    control <- dat[i, ]
    
    res <- algo(trainData, control[1:2],...)
    correct <- correct + (res != control$Species)
  }
  
  res <- (correct/l)
  return (res)
}

looCV_parsen <- function(dat, h)
{
  dat<-iris[,3:5]
  h<-seq(0.1,4,0.05)
  res <- looCV(dat, parsen, h)
  lfromh <- data.frame("h"=h, "LOO"=res)
  title<- "LOO метода парзеновского окна с прямоугольным ядром"
  jpeg(file="D:/SMPR/metric/alg_parsen/rect_LOO.jpeg",width = 697, height = 502)
  plot(lfromh, type="l", main=title)
  # best h with lowest Q
  m = lfromh[which.min(lfromh$LOO),]
  points(m, pch=21, bg="green")
  
  text(m[["h"]], m[["LOO"]], adj=c(0,-1), sprintf("(%.3f; %.3f)", m[["h"]], m[["LOO"]]))
  
  return (m)
}


# Классифицировать обьект с координатами z1,z2
FindParsen <- function(z1, z2, h, kerF) {
  points(z1,z2, pch = 21, col=colors[parsen(iris[,3:5],c(z1,z2),h,kerF)])
}

PlotMapParsen <- function(h = 0.1, kerF = EpanKer, label="ядром Епанечникова") {
  #jpeg(file=paste0("D:/SMPR/metric/alg_parsen/PlotMap_",as.character(substitute(kerF)),"_%03d.jpeg"),width = 697, height = 502)
  plot(iris[, 3:4], main=paste("Карта классификации\nдля парзеновского окна с", label), pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp='1') 
  for (i in seq(0,7,0.1)) {
    print(i)
    for (j in seq(0,3,0.1)){
      print(j)
      FindParsen(i,j,h,kerF)
    }
  }
  dev.off()
}

demo <- function(){
  plot(iris[, 3:4], main="Пример классификации методом парзеновского окна", pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp='1')
  z <- c(2,0.5)
  result <- parsen(iris[,3:5],z,c(0.35),EpanKer)
  points(z[1],z[2], pch = 21, col=colors[result])
}

colors <- c( "setosa"="red", "versicolor"="green3", "virginica"="blue" )
demo()

#looCV_parsen(iris[,3:5],seq(0.1,4,0.5))
#PlotMapParsen(0.35, RectKer, "прямоугольным ядром")
