## Р вЂўР Р†Р С”Р В»Р С‘Р Т‘Р С•Р Р†Р С• РЎР‚Р В°РЎРѓРЎРѓРЎвЂљР С•РЎРЏР Р…Р С‘Р Вµ
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

## Р РЋР С•РЎР‚РЎвЂљР С‘РЎР‚РЎС“Р ВµР С Р С•Р В±РЎР‰Р ВµР С”РЎвЂљРЎвЂ№ РЎРѓР С•Р С–Р В»Р В°РЎРѓР Р…Р С• РЎР‚Р В°РЎРѓРЎРѓРЎвЂљР С•РЎРЏР Р…Р С‘РЎРЏ Р Т‘Р С• Р С•Р В±РЎР‰Р ВµР С”РЎвЂљР В° z
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  ## Р РЋР С•Р В·Р Т‘Р В°РЎвЂР С Р СР В°РЎвЂљРЎР‚Р С‘РЎвЂ РЎС“ РЎР‚Р В°РЎРѓРЎРѓРЎвЂљР С•РЎРЏР Р…Р С‘Р в„–
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  ## Р РЋР С•РЎР‚РЎвЂљР С‘РЎР‚РЎС“Р ВµР С
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

## Р СљР ВµРЎвЂљР С•Р Т‘ Р Р†Р В·Р Р†Р ВµРЎв‚¬Р ВµР Р…Р Р…РЎвЂ№РЎвЂ¦ k-Р В±Р В»Р С‘Р В¶Р В°Р в„–РЎв‚¬Р С‘РЎвЂ¦ РЎРѓР С•РЎРѓР ВµР Т‘Р ВµР в„–
kwNN <- function(xl, z, k, q)
{
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2]
  
  classes <- orderedXl[1:length(k), n]
  
  lvls <- levels(classes)
  
  weighted_classes <- cbind(as.matrix(classes),q^as.numeric(1:length(k))) 
  class <- vector()
  for (i in seq(length(k)))
  {
    curr <- as.matrix(weighted_classes[1:i,])
    if(i == 1)
      curr <- t(curr)
    result <- as.matrix(sapply(1:length(lvls), function(x) sum(as.numeric(curr[which(curr[,1] == lvls[x]),2]))))
    
    class[i] <- lvls[which.max(result)]
  }
  
  return (class)
}

looCV_kwNN <- function(dat, algo, q = 1) {
  l <- length(dat[[1]])
  correct <- rep(0, l)
  
  for (i in seq(l)) {
    trainData <- dat[-i, ]
    control <- dat[i, ]
    
    res <- kwNN(trainData, control[1:2], seq(l), q)
    correct <- correct + (res != control$Species)
  }
  
  res <- (correct/l)
  return (res)
}

demo <- function()
{
  kdiap <- seq(5,11,length.out = 7)
  kqdiap <- seq(kdiap[1]+0.1,kdiap[length(kdiap)]+1,length.out=length(kdiap)*10)
  
  dat <- iris[,3:5]
  
  res<-matrix(0,nrow=10,ncol=length(kdiap))
  big_res<-rep(0,length(kqdiap))
  
  for(q in seq(0.1, 1, length.out = 10) )
  {
    print(q)
    qq<-looCV_kwNN(dat, kwNN, q)
    res[q*10,] <- qq[kdiap]
  }
  
  for(i in seq(length(kdiap)))
  {
     print(i)
     big_res[((i)*10-9):((i)*10)] <- res[,i]
  }
  
  dataRes <- as.data.frame(big_res)
  plot(data.frame("k"=kqdiap, "LOO(k)"=big_res), type="l")
  minK <- which.min(dataRes$res)
  points(minK,dataRes$res[minK],type="p",pch = 16, col = "green",bg = "green")
  
}
demo()
