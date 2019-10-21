# Метрические алгоритмы классификации


## [Алгоритм kNN](./alg_NN/)
**Алгоритм kNN** - метрический алгоритм классификации, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки.

### Формула алгоритма kNN:

![](./alg_NN/formula.png)
где ![](./alg_NN/formula_1.png) ответ на i-ом соседе,
а k - параметр (количество соседей), который определяет, сколько объектов (соседей) будет использовано для классификации.

### Реализация алгоритма на языке R
```r
kNN <- function(xl, z, k = c(6))
{
 orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k[length(k)], n + 1]

  class <- c(seq(length(k)))

  for (i in seq(length(k)))
  {
	counts <- table(orderedXl[1:i,n+1])
	class[i] <- names(which.max(counts))
  }
  
  return (class)
}
```

### По критерию скользящего контроля LOO оптимальное k = 6:

![](./alg_NN/_loo_KNN.png)

### Карта классификации kNN

![](./alg_NN/classificMap_KNN.png)

## [Алгоритм kwNN]

При взвешенном способе во внимание принимается не только количество попавших в область определённых классов, но и их удалённость от нового значения. Для каждого класса определяется оценка близости. У какого класса выше значение близости, тот класс и присваивается новому объекту.

### Формула алгоритма kwNN выглядит следующим образом
![](./alg_NN/formulakwNN.png)
где w(i) - функция веса, которая показывает насколько сильно i-ый объект влияет на принадлежность классифицируемого объекта к классу u; функция представляет собой геометрическую прогрессию с параметром q из диапазона [0; 1] (например: w(i) = q^i).

### Реализация алгоритма на языке R
```r
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
```

### По критерию скользящего контроля LOO оптимальное k = 6 и q = 1:

![](./alg_NN/_loo_kwNN.png)

### Карта классификации kwNN

![](./alg_NN/classificMap_kwNN.png)

