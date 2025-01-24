# ggbond: Draw everything by ggplot2

## Installation
Install the latest version of ggbond: 
 ```R
 install.packages("devtools")
devtools::install_github("RightSZ/ggbond")
 ```

## Plot aggr
```R
library(ggbond)
library(VIM)
data(sleep, package="VIM")
data <- aggr(sleep)
aggrPlot(d=data)
```

## Plot importance from randomForest
```R
set.seed(1234)
library(randomForest)
library(ggbond)
iris.rf <- randomForest(Species ~ ., data=iris,ntree=500)

optionTrees = which.min(iris.rf$err.rate[,1])
iris.rf2 = randomForest(as.factor(Species)~.,data = iris,ntree=optionTrees)
importance = importance(x=iris.rf2)
varimpPlot(importance, cutoff=3)
```
