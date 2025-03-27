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
## Plot xvg from GROMACS
### 1. Plotting RMSD (Root Mean Square Deviation)
```R
#Single File
rmsd_data <- read_xvg("./rmsd.xvg")
PlotRMSD(rmsd_data)

#Multiple Files
rmsd_data <- read_xvg(c("./rmsd_A.xvg",  # Distinguished by filename
                        "./rmsd_B.xvg",
                        "./rmsd_C.xvg"))
PlotRMSD(rmsd_data)
```
### 2. Plotting RMSF (Root Mean Square Fluctuation)
```R
#Single File
rmsf_data <- read_xvg("./rmsf.xvg")
PlotRMSF(rmsf_data)

#Multiple Files
rmsf_data <- read_xvg(c("./rmsf_A.xvg",  # Distinguished by filename
                        "./rmsf_B.xvg",
                        "./rmsf_C.xvg"))
PlotRMSF(rmsf_data)
```
### 3. Radius of Gyration
```R
gyrate_data <- read_xvg("./gyrate.xvg")
PlotGyrate(gyrate_data)
```
### 4. Solvent Accessible Surface Area
```R
area_data <- read_xvg("./area.xvg")
PlotArea(area_data)
```
### 5. Hydrogen Bonds
```R
hbonds_data <- read_xvg("./hbonds.xvg")
PlotHbonds(hbonds_data)
```
