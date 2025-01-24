# ggbond: Draw everything by ggplot2

## Installation
Install the latest version of ggbond: 
 ```
 install.packages("devtools")
devtools::install_github("RightSZ/ggbond")
 ```

## Plot aggr
```
library(ggbond)
library(VIM)
data(sleep, package="VIM")
data <- aggr(sleep)
aggrPlot(d=data)
```
