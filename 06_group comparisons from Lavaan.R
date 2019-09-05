rm(list = ls())                               #Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
cat ("\014")                                  #Clean the R console

## Conduct weak invariance testing manually by using fixed-factor
## method of scale identification

library(lavaan)
library(semPlot)

HW.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

models2 <- measurementInvariance(model = HW.model, data=HolzingerSwineford1939,
                                 group="school")
partialInvariance(models2, "metric")

#  free "x5" across groups in advance
partialInvariance(models2, "metric", free = "x5") 

