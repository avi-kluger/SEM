model <- '
  # measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
library(lavaan)
fit <- sem(model, data=PoliticalDemocracy)
summary(fit, standardized=TRUE)

library(semPlot)
semPaths(fit, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 8,
         edge.label.cex = .8,
         label.prop = .6,
         #cardinal = FALSE,
         cardinal = c("exogenous covariances"))
