rm(list = ls())                               #Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
cat ("\014")                                  #Clean the R console

# load the lavaan package
library(lavaan)

# specify the model
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

# fit the model
fit <- cfa(HS.model, data=HolzingerSwineford1939)

# display summary output
summary(fit, fit.measures=TRUE, standardized=TRUE)

# plot the model
library(semPlot)
semPaths(fit)

# Tweak the plot
semPaths(fit, "std", 
         rotation = 4, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         cardinal = FALSE)

browseURL("http://lavaan.ugent.be/tutorial/index.html")
