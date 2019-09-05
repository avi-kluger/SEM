# principles and practice of sem (4th ed.), rex kline
# recursive path model of illness, figure 7.5, table 4.2

date()
library(lavaan)

# input the correlations in lower diagnonal form
rothLower.cor <- '
1.00
-.03 1.00
.39  .07 1.00
-.05 -.23 -.13 1.00
-.08 -.16 -.29  .34 1.00 '

# name the variables and convert to full correlation matrix
rothFull.cor <- getCov(rothLower.cor, 
                names = c("exercise","hardy","fitness","stress","illness"))

# display the correlations
rothFull.cor

# add the standard deviations and convert to covariances
rothFull.cov <- cor2cov(rothFull.cor, sds = c(66.50,38.00,18.40,33.50,62.48))

# display the covariances
rothFull.cov

# specify path model
        roth.model <- '

# regressions
        stress ~ hardy
        fitness ~ exercise
        illness ~ fitness + stress'

        # unanalyzed association between exercise and hardy
        # automatically specified

# fit initial model to data
# variances and covariance of measured exogenous
# variables are free parameters
# variances calculated with N - 1 in the denominator instead of N
        
model <- sem(roth.model,
         sample.cov  = rothFull.cov,
         sample.nobs = 373, 
         fixed.x     = FALSE, #TRUE if the exogenous x-covariates are treated as fixed.
         sample.cov.rescale = FALSE) #the user provided
        # covariance matrix is internally rescaled by multiplying it with a factor
        # (N-1)/N, to ensure that the covariance matrix has been divided by N. This can
        # be turned off by setting the sample.cov.rescale argument to FALSE.

summary(model, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

library(semPlot)

# Tweak the plot
semPaths(model, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10,
         layout = "tree2")

fitted    (model)
residuals (model, type = "raw")
residuals (model, type = "standardized")
residuals (model, type = "cor")

modindices(model)

# revise the model
        roth.model.revised <- '

# regressions
        stress ~ hardy
        fitness ~ exercise + stress
        illness ~ fitness + stress'

model.revised <- sem(roth.model.revised,
         sample.cov  = rothFull.cov,
         sample.nobs = 373, 
         fixed.x     = FALSE, #TRUE if the exogenous x-covariates are treated as fixed.
         sample.cov.rescale = FALSE)
        
# compare models 
anova(model, model.revised) 
myTable <- cbind(Original = inspect(model, 'fit.measures'),
           Modified = inspect(model.revised, 'fit.measures'))
round(as.data.frame(myTable), 2)

# revise the model with update
        roth.model.revised.with.update <- update(model, 
                                                 add = "fitness ~  stress")


model.update <- sem(roth.model.revised,
         sample.cov  = rothFull.cov,
         sample.nobs = 373, 
         fixed.x     = FALSE, #TRUE if the exogenous x-covariates are treated as fixed.
         sample.cov.rescale = FALSE)

# test the two ways of writing the syntax are identical        
anova(model.revised, model.update)     

summary(model.revised, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)       
semPaths(model.revised, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         # cardinal = FALSE,
         sizeMan = 10,
         layout = "tree2")
anova(model, model.revised)

# revise the model again by dropping the covariance
model.no.covariance <- update(model.revised, add = "exercise ~~ 0*hardy")
summary(model.no.covariance)
anova(model.revised, model.no.covariance)

semPaths(model.no.covariance, "std", 
         rotation = 2, 
         exoVar = FALSE, 
         exoCov = FALSE, # Exclude the covariance
         nCharNodes = 0,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 15,
         edge.label.cex = 1,
         label.prop = .8,
         sizeMan = 10,
         layout = "tree2")
