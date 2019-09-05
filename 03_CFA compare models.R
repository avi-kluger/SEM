# principles and practice of sem (4th ed.), rex kline
# two-factor model of the kabc-i, figure 9.7, table 13.1

date()
library(lavaan)

# input the correlations in lower diagnonal form
kabcLower.cor <- '
1.00
.39 1.00
.35  .67 1.00
.21  .11  .16 1.00
.32  .27  .29  .38 1.00
.40  .29  .28  .30  .47 1.00
.39  .32  .30  .31  .42  .41 1.00
.39  .29  .37  .42  .58  .51  .42 1.00 '
# name the variables and convert to full correlation matrix
kabcFull.cor <- getCov(kabcLower.cor, 
                       names = c("hm","nr","wo","gc","tr","sm","ma","ps"))
# display the correlations
kabcFull.cor
# add the standard deviations and convert to covariances
kabcFull.cov <- cor2cov(kabcFull.cor, 
                        sds = c(3.40,2.40,2.90,2.70,2.70,4.20,2.80,3.00))
kabcFull.cov

# specify cfa model

kabc.model <- '
# latent variables
      OneFactor  =~ hm + nr + wo + gc + tr + sm + ma + ps '
      
# fit model to data
modelOneF <- sem(kabc.model,
             sample.cov=kabcFull.cov,
             sample.nobs=200)
summary(modelOneF, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(modelOneF)
residuals(modelOneF, type = "raw")
residuals(modelOneF, type = "standardized")
residuals(modelOneF, type = "cor")
modindices(modelOneF)


kabc.model <- '
# latent variables
      Sequent  =~ hm + nr + wo
      Simultan =~ gc + tr + sm + ma + ps '

modelTwoF <- sem(kabc.model,
             sample.cov=kabcFull.cov,
             sample.nobs=200) 
summary(modelTwoF, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
library(semTools)

# Latent scale reliability
# https://rdrr.io/cran/semTools/man/reliability.html#heading-6
reliability (modelTwoF)
anova(modelOneF, modelTwoF)

# A better way to create one-factor model for comparison purposes
kabc.model <- '
# latent variables
      Sequent  =~ hm + nr + wo
      Simultan =~ gc + tr + sm + ma + ps 
# constrain the correlation between factor to 1      
      Sequent ~~ 1*Simultan '

# Force lavaan to test all loadings
model <- sem(kabc.model,
             sample.cov=kabcFull.cov,
             sample.nobs=200,
# "if you want to fix the variances of all the latent variables in a CFA model 
# to unity, there is again a shortcut. Simply add the argument std.lv=TRUE 
# to the function call". "If the argument std.lv=TRUE is used, the factor 
# loadings of the first indicator of each latent variable will no
# longer be fixed to 1.             
             std.lv = TRUE) 

model <- sem(modelTwoF,
             sample.cov=kabcFull.cov,
             sample.nobs=200,
# "if you want to fix the variances of all the latent variables in a CFA model 
# to unity, there is again a shortcut. Simply add the argument std.lv=TRUE 
# to the function call". "If the argument std.lv=TRUE is used, the factor 
# loadings of the first indicator of each latent variable will no
# longer be fixed to 1.             
             std.lv = TRUE)

summary(model, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fit <- fitmeasures(model, c("ntotal", "fmin", "df", "chisq", "pvalue", 
                      "rmsea.pvalue", "rmsea", "rmsea.ci.lower", 
                      "rmsea.ci.upper", "cfi", "srmr", "baseline.chisq",
                      "baseline.df"))
t(t(fit))



