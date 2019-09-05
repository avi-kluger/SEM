#Q 14.1
#------------
# input Table 14.4

txt <- 'Constructive_w_Dysfunctional -.028 .018 -.124 .073
Constructive_w_SubjectiveWellBeing .024 .014 .140 .080
Constructive_w_JobSatisfaction .060 .029 .165 .074
Dysfunctional_w_SubjectiveWellBeing -.088 .018 -.480 .063
Dysfunctional_w_JobSatisfaction -.132 .030 -.344 .064
SubjectiveWellBeing_w_JobSatisfaction .139 .027 .466 .066'

x           <- data.frame(read.table(text = txt))

x$UnstandardizedZ <- x$V2/x$V3
x$StandardizedZ   <- x$V4/x$V5
x$UnstandardizedP <- ifelse(x$UnstandardizedZ > 0,
                            1-pnorm(x$UnstandardizedZ),
                            pnorm(x$UnstandardizedZ))
x$StandardizedP   <- ifelse(x$StandardizedZ > 0,
                            1-pnorm(x$StandardizedZ),
                            pnorm(x$StandardizedZ))
rownames(x) <- x[, 1]
round(x[,-c(1:5)], 3)
#------------
#Q14.2
#-----------
# principles and practice of sem (4th ed.), rex kline
# fully latent model of thought strategies and job satisfaction
# figures 10.6, 14.1-14.2, table 14.1

date()
library(lavaan)

# input the correlations in lower diagnonal form
houghtonLower.cor <- '
1.000
.668 1.000
.635  .599 1.000
.263  .261   .164 1.000
.290  .315   .247  .486 1.000
.207  .245   .231  .251  .449 1.000
-.206 -.182  -.195 -.309 -.266 -.142 1.000 
-.280 -.241  -.238 -.344 -.305 -.230  .753 1.000
-.258 -.244 -.185  -.255 -.255 -.215  .554  .587 1.000 
.080  .096  .094  -.017  .151  .141 -.074 -.111  .016 1.000
.061  .028 -.035  -.058 -.051 -.003 -.040 -.040 -.018 .284 1.000
.113  .174  .059   .063  .138  .044 -.119 -.073 -.084 .563  .379 1.000 '
# name the variables and convert to full correlation matrix
houghtonFull.cor <- 
  getCov(houghtonLower.cor, names = c("wk1","wk2","wk3","hap","md1","md2","pr1","pr2","app","bel","st","ima"))
# display the correlations
houghtonFull.cor
# add the standard deviations and convert to covariances
houghtonFull.cov <- 
  cor2cov(houghtonFull.cor, sds = c(.939,1.017,.937,.562,.760,.524,.585,.609,.731,.711,1.124,1.001))
houghtonFull.cov

# specify cfa model
houghtonCFA.model <- '
# measurement part
Constru =~ bel + st + ima
Dysfunc =~ pr1 + pr2 + app
WellBe =~ hap + md1 + md2
JobSat =~ wk1 + wk2 + wk3
# error covariance
hap ~~ md2 '


# specify sr model
houghtonSR.model <- '
# measurement part
     Construc =~ bel + st + ima
     Dysfunc  =~ pr1 + pr2 + app
     WellBe   =~ hap + md1 + md2
     JobSat   =~ wk1 + wk2 + wk3
# error covariance
     hap ~~ md2 
# structural part
     Dysfunc ~ Construc
     WellBe  ~ Construc + Dysfunc
     JobSat  ~ Construc + Dysfunc + WellBe '

# fit cfa model to data
cfamodel <- sem(houghtonCFA.model, 
                sample.cov=houghtonFull.cov,
                sample.nobs=263)
summary(cfamodel, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(cfamodel)
residuals(cfamodel, type = "raw")
residuals(cfamodel, type = "standardized")
residuals(cfamodel, type = "cor")
modindices(cfamodel)

# fit sr model to data
srmodel <- sem(houghtonSR.model,
               sample.cov=houghtonFull.cov,
               sample.nobs=263)
summary(srmodel, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(srmodel)
residuals(srmodel, type = "raw")
residuals(srmodel, type = "standardized")
residuals(srmodel, type = "cor")
modindices(srmodel)

# specify sr model with direct and indirect effects
houghtonSR.model <- '
# measurement part
     Construc =~ bel + st + ima
     Dysfunc  =~ pr1 + pr2 + app
     WellBe   =~ hap + md1 + md2
     JobSat   =~ wk1 + wk2 + wk3
# error covariance
     hap ~~ md2 
# structural part
     Dysfunc ~ a*Construc
     WellBe  ~ b*Construc + c*Dysfunc
     JobSat  ~ d*Construc + e*Dysfunc + f*WellBe 
# WB effects 
         WBdir   := b  
         WBind   := a*c
         WBtotal := b + (a*c)
# DT on JS effects
         DT_JSdir   := e
         DT_JSind   := c*f
         DT_JSTotal := e  + (c*f) 
# CT on JS effects
         CT_JSdir   := d
         CT_JSind   := (a*e) + (b*f) + (a*c*f)
         CT_JSTotal := d + (a*e) + (b*f) + (a*c*f)   
'
srmodel <- sem(houghtonSR.model,
               sample.cov=houghtonFull.cov,
               sample.nobs=263)
summary(srmodel, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
#-----------
#Q 14.3
#-----------
# principles and practice of sem (4th ed.), rex kline
# nonrecursive model of commitment and turnover intention
# figure 11.7, table 14.5

date()
library(lavaan)

# input the correlations in lower diagnonal form
changLower.cor <- '
1.0
-.10  1.0
.66  .10  1.0
.48  .06  .42  1.0
.08  .58  .15  .22  1.0
.48  .12  .44  .69  .34  1.0
-.53 -.04 -.58 -.34 -.13 -.34 1.0
-.50 -.02 -.40 -.63 -.28 -.58 .56 1.0 '
# name the variables and convert to full correlation matrix
changFull.cor <-
  getCov(changLower.cor, names = c("aoc","coc","noc","apc","cpc","npc","orgti","occti"))
# display the correlations
changFull.cor
# add the standard deviations and convert to covariances
changFull.cov <-
  cor2cov(changFull.cor, sds = c(1.04,.98,.97,1.07,.78,1.09,1.40,1.50))
changFull.cov

# specify initial model with single indicators
chang.model1 <- '
#latent variables
AOC =~ aoc
COC =~ coc
NOC =~ noc
APC =~ apc
CPC =~ cpc
NPC =~ npc
OrgTI =~ orgti
OccTI =~ occti
#regressions
OrgTI ~ AOC + COC + NOC
OccTI ~ APC + CPC + NPC
OrgTI ~ OccTI
OccTI ~ OrgTI
#fix error variances
aoc ~~ .1947*aoc
coc ~~ .2881*coc
noc ~~ .2446*noc
apc ~~ .1603*apc
cpc ~~ .1764*cpc
npc ~~ .1901*npc
orgti ~~ .2744*orgti
occti ~~ .2700*occti
#correlated disturbances
OrgTI ~~ OccTI '

# respecified model with single indicators
chang.model2 <- '
#latent variables
AOC =~ aoc
COC =~ coc
NOC =~ noc
APC =~ apc
CPC =~ cpc
NPC =~ npc
OrgTI =~ orgti
OccTI =~ occti
#regressions
OrgTI ~ AOC + COC + NOC
OccTI ~ APC + CPC + NPC + COC
OrgTI ~ OccTI
OccTI ~ OrgTI
#fix error variances
aoc ~~ .1947*aoc
coc ~~ .2881*coc
noc ~~ .2446*noc
apc ~~ .1603*apc
cpc ~~ .1764*cpc
npc ~~ .1901*npc
orgti ~~ .2744*orgti
occti ~~ .2700*occti
#correlated disturbances
OrgTI ~~ OccTI '

# fit initial model to data
model1 <- sem(chang.model1,
              sample.cov=changFull.cov,
              sample.nobs=177)
summary(model1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(model1)
residuals(model1, type = "raw")
residuals(model1, type = "standardized")
residuals(model1, type = "cor")
modindices(model1)

# fit respecified model to data
model2 <- sem(chang.model2,
              sample.cov=changFull.cov,
              sample.nobs=177)
summary(model2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(model2)
residuals(model2, type = "raw")
residuals(model2, type = "standardized")
residuals(model2, type = "cor")
modindices(model2)

# compare two nested models
anova(model1, model2)

# equality-constrained reciprocal effect
chang.model3 <- '
# latent variables
AOC =~ aoc
COC =~ coc
NOC =~ noc
APC =~ apc
CPC =~ cpc
NPC =~ npc
OrgTI =~ orgti
OccTI =~ occti
# regressions
OrgTI ~ AOC + COC + NOC
OccTI ~ APC + CPC + NPC + COC
# constrained imposed through label ("a")
OrgTI ~ a*OccTI
OccTI ~ a*OrgTI
# fix error variances
aoc ~~ .1947*aoc
coc ~~ .2881*coc
noc ~~ .2446*noc
apc ~~ .1603*apc
cpc ~~ .1764*cpc
npc ~~ .1901*npc
orgti ~~ .2744*orgti
occti ~~ .2700*occti
# correlated disturbances
OrgTI ~~ OccTI '

model3 <- sem(chang.model3,
       sample.cov=changFull.cov,
       sample.nobs=177)
summary(model3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
#-----------

#Q 14.4
#-----------