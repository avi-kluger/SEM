NLSY <- read.csv("https://www.dropbox.com/s/yzrwqldr2ngy70i/NLSY.csv?dl=1")
names(NLSY) <- c("ID",
                 "RACL/ETHNIC COHORT",
                 "SEX",
                 "Worth",
                 "Good.Qualities",
                 "Feel.Am.Failure",
                 "Capable.As.Others",
                 "Not.Proud",
                 "Positive.Attitude",
                 "Satisfied.W.Myself",
                 "Wish.More.Self.Respect",
                 "Feel.Useless",
                 "No.Good",
                 "IQ")


# NOTE on levels

  # RACL/ETHNIC COHORT
  # 1 "HISPANIC"
  # 2 "BLACK"
  # 3 "NON-BLACK, NON-HISPANIC"

  # SEX
  # 1 "MALE"
  # 2 "FEMALE"

  #Self Esteem items
  # 1 "STRONGLY AGREE"
  # 2 "AGREE"
  # 3 "DISAGREE"
  # 4 "STRONGLY DISAGREE"

apply(NLSY[, -c(1, 14)], 2, table)

#Missing values are coded as negative numbers.  Change to NA.
for (i in 4:14) NLSY[, i] <- ifelse(NLSY[, i] < 0, NA, NLSY[, i])
apply(NLSY[, -c(1, 14)], 2, table)       

library(psych)
describe (NLSY[, 14])
NLSY[, 14] <- NLSY[, 14] / 1000
describe (NLSY[, 14])
boxplot(NLSY[, 14])

library(lavaan)

SE.model <- '
# latent variables
     Positive  =~  Worth +
                   Good.Qualities +
                   Capable.As.Others +
                   Positive.Attitude +
                   Satisfied.W.Myself 
     Negative  =~  Feel.Am.Failure +
                   Not.Proud +
                   Wish.More.Self.Respect +
                   Feel.Useless +
                   No.Good
'

# constrain the correlation between factors to 1      
     
#      Positive ~~ 1*Negative '

model1 <- cfa(SE.model, data = NLSY, std.lv = TRUE)
model2 <- cfa(SE.model, data = NLSY, std.lv = TRUE, 
             ordered = c( "Worth",
                          "Good.Qualities",
                          "Feel.Am.Failure",
                          "Capable.As.Others",
                          "Not.Proud",
                          "Positive.Attitude",
                          "Satisfied.W.Myself",
                          "Wish.More.Self.Respect",
                          "Feel.Useless",
                          "No.Good"))

summary(model1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(model2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

SE.model1F <- '
# latent variables
    Positive  =~  Worth + Good.Qualities + Capable.As.Others + Positive.Attitude + Satisfied.W.Myself 
    Negative  =~  Feel.Am.Failure + Not.Proud + Wish.More.Self.Respect + Feel.Useless + No.Good
# constrain the correlation between factors to 1      

   Positive ~~ 1*Negative '
model3 <- cfa(SE.model1F, data = NLSY, std.lv = TRUE)

options(scipen = 7)
cbind(Original=inspect(model3, 'fit.measures'), Modified=inspect(model1, 'fit.measures'))
anova(model3, model1)

residuals(model1, type = "standardized")
residuals(model1, type = "cor")$cov
modindices(model1)

library(semPlot)
semPaths(model1, "std", 
         rotation = 1,
         layout = "tree2",
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 8,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 4,
         edge.label.cex = 1,
         label.prop = .9,
         cardinal = c("exogenous covariances"),
         edge.label.position = .75,
         sizeMan =  5,
         sizeLat = 10,
         curve = 4)

## Parcels

NLSY$p1 <-     rowMeans(NLSY[,c("Worth", "Good.Qualities", "Capable.As.Others")])
NLSY$p2 <-     rowMeans(NLSY[,c("Positive.Attitude", "Satisfied.W.Myself"     )]) 
NLSY$n1 <-     rowMeans(NLSY[,c("Feel.Am.Failure", "Not.Proud", "Wish.More.Self.Respect")]) 
NLSY$n2 <-     rowMeans(NLSY[,c("Feel.Useless", "No.Good")])

SE.modelParcel <- '
# latent variables
    Positive  =~  p1 + p2
    Negative  =~  n1 + n2'

model4 <- cfa(SE.modelParcel, data = NLSY, std.lv = TRUE)
summary(model4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
semPaths(model4, "std", 
         rotation = 1,
         layout = "tree2",
         exoVar = FALSE, 
         exoCov = TRUE, 
         nCharNodes = 8,
         edge.color = "black",
         what = "mod",
         residuals = FALSE,
         sizeInt = 4,
         edge.label.cex = 1,
         label.prop = .9,
         cardinal = c("exogenous covariances"),
         edge.label.position = .75,
         sizeMan =  5,
         sizeLat = 10,
         curve = 4)
