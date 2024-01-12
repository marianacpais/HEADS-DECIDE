#### Use of meta package to perform meta-analysis for binary outcomes


##Installing and opening meta package

install.packages("meta") #Installs the meta package
library(meta)            #Opens the meta package


bcg <- read.csv("Lesson 07/bcg.csv")

##Meta-analysis for binary outcomes: Performing meta-analysis

#To demonstrate how to perform meta-analysis for binary outcomes, bcg dataset will be used. # nolint
#Please make sure that bcg.csv dataset has already been imported to your local environment. # nolint

?metabin       #Checks the arguments for metabin() function, which allows for performing meta-analysis of binary outcomes # nolint

ma04 <- metabin(
  data=bcg,
  event.e=tpos,
  n.e=tpos+tneg,
  event.c=cpos,
  n.c=cpos+cneg,
  method="Inverse",
  sm="RR",
  method.tau="REML",
  prediction=TRUE,
  studlab=paste(author,year)
)

#Meta-analysis weighting by the inverse variance and with the restricted maximum-likelihood estimator method for the amount of heterogeneity. The prediction argument allows for obtention of estimates of a hypothetical future study.  # nolint

ma04       #Shows the results of the meta-analysis


##Meta-analysis for binary outcomes: Forest plot creation

forest(
  ma04,
  common=FALSE,
  prediction = TRUE,
  digits = 1
)

#Creates a forest plot for the meta-analysis we have performed based on the bcg dataset. The "common=FALSE" expression is used to hide fixed effects model results. To hide random effects model results, we would use the expression "comb.random=FALSE". The "prediction" argument orders the presentation of the predicted interval. The number of decimal places is set by the "digits" argument.  # nolint

metainf(
  ma04,
  pooled="random"
)

##Meta-analysis for binary outcomes: Exploration of heterogeneity - Meta-regression and subgroup analyses # nolint

mreg04_lat <- metareg(
  ma04,
  ablat
)
#Univariable meta-regression with the latitude as a covariate
mreg04_year <- metareg(
  ma04,
  year
)
#Univariable meta-regression with the year as a covariate
mreg04_alloc <- metareg(
  ma04,
  alloc
)
#Univariable meta-regression with the allocation method as a covariate

mreg04_lat
#Results of univariable meta-regression model with the latitude as covariate
summary(mreg04_year)
#Results of univariable meta-regression model with the year as covariate
mreg04_alloc
#Results of univariable meta-regression model with the allocation method as covariate # nolint

bubble(mreg04_lat)
#Plots the results of a meta-regression for a continuous covariate

mreg04_multi <- metareg(
  ma04,
  ablat+year
)
#Multivariable meta-regression with latitude and year as covariates (not recommended in real practice with such a low number of primary studies) # nolint
mreg04_multi
#Results of the multivariable meta-regression model

ma04_sub <- metabin(
  data=bcg,
  event.e=tpos,
  n.e=tpos+tneg,
  event.c=cpos,
  n.c=cpos+cneg,
  method="Inverse",
  sm="RR",
  method.tau="REML",
  prediction=TRUE,
  studlab=paste(author,year),
  byvar=alloc
)
# Subgroup analysis (in relation to the allocation type): Analysis in a a meta-analysis performed de novo. # nolint

ma04_sub <- update(ma04, byvar=alloc)
# Alternative method of performing subgroup analysis when there is an already existent meta-analysis. This subgroup analysis is based on the allocation method # nolint
ma04_sub
# Results of the subgroup analysis based on the allocation method

forest(
  ma04_sub,
  prediction=FALSE,
  common=FALSE,
  xlim=c(0.05,10),
  digits=1,
  digits.sd=1
)
# Plots the results of subgroup analyses


##Meta-analysis for binary outcomes: Assessment of publication bias

metabias(ma04, method="rank")
# Calculates the rank correlation test of Begg. To perform Begg's rank correlation test, the argument "method" must be set to "rank". For Egger's linear regression test: method="linreg". Particularly for binary outcomes, Harbord's test (method="score") and Peter's test (method="peters") may be particularly interesting. # nolint

funnel(ma04, common=FALSE)
# Obtention of the funnel plot

(ma04_tf <- trimfill(ma04, common=FALSE))
funnel(ma04_tf)
# Obtention of the funnel plot with trim-and-fill