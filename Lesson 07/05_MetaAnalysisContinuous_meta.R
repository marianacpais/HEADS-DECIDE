#### Use of meta package to perform meta-analysis for continuous outcomes


##Installing and opening meta package

install.packages("meta") #Installs the meta package
library(meta)            #Opens the meta package


##Meta-analysis for continuous outcomes: Performing meta-analysis

#To demonstrate how to perform meta-analysis for continuous outcomes, stroke dataset will be used.
#Please make sure that stroke.csv dataset has already been imported to your local environment.

?metacont       #Checks the arguments for metacont() function, which allows for performing meta-analysis of continuous outcomes

ma05 <- metacont(data=stroke, n.e=n_spec, mean.e=los_spec, sd.e=sd_spec, n.c=n_rout, mean.c=los_rout, sd.c=sd_rout, sm="MD", method.tau="REML", prediction=TRUE, studlab=paste(study,year))               #Meta-analysis weighting by the inverse variance and with the restricted maximum-likelihood estimator method for the amount of heterogeneity. The prediction argument allows for obtention of estimates of a hypothetical future study. 

ma05       #Shows the results of the meta-analysis

ma05a <- metacont(data=stroke, n.e=n_spec, mean.e=los_spec, sd.e=sd_spec, n.c=n_rout, mean.c=los_rout, sd.c=sd_rout, sm="MD", method.tau="REML", prediction=TRUE, hakn=TRUE, studlab=paste(study,year))   #Meta-analysis with the Hartung-Knapp adjustment.


##Meta-analysis for continuous outcomes: Forest plot creation

forest(ma05, common=FALSE, prediction=FALSE, xlim=c(-75,50), digits=1, digits.sd=1)      #Creates a forest plot for the meta-analysis we have performed based on the stroke dataset. The "common=FALSE" argument is used to hide fixed effects model results. In this example, the predicted interval will not be presented  ("prediction=FALSE"). The "xlim" argument sets the limits of the xx axis of the forest plot. The number of decimal places is set by the "digits" arguments. 


##Meta-analysis for continuous outcomes: Exploration of heterogeneity - Meta-regression and subgroup analyses

mreg05_year <- metareg(ma05, year)       #Univariable meta-regression with the year as a covariate
mreg05_age <- metareg(ma05, age_group)   #Univariable meta-regression with the age group as a covariate

mreg05_year     #Results of univariable meta-regression model with the year as covariate
mreg05_age      #Results of univariable meta-regression model with the age group as covariate

ma05_sub <- metacont(data=stroke, n.e=n_spec, mean.e=los_spec, sd.e=sd_spec, n.c=n_rout, mean.c=los_rout, sd.c=sd_rout, sm="MD", method.tau="REML", prediction=TRUE, studlab=paste(study,year), byvar=age_group)           #Subgroup analysis (in relation to the age group): Analysis ina a meta-analysis performed de novo.

ma05_sub <- update(ma05, byvar=age_group)     #Alternative method of performing subgroup analysis when there is an already existent meta-analysis. This subgroup analysis is based on the age group.
ma05_sub                                      #Results of the subgroup analysis based on the age group

forest(ma05_sub, prediction=FALSE, common=FALSE, xlim=c(-100,50), digits=1, digits.sd=1)        #Plots the results of subgroup analyses


##Meta-analysis for continuous outcomes: Assessment of publication bias

metabias(ma05, method="rank", k.min=9)    #Calculates the rank correlation test of Begg. To perform Begg's rank correlation test, the argument "method" must be set to "rank". For Egger's linear regression test: method="linreg". Particularly for binary outcomes, Harbord's test (method="score") and Peter's test (method="peters") may be particularly interesting. k.min argument allows execution of this test for <10 primary studies.

funnel(ma05, common=FALSE)   #Obtention of the funnel plot

(ma05_tf <- trimfill(ma05, common=FALSE))
funnel(ma05_tf)                                #Obtention of the funnel plot with trim-and-fill



#### Use of metafor package to perform meta-analysis according to the generic inverse variance method

#To demonstrate how to perform meta-analysis according to the generic inverse variance method, stroke2 dataset will be used.

stroke2 <- as.data.frame(stroke2)

ma06 <-  metagen(data=stroke2, TE=mean_diff, seTE=sem, sm="MD", prediction=TRUE, studlab=paste(study, year))      #In meta package, metagen() function allows to perform meta-analysis based on the generic inverse variance method

ma06  #Shows the results of the meta-analysis

forest(ma06, random=FALSE, prediction=TRUE, xlim=c(-16,5), digits=1, digits.se = 1)     #Creates a forest plot for the meta-analysis performed based on the generic inverse variance method