#### Use of metafor package to perform meta-analysis for continuous outcomes


##Installing and opening metafor package

install.packages("metafor") #Installs the metafor package
library(metafor)            #Opens the metafor package


##Meta-analysis for continuous outcomes: Effect size calculation

#To demonstrate how to perform meta-analysis for binary outcomes, stroke dataset will be used.
#Please make sure that stroke.csv dataset has already been imported to your local environment.
stroke <- as.data.frame(stroke)

?escalc        #Checks the arguments for escalc() function, which allows for effect size calculation

stroke <- escalc(measure="MD", n1i=n_spec, m1i=los_spec, sd1i=sd_spec, n2i=n_rout, m2i=los_rout, sd2i=sd_rout, data=stroke)          #Computation of the effect size (mean difference) and of the corresponding sampling variance for each primary study


##Meta-analysis for continuous outcomes: Performing meta-analysis

ma02 <- rma(yi=yi, vi=vi, data=stroke, slab=paste(study, year, sep=", "), method="REML")         #Meta-analysis following a random-effects model with the restricted maximum-likelihood estimator method for the amount of heterogeneity


ma02 <- rma(measure="MD", n1i=n_spec, m1i=los_spec, sd1i=sd_spec, n2i=n_rout, m2i=los_rout, sd2i=sd_rout, data=stroke, slab=paste(study, year, sep=", "), method="REML")      #This is another possibility for performing meta-anlysis: Instead of calculating the effect size measure for each primary study and then perform meta-analysis, it is possible to do it all in a single step!

ma02       #Shows the results of the meta-analysis

cumul(ma02)  #Shows the results for cumulative meta-analysis


##Meta-analysis for continuous outcomes: Forest plot creation

forest.rma(ma02, showweights=TRUE, order = order(stroke$year), xlim=c(-350,350))
text(-350,10.5, "Author(s) and Year",  pos=4)
text(150,10.5, "Weight", pos=2)
text(350,10.5, "RR [95% CI]", pos=2)
text(265, -1.85, pos=4, cex=0.8, bquote(paste("p = ", .(formatC(ma02$pval, digits=3, format="f")))))
text(-350, -1.8, pos=4, cex=0.8, bquote(paste("Q Cochran p<0.001", "; ", I^2, " = ", .(formatC(ma02$I2, digits=1, format="f")), "%")))


##Meta-analysis for continuous outcomes: Exploration of heterogeneity - Sensitivity analyses, meta-regression and subgroup analyses

leave1out(ma02)        #Performs leave1out() sensitivity analysis

mreg02_year <- rma(yi=yi~year, vi=vi, data=stroke, method="REML")         #Univariable meta-regression with the year as a covariate
mreg02_age <- rma(yi=yi~age_group, vi=vi, data=stroke, method="REML")     #Univariable meta-regression with the age group as a covariate

mreg02_year     #Results of univariable meta-regression model with the year as covariate
mreg02_age      #Results of univariable meta-regression model with the age group as covariate

regplot(mreg02_year)       #Bubble plot for the univariable meta-regression model with the year as covariate


ma02_2010 <- rma(yi=yi, vi=vi, data=stroke, method="REML", slab=paste(study, year, sep=", "), subset=year>=2010)               #Subgroup analysis in relation to the year: Subgroup of post-2010 studies
ma02_uk <- rma(yi=yi, vi=vi, data=stroke, method="REML", slab=paste(study, year, sep=", "), subset=region=="uk")               #Subgroup analysis in relation to the region: Subgroup of UK studies

ma02_2010       #Results of the subgroup analysis for post-2010 studies
ma02_uk         #Results of the subgroup analysis for studies from the UK


##Meta-analysis for continuous outcomes: Assessment of publication bias

ranktest(ma02)    #Calculates the rank correlation test of Begg

funnel(ma02)      #Obtention of the funnel plot

(ma02_tf <- trimfill(ma02))
funnel(ma02_tf)              #Obtention of the funnel plot with trim-and-fill


##Meta-analysis for continuous outcomes: Obtention of other plots

radial(ma02)       #Obtention of the radial (Galbraith) plot

baujat(ma02)       #Obtention of the Baujat plot

plot(gosh(ma02))   #Obtention of the GOSH plot



#### Use of metafor package to perform meta-analysis according to the generic inverse variance method

#To demonstrate how to perform meta-analysis according to the generic inverse variance method, stroke2 dataset will be used.

stroke2 <- as.data.frame(stroke2)

ma03 <- rma.uni(yi=mean_diff, vi=sem, data=stroke2, slab=paste(study, year, sep=", "), method="REML")      #We do not need to calculate effect measures - they had already been calculated

ma03  #Shows the results of the meta-analysis
