#### Use of metafor package to perform meta-analysis for binary outcomes


##Installing and opening metafor package

install.packages("metafor") #Installs the metafor package
library(metafor)            #Opens the metafor package


##Meta-analysis for binary outcomes: Effect size calculation

#To demonstrate how to perform meta-analysis for binary outcomes, bcg dataset will be used.
#Please make sure that bcg.csv dataset has already been imported to your local environment.

?escalc        #Checks the arguments for escalc() function, which allows for effect size calculation

bcg <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=bcg)          #Computation of the effect size (log risk ratio) and of the corresponding sampling variance for each primary study


##Meta-analysis for binary outcomes: Performing meta-analysis

ma01 <- rma(yi=yi, vi=vi, data=bcg, slab=paste(author, year, sep=", "), method="REML")         #Meta-analysis following a random-effects model with the restricted maximum-likelihood estimator method for the amount of heterogeneity


ma01 <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=bcg, slab=paste(author, year, sep=", "), method="REML")      #This is another possibility for performing meta-anlysis: Instead of calculating the effect size measure for each primary study and then perform meta-analysis, it is possible to do it all in a single step!

ma01       #Shows the results of the meta-analysis

exp(c(ma01$beta, ma01$ci.lb, ma01$ci.ub))        #This expression allows for obtaining pooled estimates and the respective confidence interval bounds transformed into the natural scale. This is particularly needed in this case, as we performed a meta-analysis of log risk ratio. 


##Meta-analysis for binary outcomes: Forest plot creation

forest(ma01)      #Creates a forest plot for the meta-analysis we have performed based on the bcg dataset

forest.rma(ma01, transf=exp, order = order(bcg$year), showweights=TRUE)
text(-7.75, -1.85, pos=4, cex=0.8, bquote(paste("Q Cochran p = ", .(formatC(ma01$QEp, digits=2, format="f")), "; ", I^2, " = ", .(formatC(ma01$I2, digits=1, format="f")), "%")))         # The "transf" argument is used to present results in the natural scale. The "order" argument is used to present primary studies sorted by year. Relative weights are shown if "showweights"=TRUE. The text() function adds text for heterogeneity.

forest.rma(ma01, transf=exp, showweights=TRUE, alim =c(0,2), xlim=c(-2.5,4.5), at=c(0,0.5,1,1.5,2), order = order(bcg$year), refline=1)
text(-2.5,15, "Author(s) and Year",  pos=4)
text(3,15, "Weight", pos=2)
text(4.5,15, "RR [95% CI]", pos=2)
text(3.7, -1.85, pos=4, cex=0.8, bquote(paste("p <0.001")))
text(-2.5, -1.85, pos=4, cex=0.8, bquote(paste("Q Cochran p<0.001", "; ", I^2, " = ", .(formatC(ma01$I2, digits=1, format="f")), "%")))               # "alim" corresponds to the limit of the xx axis of the forest plot; "xlim" concerns the width of the plot; "at" locates the xx axis marks; "refline" locates the reference line.  


##Meta-analysis for binary outcomes: Exploration of heterogeneity - Sensitivity analyses, meta-regression and subgroup analyses

leave1out(ma01)        #Performs leave1out() sensitivity analysis

mreg01_lat <- rma(yi=yi~ablat, vi=vi, data=bcg, method="REML")       #Univariable meta-regression with the latitude as a covariate
mreg01_year <- rma(yi=yi~year, vi=vi, data=bcg, method="REML")       #Univariable meta-regression with the year as a covariate
mreg01_alloc <- rma(yi=yi~alloc, vi=vi, data=bcg, method="REML")     #Univariable meta-regression with the allocation method as a covariate

mreg01_lat      #Results of univariable meta-regression model with the latitude as covariate
mreg01_year     #Results of univariable meta-regression model with the year as covariate
mreg01_alloc    #Results of univariable meta-regression model with the allocation method as covariate

regplot(mreg01_lat)       #Bubble plot for the univariable meta-regression model with the latitude as covariate
regplot(mreg01_year)      #Bubble plot for the univariable meta-regression model with the year as covariate


mreg01_multi <- rma(yi=yi~ablat+year, vi=vi, data=bcg, method="REML")     #Multivariable meta-regression with latitude and year as covariates (not recommended in real practice with such a low number of primary studies)
mreg01_multi                                                              #Results of the multivariable meta-regression model
  
ma01_random <- rma(yi=yi, vi=vi, data=bcg, method="REML", slab=paste(author, year, sep=", "), subset=alloc=="random")          #Subgroup analysis in relation to the allocation type: Subgroup of random allocation
ma01_alternate <- rma(yi=yi, vi=vi, data=bcg, method="REML", slab=paste(author, year, sep=", "), subset=alloc=="alternate")    #Subgroup analysis in relation to the allocation type: Subgroup of alternate allocation
ma01_system <- rma(yi=yi, vi=vi, data=bcg, method="REML", slab=paste(author, year, sep=", "), subset=alloc=="systematic")      #Subgroup analysis in relation to the allocation type: Subgroup of systematic allocation

ma01_random       #Results of the subgroup analysis for studies with random sampling
ma01_alternate    #Results of the subgroup analysis for studies with alternate sampling
ma01_system       #Results of the subgroup analysis for studies with systematic sampling


##Meta-analysis for binary outcomes: Forest plot with subgroup analysis
par(mar=c(4,4,1,2)) #decrease margins so the full space is used
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR",slab=paste(author, year, sep=", "), method="REML") #fit random-effects model (use slab argument to define study labels)
# set up forest plot (with 2x2 table counts added; rows argument is used
forest(res, xlim=c(-16, 6), at=log(c(0.05, 0.25, 1, 4)), atransf=exp,ilab=cbind(dat.bcg$tpos, dat.bcg$tneg, dat.bcg$cpos, dat.bcg$cneg),ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.75, ylim=c(-1, 27),order=order(dat.bcg$alloc), rows=c(3:4,9:15,20:23),xlab="Risk Ratio", mlab="", psize=1)   #to specify exactly in which rows the outcomes will be plotted)
text(-16, -1, pos=4, cex=0.75, bquote(paste("RE Model for All Studies (Q = ",.(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",.(formatC(res$I2, digits=1, format="f")), "%)"))) # add text with Q-value, dfs, p-value, and I^2 statistic
op <- par(cex=0.75, font=4)  # set font expansion factor (as in forest() above) and use bold italic  # font and save original settings in object 'op'
text(-16, c(24,16,5), pos=4, c("Systematic Allocation","Random Allocation","Alternate Allocation")) #add text for the subgroups
par(font=2) # switch to bold font
text(c(-9.5,-8,-6,-4.5), 26, c("TB+", "TB-", "TB+", "TB-")) #add column headings to the plot
text(c(-8.75,-5.25),     27, c("Vaccinated", "Control"))
text(-16,                26, "Author(s) and Year",  pos=4)
text(6,                  26, "Risk Ratio [95% CI]", pos=2)
par(op) #set par back to the original settings
res.s <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR",subset=(alloc=="systematic"), method="REML")  # fit random-effects model in the three subgroups
res.r <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR",subset=(alloc=="random"), method="REML")
res.a <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR",subset=(alloc=="alternate"), method="REML")
addpoly(res.s, row=18.5, cex=0.75, atransf=exp, mlab="")#add summary polygons for the three subgroups
addpoly(res.r, row= 7.5, cex=0.75, atransf=exp, mlab="")
addpoly(res.a, row= 1.5, cex=0.75, atransf=exp, mlab="")
text(-16, 18.5, pos=4, cex=0.75, bquote(paste("RE Model for Subgroup (Q = ",.(formatC(res.s$QE, digits=2, format="f")), ", df = ", .(res.s$k - res.s$p),", p = ", .(formatC(res.s$QEp, digits=2, format="f")), "; ", I^2, " = ",.(formatC(res.s$I2, digits=1, format="f")), "%)"))) # add text with Q-value, dfs, p-value, and I^2 statistic for subgroups
text(-16, 7.5, pos=4, cex=0.75, bquote(paste("RE Model for Subgroup (Q = ",.(formatC(res.r$QE, digits=2, format="f")), ", df = ", .(res.r$k - res.r$p),", p = ", .(formatC(res.r$QEp, digits=2, format="f")), "; ", I^2, " = ",.(formatC(res.r$I2, digits=1, format="f")), "%)")))
text(-16, 1.5, pos=4, cex=0.75, bquote(paste("RE Model for Subgroup (Q = ",.(formatC(res.a$QE, digits=2, format="f")), ", df = ", .(res.a$k - res.a$p),", p = ", .(formatC(res.a$QEp, digits=2, format="f")), "; ", I^2, " = ",.(formatC(res.a$I2, digits=1, format="f")), "%)")))

##Meta-analysis for binary outcomes: Assessment of publication bias

ranktest(ma01)    #Calculates the rank correlation test of Begg

funnel(ma01)      #Obtention of the funnel plot

(ma01_tf <- trimfill(ma01))
funnel(ma01_tf)              #Obtention of the funnel plot with trim-and-fill


##Meta-analysis for binary outcomes: Obtention of other plots

radial(ma01)       #Obtention of the radial (Galbraith) plot

labbe(ma01)        #Obtention of the L'Abb? plot

baujat(ma01)       #Obtention of the Baujat plot

plot(gosh(ma01))   #Obtention of the GOSH plot