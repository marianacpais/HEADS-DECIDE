install.packages("estimraw")
library(estimraw)

estim_raw(es=0.6,lb=0.4,ub=0.9,m1=352,m2=376,dec=1,measure="rr")
estim_raw(es=0.60,lb=0.40,ub=0.90,m1=352,m2=376,dec=2,measure="rr")
estim_raw(es=0.60,lb=0.40,ub=0.90,m1=352,m2=376,e1=89,dec=2,measure="rr")