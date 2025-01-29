library(survival) # for survival analysis
library(visreg) # for the beautiful graphs
library(RColorBrewer) # for the Color pallettes
library(survminer)# graphs
library(dplyr)
library(tidyr)

#survivorship analysis for crabs post thermal metabolic experiment

crab_surv<-crabsurvival

crab_surv<-crab_surv %>% filter(group =="infected"|group =="nonovigerous"|group =="former_ovigerous")
crab_surv<-crab_surv %>% filter(individual != 46)

## creating a survival object

surv<-survfit(Surv(time,death)~group, data=crab_surv)

## fit the full experiment, with time (day of event) and death/census in the Surv object
## and predictors variables after the ~

fullfit=coxph(Surv(time,death)~group, data=crab_surv)
fullfit
#check assumptions
ffassum=cox.zph(fullfit)
ffassum

plot(ffassum)

ggcoxdiagnostics(fullfit, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
ggcoxdiagnostics(fullfit, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#plots
p1<-ggsurvplot(surv, data=crab_surv,fun="cumhaz", conf.int = T, risk.table.col="strata", censor=FALSE)+xlab("Time (days)");p1

p2<-ggsurvplot(surv, data=crab_surv, fun="pct", conf.int = T, risk.table.col="strata")+xlab("Time(days)") ; p2


ggforest(fullfit, data=crab_surv, fontsize = 1.4)

surv

